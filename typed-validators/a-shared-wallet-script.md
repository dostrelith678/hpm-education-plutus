# A Shared Wallet Script

Now, we will write our first typed validator. It will act as a shared wallet between two trusted parties, where unlocking funds is allowed if either of their signatures is present in the spending transaction. For this example, we will define the public key hashes of the two parties in the _**datum**_, and the validator will check that either of those hashes signed the transaction. We do not need to use the redeemer in this example, only the `datum` and `context`.

### Writing the validator

We can start off by creating our datum type, `SharedDatum`, and the corresponding `ValidatorTypes`, `Shared`. `SharedDatum` will have two fields, each corresponding to a `PubKeyHash` of one of the parties.

```haskell
-- create a new datum type
data SharedDatum = SharedDatum {
    wallet1 :: Plutus.PubKeyHash,
    wallet2 :: Plutus.PubKeyHash
  }
PlutusTx.unstableMakeIsData ''SharedDatum

-- create validator types
data SharedWalletValidator
instance Scripts.ValidatorTypes SharedWalletValidator where
  type instance DatumType SharedWalletValidator = SharedDatum
  type instance RedeemerType SharedWalletValidator = ()
```

Make sure that you import the `Scripts` package as:

```haskell
...

import qualified Ledger.Typed.Scripts as Scripts

...
```

Next, we need to write the `mkValidator` logic. As before, we need to destructure the transaction context to get `TxInfo`, and use the [`txSignedBy` ](https://input-output-hk.github.io/plutus-apps/main/plutus-ledger-api/html/Plutus-V2-Ledger-Contexts.html#v:txSignedBy)function from `Plutus.V2.Ledger.Contexts` to check for the signature. `txSignedBy` accepts two arguments, one of type `TxInfo` and the other `PubKeyHash`, returning `True` if the signature is present:

`txSignedBy :: TxInfo -> Plutus.V1.Ledger.Crypto.PubKeyHash -> Bool`

Our `mkValidator` type signature will be `mkValidator :: SharedDatum -> () -> Plutus.ScriptContext -> Bool`, and the main goal is to check for either of the two signatures:

`mkValidator sdat _ ctx = checkSignature1 || checkSignature2`

We then write the helper functions as before, `signature1` and `signature2` for deconstructing our datum type to get each of the corresponding signatures and the `checkSignature1` and `checkSignature2` functions to check each of them with `txSignedBy`. We can also add some error logging with `traceIfFalse`. The full function looks like this:

```haskell
mkValidator :: SharedDatum -> () -> Plutus.ScriptContext -> Bool
mkValidator sdat _ ctx = checkSignature1 || checkSignature2 
  where
    info :: Plutus.TxInfo
    info = Plutus.scriptContextTxInfo ctx

    signature1 :: PubKeyHash
    signature1 = wallet1 sdat

    signature2 :: PubKeyHash
    signature2 = wallet2 sdat

    checkSignature1 :: Bool
    checkSignature1 = traceIfFalse "signature 1 missing" $ txSignedBy info signature1
    
    checkSignature2 :: Bool
    checkSignature2 = traceIfFalse "signature 2 missing" $ txSignedBy info signature2
```

Make sure to import the `txSignedBy` function:

```haskell
...

import Plutus.V2.Ledger.Contexts (txSignedBy)

...
```

Now we have a function of type `SharedDatum -> () -> Plutus.ScriptContext -> Bool`, but remember that we always have to get down to the Untyped Plutus Core version of a validator: `BuiltinData -> BuiltinData -> BuiltinData -> ()`. So with typed validators, we have to do some extra steps to get there. Instead of the simple `Plutus.mkValidatorScript`, we need to use `PSU.V2.mkTypedValidator` (from `Plutus.Script.Utils.V2.Typed.Scripts` which we also need to import). The type signature of `PSU.V2.mkTypedValidator` is:

```haskell
-- | Make a 'TypedValidator' from the 'CompiledCode' of a validator script and its wrapper.
mkTypedValidator ::
    CompiledCode (ValidatorType a)
    -- ^ Validator script (compiled)
    -> CompiledCode (ValidatorType a -> WrappedValidatorType)
    -- ^ A wrapper for the compiled validator
    -> TypedValidator a
```

It accepts a compiled code of a typed validator (with some `ValidatorType a`) and a wrapper that is a function of compiled code `ValidatorType a -> WrappedValidatorType`. The `WrappedValidatorType` is simply a synonym for the basic validator function `BuiltinData -> BuiltinData -> BuiltinData -> ()`. That wrapper function for us is simply `PSU.mkUntypedValidator` which has the type signature:

<pre class="language-haskell"><code class="lang-haskell"><strong>mkUntypedValidator
</strong>    :: forall d r
    . (PV1.UnsafeFromData d, PV1.UnsafeFromData r)
    => (d -> r -> sc -> Bool)
    -> UntypedValidator
</code></pre>

What this means is simply that instead of compiling just the validator function to Plutus core with `$$(PlutusTx.compile [|| mkValidator ||])`, we also need to compile this wrapper. So `PSU.V2.mkTypedValidator` ends up being applied to both of the compiled code instances:

```haskell
typedValidator :: PSU.TypedValidator Shared
typedValidator = PSU.V2.mkTypedValidator @Shared
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PSU.mkUntypedValidator
```

Make sure that the `plutus-script-utils` packages are imported:

```haskell
...

import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.Script.Utils.Typed as PSU

...
```

From there, the rest is the same as before with only a minor difference in getting the actual validator and its hash. When compiling untyped validators, we got a type of `Plutus.Validator` as the compilation result. However, with `TypedValidator` we get a type that has multiple fields so we need to destructure it first to get to `Plutus.Script` type that we can use to serialise the script:

```haskell
validator :: Plutus.Validator  -- uses tvValidator field to get the validator
validator = PSU.V2.validatorScript typedValidator

script :: Plutus.Script -- gets the hash
script = Plutus.unValidatorScript validator
```

Lastly, we just need to add serialise/write file functions for this module.

```haskell
scriptShortBs :: SBS.ShortByteString
scriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

scriptSerialised :: PlutusScript PlutusScriptV2
scriptSerialised = PlutusScriptSerialised scriptShortBs

writeSerialisedScript :: IO (Either (FileError ()) ())
writeSerialisedScript = writeFileTextEnvelope "compiled/SharedWallet.plutus" Nothing scriptSerialised
```

For reference, here is the full list of imports for this module:

```haskell
...

import qualified PlutusTx
import PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as Plutus

import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2, writeFileTextEnvelope)
import Cardano.Api (FileError)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise

import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.Script.Utils.Typed as PSU
import Plutus.V2.Ledger.Contexts (txSignedBy)

import Prelude (IO)

...
```

### Serialising a custom datum type

That's it for the script. We now need to create the correct datum and learn how to construct valid transactions for this use case. First of all, how can we get a `PubKeyHash` of an address? We can use the `cardano-cli address key-hash` command:

```bash
cardano-cli address key-hash \
    --payment-verification-key-file ../address/01.vkey
a5d318dadfb52eeffb260ae097f846aea0ca78e6cc4fe406d4ceedc0

cardano-cli address key-hash \
    --payment-verification-key-file ../address/02.vkey
1b1e5895b03302b248e8c459817bab49471c4013a0806ac52cb73f9b
```

That gets us the `PubKeyHash`es. How can we write them to a valid datum file in JSON format? We can create and import the `SharedDatum` type (we have to export it first) from the `TypedValidator.hs` module and create an instance of it, then serialise and write it to a file. We already have our `Utils` module to write Plutus data to JSON so we can use that. We just need to use some REPL wizardry to do it right. First off, we want to export the `SharedDatum` data type from our `SharedWallet.hs` in order to be able to import it somewhere else. We use the `SharedDatum (..)` syntax to export the type constructor and not just the type.&#x20;

```haskell
module SharedWallet
  (
    scriptSerialised,
    writeSerialisedScript,
    SharedDatum (..)
  )
where
```

Next, update the `exposed-modules` in the `.cabal` file of the project for `SharedWallet`:

```haskell
...
    exposed-modules:      SimplestSuccess
                        , GuessingGame
                        , ExploringScriptContext
                        , SharedWallet -- add SharedWallet here
                        , Helpers.Utils
...
```

&#x20;We are now ready to load up our `cabal repl` and start importing:

<pre class="language-haskell"><code class="lang-haskell">Prelude SimplestSuccess> import SharedWallet 
Prelude SharedWallet SimplestSuccess> import Helpers.Utils 
Prelude SharedWallet Helpers.Utils SimplestSuccess> :set -XOverloadedStrings
<strong>Prelude SharedWallet Helpers.Utils SimplestSuccess> myDatum = SharedDatum "a5d318dadfb52eeffb260ae097f846aea0ca78e6cc4fe406d4ceedc0" "1b1e5895b03302b248e8c459817bab49471c4013a0806ac52cb73f9b"
</strong>Prelude SharedWallet Helpers.Utils SimplestSuccess> writeJSONData "compiled/assets/SharedDatum.json"
</code></pre>

We used `:set -XOverloadedStrings` to enable the overloaded strings extension inside the REPL in order for it to be able to interpret our strings as `PubKeyHash`es, which is the type our datum requires. We end up with the following in `compiled/assets/SharedDatum.json`:

```json
{"constructor":0,"fields":[{"bytes":"a5d318dadfb52eeffb260ae097f846aea0ca78e6cc4fe406d4ceedc0"},{"bytes":"1b1e5895b03302b248e8c459817bab49471c4013a0806ac52cb73f9b"}]}
```

We now have everything we need to start testing the validator on the testnet.

### Testing the validator

1\) `create-script-address.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet

# Build script address
cardano-cli address build \
--payment-script-file TypedValidator.plutus \
--testnet-magic $NWMAGIC \
--out-file TypedValidator.addr

echo "Script address: $(cat TypedValidator.addr)"
```

2\) `check-utxos.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

funds_normal1=$(cardano-cli query utxo \
--address $(cat ../normal_address/01.addr) \
--testnet-magic $NWMAGIC)

funds_normal2=$(cardano-cli query utxo \
--address $(cat ../normal_address/02.addr) \
--testnet-magic $NWMAGIC)

funds_script=$(cardano-cli query utxo \
--address $(cat TypedValidator.addr) \
--testnet-magic $NWMAGIC)

echo "Normal address1:"
echo "${funds_normal1}"
echo ""

echo "Normal address2:"
echo "${funds_normal2}"
echo ""

echo "Script address:"
echo "${funds_script}"
```

3\) `send-funds-to-script.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../normal_address/01.addr) \
    --tx-in 41393cb43ed78ebda383530b1feb13b41b0d4d9ff52510fbf09d3f056863c2ab#1 \
    --tx-out $(cat TypedValidator.addr)+20000000 \
    --tx-out-datum-embed-file ../assets/sharedDatum.json \
    --protocol-params-file ../normal_address/protocol.json \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file ../normal_address/01.skey \
    --testnet-magic $NWMAGIC \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic $NWMAGIC \
    --tx-file tx.signed
```

4\) `spend-script-utxo.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../normal_address/02.addr) \
    --tx-in 892ff638586f24f2413bab71ff49acdef62af2e8d72ad223b67e11d2607e33d6#0 \
    --tx-in-script-file TypedValidator.plutus \
    --tx-in-datum-file ../assets/sharedDatum.json \
    --tx-in-redeemer-file ../assets/unit.json \
    --tx-in-collateral 45337a0fb353dadc7e31f865378885207553b4471814384421e0fa1607271bf6#1 \
    --required-signer-hash 1dbbab8486140e253674dd2af159c322c5c48232b1a358670b1ef5a7 \
    --protocol-params-file ../normal_address/protocol.json \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file ../normal_address/02.skey \
    --signing-key-file ../normal_address/01.skey \
    --testnet-magic $NWMAGIC \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic $NWMAGIC \
    --tx-file tx.signed
```

One important note is that in `spend-script-utxo.sh`, the transaction building command has to use the `--required-signer-hash` specifying one or the other key hashes on the datum of the UTxO we are spending. This is because the `build` command already has to run its validation for the script input, and unless we provide it with the required signer parameter, it cannot correctly check `txSignedBy` logic, as the transaction is not signed by any signature at this stage. It is a way of saying "evaluate this script under the assumption that the transaction _**WILL**_ be signed by this public key hash!".
