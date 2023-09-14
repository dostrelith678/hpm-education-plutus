# A Shared Wallet Script

Now, we will write our first typed validator. It will act as a shared wallet between two trusted parties, where unlocking funds is allowed if either of their signatures is present in the spending transaction. For this example, we will define the public key hashes of the two parties in the _**datum**_, and the validator will check that either of those hashes signed the transaction. We do not need to use the redeemer in this example, only the `datum` and `context`.

### Writing the validator

We can start off by creating our datum type, `SharedDatum`, and the corresponding `ValidatorTypes`, `Shared`. `SharedDatum` will have two fields, each corresponding to a `PubKeyHash` (from the `Plutus.V2.Ledger.Api` module) of one of the parties.

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

```haskell
mkUntypedValidator
    :: forall d r
    . (PV1.UnsafeFromData d, PV1.UnsafeFromData r)
    => (d -> r -> sc -> Bool)
    -> UntypedValidator
```

What this means is simply that instead of compiling just the validator function to Plutus core with `$$(PlutusTx.compile [|| mkValidator ||])`, we also need to compile this wrapper. So `PSU.V2.mkTypedValidator` ends up being applied to both of the compiled code instances:

```haskell
typedValidator :: PSU.TypedValidator SharedWalletValidator
typedValidator = PSU.V2.mkTypedValidator @SharedWalletValidator
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PSU.mkUntypedValidator
```

In order for this to work, we also need to enable the `DataKinds` GHC extension.

```haskell
{-# LANGUAGE DataKinds #-}
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

```haskell
Prelude SimplestSuccess> import SharedWallet 
Prelude SharedWallet SimplestSuccess> import Helpers.Utils 
Prelude SharedWallet Helpers.Utils SimplestSuccess> :set -XOverloadedStrings
Prelude SharedWallet Helpers.Utils SimplestSuccess> myDatum = SharedDatum "a5d318dadfb52eeffb260ae097f846aea0ca78e6cc4fe406d4ceedc0" "1b1e5895b03302b248e8c459817bab49471c4013a0806ac52cb73f9b"
Prelude SharedWallet Helpers.Utils SimplestSuccess> writeJSONData "compiled/assets/SharedDatum.json"
```

We used `:set -XOverloadedStrings` to enable the overloaded strings extension inside the REPL in order for it to be able to interpret our strings as `PubKeyHash`es, which is the type our datum requires. We end up with the following in `compiled/assets/SharedDatum.json`:

```json
{"constructor":0,"fields":[{"bytes":"a5d318dadfb52eeffb260ae097f846aea0ca78e6cc4fe406d4ceedc0"},{"bytes":"1b1e5895b03302b248e8c459817bab49471c4013a0806ac52cb73f9b"}]}
```

We now have everything we need to start testing the validator on the testnet.

### Testing the validator

Let's create a script address for this validator in `create-script-address.sh`.

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet

# Build script address
cardano-cli address build \
--payment-script-file ../../compiled/SharedWallet.plutus \
--testnet-magic $NWMAGIC \
--out-file SharedWallet.addr

echo "Script address: $(cat SharedWallet.addr)"
```

We need to update the `check-utxos.sh` for this validator as well.

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

funds_normal=$(cardano-cli query utxo \
--address $(cat ../address/01.addr) \
--testnet-magic $NWMAGIC)

funds_script=$(cardano-cli query utxo \
--address $(cat SharedWallet.addr) \
--testnet-magic $NWMAGIC)

echo "Normal address:"
echo "${funds_normal}"

echo "Script address:"
echo "${funds_script}"
```

After running these two scripts, we see the UTxO status. The script address does not have any UTxOs on it.

```bash
./create-script-address.sh 
Script address: addr_test1wqtul3uvnfqvk7a52fa7r5wcrn6alna6t6pd684jj8mmdvgdxn9r9

./check-utxos.sh 
Normal address:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5dc5111e257f8e68b0978c9619e57bbb12d365c0ec45d879115866bb674156ae     0        1826915 lovelace + TxOutDatumNone
e4a68d9cb4e58d47085c74426441445eaa30c2a60a9d102217d27ec0b0664db8     1        9985492189 lovelace + TxOutDatumNone
ede24e9e40ca82830c75d827b5c3b090132c1afaebd3a4256655fb5d2382474a     0        9649776 lovelace + TxOutDatumNone
ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1     0        1829006 lovelace + TxOutDatumNone

Script address:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

We need to supply the script with some funds in order to test that we can spend it with a valid signature. We already serialised our datum that specifies the two public key hashes that are allowed to spend the funds associated with the UTxO. Let's create a `send-funds-to-script.sh` that will do that for us by sending 20 tADA to the script along with the datum.

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/01.addr) \
    --tx-in e4a68d9cb4e58d47085c74426441445eaa30c2a60a9d102217d27ec0b0664db8#1 \
    --tx-out $(cat SharedWallet.addr)+20000000 \
    --tx-out-datum-embed-file ../../compiled/assets/SharedDatum.json \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file ../address/01.skey \
    --testnet-magic $NWMAGIC \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic $NWMAGIC \
    --tx-file tx.signed
```

Checking the UTxOs on the script shows us the new output with the hash of our datum:

```bash
Script address:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
b980f55aade4508803dcac1c48c28554ee9cf942e40ac1cb5e40e48a621263a5     0        20000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "7857b572c6e6f6fdacffef2bdec1ecc87e32b805c11c75abd59ad9d18e7f438f"
```

We now want to spend this UTxO. Let's try it first without signing the transaction with anything. Create a `spend-script-funds-no-signature.sh`.

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/02.addr) \
    --tx-in b980f55aade4508803dcac1c48c28554ee9cf942e40ac1cb5e40e48a621263a5#0 \
    --tx-in-script-file ../../compiled/SharedWallet.plutus \
    --tx-in-datum-file ../../compiled/assets/SharedDatum.json \
    --tx-in-redeemer-file ../../compiled/assets/unit.json \
    --tx-in-collateral ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1#0 \
    --out-file tx.body

cardano-cli transaction submit \
    --testnet-magic $NWMAGIC \
    --tx-file tx.signed
```

Running this will give us an error:

```bash
./spend-script-funds-no-signature.sh 
Command failed: transaction build  Error: The following scripts have execution failures:
...
Script debugging logs: signature 1 missing
signature 2 missing
```

We see that the `transaction build` command failed before we even get to the point where we sign the transaction with `cardano-cli transaction sign`. This is because the `transaction build` command must already run the validator in question to determine whether the transaction is valid. As our validator checks the transaction signatures with `txSignedBy`, it will certainly fail validation since the transaction is not signed by anything at this build stage. So we need a way to tell the transaction-building command that the transaction _needs to be signed_ by some private key corresponding to a public key hash.

This is where the `--required-signer-hash` option comes in.

```bash
cardano-cli transaction build --help
...
--required-signer-hash HASH
                           Hash of the verification key (zero or more) whose
                           signature is required.
...
```

The `--required-signer-hash` will run the validator simulation _as if the transaction was signed with the private key matching the specified public key hash_. Besides creating the correct simulation for the validator, this option will also make the transaction invalid for submitting _unless_ it really is signed by the correct key. We can check this ourselves by specifying the `--required-signer-hash` to pass validator simulation, but then try submitting the transaction without a signature. Let's update the `cardano-cli transaction build` command in our `spend-script-funds-no-signature.sh`  to include the `--required-signer-hash` option.

```bash
cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/02.addr) \
    --tx-in b980f55aade4508803dcac1c48c28554ee9cf942e40ac1cb5e40e48a621263a5#0 \
    --tx-in-script-file ../../compiled/SharedWallet.plutus \
    --tx-in-datum-file ../../compiled/assets/SharedDatum.json \
    --tx-in-redeemer-file ../../compiled/assets/unit.json \
    --tx-in-collateral ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1#0 \
    --required-signer-hash a5d318dadfb52eeffb260ae097f846aea0ca78e6cc4fe406d4ceedc0 \
    --out-file tx.body
```

We will get through the validator run this time, but the transaction will still fail to be submitted as expected.

```bash
./spend-script-funds-no-signature.sh 
Estimated transaction fee: Lovelace 317891
Command failed: transaction submit  Error: Error while submitting tx: ShelleyTxValidationError ShelleyBasedEraBabbage (ApplyTxError [UtxowFailure (UtxoFailure (AlonzoInBabbageUtxoPredFailure (ValueNotConservedUTxO (MaryValue 0 (MultiAsset (fromList []))) (MaryValue 9985492189 (MultiAsset (fromList [])))))),UtxowFailure (UtxoFailure (AlonzoInBabbageUtxoPredFailure (BadInputsUTxO (fromList [TxIn (TxId {unTxId = SafeHash "e4a68d9cb4e58d47085c74426441445eaa30c2a60a9d102217d27ec0b0664db8"}) (TxIx 1)]))))])
```

That's enough testing invalid transactions for this validator. Finally, let's create a valid transaction that will spend the funds by signing the transaction with our `02.skey` (even though the funds were sent from `01.addr`). Create a `spend-script-funds.sh` script with an updated `--required-signer-hash`, and sign and submit the transaction. The change address will be `02.addr`.

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/02.addr) \
    --tx-in b980f55aade4508803dcac1c48c28554ee9cf942e40ac1cb5e40e48a621263a5#0 \
    --tx-in-script-file ../../compiled/SharedWallet.plutus \
    --tx-in-datum-file ../../compiled/assets/SharedDatum.json \
    --tx-in-redeemer-file ../../compiled/assets/unit.json \
    --tx-in-collateral ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1#0 \
    --required-signer-hash 1b1e5895b03302b248e8c459817bab49471c4013a0806ac52cb73f9b \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file ../address/02.skey \
    --testnet-magic $NWMAGIC \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic $NWMAGIC \
    --tx-file tx.signed
```

Interestingly, the transaction still fails at the submit stage. But notice that the transaction did successfully pass the validator simulation before failing.

```bash
./spend-script-funds.sh 
Estimated transaction fee: Lovelace 317891
Command failed: transaction submit  Error: Error while submitting tx: ShelleyTxValidationError ShelleyBasedEraBabbage (ApplyTxError [UtxowFailure (AlonzoInBabbageUtxowPredFailure (ShelleyInAlonzoUtxowPredFailure (MissingVKeyWitnessesUTXOW (fromList [KeyHash "a5d318dadfb52eeffb260ae097f846aea0ca78e6cc4fe406d4ceedc0"]))))])
```

It says we have a `MissingVKeyWitnessesUTXOW` corresponding to `a5d318dadfb52eeffb260ae097f846aea0ca78e6cc4fe406d4ceedc0`. But that is our `01.addr` public key hash! Why is it complaining about it? Well, remember what we said about collateral inputs, they must always be present with script transactions to cover potential failures. Our collateral input for this transaction is still `ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1#0`, which belongs to `01.addr` so clearly the transaction must be signed by `01.skey` as well in order to allow the usage of this UTxO. Let's add that signature to the `cardano-cli transaction sign` command to allow the transaction to use that collateral input.

```bash
...

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file ../address/01.skey \
    --signing-key-file ../address/02.skey \
    --testnet-magic $NWMAGIC \
    --out-file tx.signed

...
```

Submitting the transaction again works as expected now.

```bash
./spend-script-funds.sh 
Estimated transaction fee: Lovelace 317891
Transaction successfully submitted.
```

If we check our `02.addr` UTxOs, we will see the funds we just spent from the script.

```bash
cardano-cli query utxo \
--address $(cat ../address/02.addr) \
--testnet-magic 2 \
--socket-path $CNODE_HOME/sockets/node0.socket

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
59590fab00fb430d205151c59ca7e00af38e9945d778abdae6897f368aa39591     0        19682109 lovelace + TxOutDatumNone
```
