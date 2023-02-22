# Shared Wallet Example

Now, we will look at an example of a typed validator. It will act as a shared wallet between two trusted parties, where unlocking funds is allowed if either of their signatures is present in the spending transaction. For this example, we will define the public key hashes of the two parties in the _**datum**_, and the validator will check that either of those hashes signed the transaction. We do not need to use the redeemer in this example, only the `datum` and `context`.

We can start off by creating our datum type, `SharedDatum`, and the corresponding `ValidatorTypes`, `Shared`. `SharedDatum` will have two fields, each corresponding to a `PubKeyHash` of one of the parties.

```haskell
-- create a new datum type
data SharedDatum = SharedDatum {
    wallet1 :: PubKeyHash,
    wallet2 :: PubKeyHash
  }
PlutusTx.unstableMakeIsData ''SharedDatum

-- create validator types
data Shared
instance Scripts.ValidatorTypes Shared where
  type instance DatumType Shared = SharedDatum
  type instance RedeemerType Shared = ()
```

Next, we need to write the `mkValidator` logic. As before, we need to destructure the transaction context to get `TxInfo`, and use the [`txSignedBy` ](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V2-Ledger-Contexts.html#v:txSignedBy)function from `Plutus.V2.Ledger.Contexts` to check for the signature. `txSignedBy` accepts two arguments, one of type `TxInfo` and the other `PubKeyHash`, returning `True` if the signature is present:

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

Now we have a function of type `SharedDatum -> () -> Plutus.ScriptContext -> Bool`, but remember that we always have to get down to the Plutus Core version of a validator: `BuiltinData -> BuiltinData -> BuiltinData -> ()`. So with typed validators, we have to do some extra steps to get there. Instead of the simple `Plutus.mkValidatorScript`, we need to use `PSU.V2.mkTypedValidator` (from `import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2`). The definition of `PSU.V2.mkTypedValidator` is:

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

What this means is simply that instead of compiling just the validator function to Plutus core (`$$(PlutusTx.compile [|| mkValidator ||])`) we also need to compile this wrapper. So `PSU.V2.mkTypedValidator` ends up being applied to both of the compiled code instances:

```haskell
typedValidator :: PSU.TypedValidator Shared
typedValidator = PSU.V2.mkTypedValidator @Shared
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PSU.mkUntypedValidator
```

From there, the rest is the same as before with only a minor difference in getting the actual validator and its hash. The `TypedValidator` type has multiple fields so the following is pretty much just destructuring to get what we want:

```haskell
validator :: Plutus.Validator  -- uses tvValidator to get the validator
validator = PSU.V2.validatorScript typedValidator

script :: Plutus.Script -- gets the hash
script = Plutus.unValidatorScript validator

-- could just use this single function instead on typedValidator:
-- | The hash of the validator.
-- validatorHash :: TypedValidator a -> Scripts.ValidatorHash
-- validatorHash = tvValidatorHash
```

Lastly, we just need to adjust the serialise/write file functions to correct names for this module.

That's it for the script. We now need to create the correct datum and learn how to construct valid transactions for this use case. First of all, how do we get a `PubKeyHash` of an address? We can use the `cardano-cli address key-hash` command:

```bash
cardano-cli address key-hash \
    --payment-verification-key-file ../normal_address/01.vkey
42f6fcc03996f6af623bd761845a4c3470623e5cdfd72fc20dee990b

cardano-cli address key-hash \
    --payment-verification-key-file ../normal_address/02.vkey
1dbbab8486140e253674dd2af159c322c5c48232b1a358670b1ef5a7
```

That gets us the `PubKeyHash`es. How do we write them to a valid datum file? We can create and import the `SharedDatum` type (we have to export it first) from the `TypedValidator.hs` module and create an instance of it, then serialise and write it to a file. With our helper functions from before we can create a new module `WriteSharedDatum.hs`:

```haskell
{-# LANGUAGE DataKinds         #-} -- make any type constructor into a type
{-# LANGUAGE NoImplicitPrelude #-} -- don't import Prelude by default
{-# LANGUAGE OverloadedStrings #-} -- allows embedding domain specific language into the Haskell host language

module WriteSharedDatum

where

import qualified PlutusTx
import PlutusTx.Prelude
import Data.Aeson (encode)
import TypedValidator (SharedDatum (..))
import Ledger (PubKeyHash (..), PaymentPubKeyHash (..))

import Cardano.Api.Shelley (fromPlutusData, scriptDataToJson, ScriptDataJsonSchema (ScriptDataJsonDetailedSchema))

import qualified Data.ByteString.Lazy as LBS

import Prelude (IO, String)

myDatum1 = SharedDatum {
  wallet1="42f6fcc03996f6af623bd761845a4c3470623e5cdfd72fc20dee990b" :: PubKeyHash,
  wallet2="1dbbab8486140e253674dd2af159c322c5c48232b1a358670b1ef5a7" :: PubKeyHash
}

plutusDataToJSON :: PlutusTx.ToData a => a ->  LBS.ByteString
plutusDataToJSON = encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData

writeJSONData :: PlutusTx.ToData a => String -> IO ()
writeJSONData filePath = LBS.writeFile filePath $ plutusDataToJSON myDatum1
```

Load the module in the `cabal repl` and execute the function:

`ghci> writeJSONData compiled/assets/sharedDatum.json`

We end up with the following in `compiled/assets/sharedDatum.json`:

```
{"constructor":0,"fields":[{"bytes":"42f6fcc03996f6af623bd761845a4c3470623e5cdfd72fc20dee990b"},{"bytes":"1dbbab8486140e253674dd2af159c322c5c48232b1a358670b1ef5a7"}]}
```

As before we have the helper scripts for testing:

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
