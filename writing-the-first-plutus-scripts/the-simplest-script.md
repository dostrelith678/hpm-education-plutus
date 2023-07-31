# The Simplest Script

As mentioned in the [EUTxO overview](../the-eutxo-model/introduction-to-the-eutxo-model/the-eutxo-model-extended-utxo-model.md), the validator script receives three arguments:

1. datum
2. redeemer
3. context

The haddock documentation for Plutus specifies the main modules ([https://input-output-hk.github.io/plutus/master/](https://input-output-hk.github.io/plutus/master/)):

```
PlutusTx: Compiling Haskell to PLC (Plutus Core; on-chain code).

PlutusTx.Prelude: Haskell prelude replacement compatible with PLC.

PlutusCore: Programming language in which scripts on the Cardano blockchain are written.

UntypedPlutusCore: On-chain Plutus code.
```

The two modules that we will be importing into our Haskell files are `PlutusTx` and `PlutusTx.Prelude`. We start with a new `SimplestSuccess.hs` file. We will **write the simplest contract that successfully validates every attempt to spend its funds**. First, we add some GHC extensions at the start of the file:

```haskell
{-# LANGUAGE DataKinds         #-} -- make any type constructor into a type
{-# LANGUAGE NoImplicitPrelude #-} -- do not import Prelude by default
{-# LANGUAGE TemplateHaskell   #-} -- allows embedding domain-specific language into the Haskell host language
```

Next, we define our module name (same as the filename):

```haskell
module SimplestSuccess
  (
    successScriptSerialised,
    writeSerialisedSuccessScript
  ) 
where
```

Next, we need to import the packages required for the compilation of our script. Note that these must be defined in our `.cabal` file in order to be imported here. For now, these will be:

```haskell
import qualified PlutusTx                           -- main on-chain code module
import qualified PlutusTx.Prelude                   -- Prelude replacement for Plutus
import qualified Plutus.V2.Ledger.Api as Plutus     -- functions for working with scripts on the ledger

-- this is not from Plutus but cardano-node https://input-output-hk.github.io/cardano-node/cardano-api/lib/Cardano-Api-Shelley.html
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2, writeFileTextEnvelope)
import Cardano.Api (FileError)

-- Hackage packages
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise

import Prelude (IO)
```

In general, we want to **write three major parts** of our Haskell file:

1. The `mkValidator` function that contains our validation logic.
2. Compilation of that function to a Plutus Core script (the on-chain language). This is done by using template Haskell.
3. Serialise the script and write the script to a `.plutus` file.

When we write a validator we define a function that receives the three aforementioned arguments and **returns a unit (`()`) if successful**. Not returning a `()` means that the validation failed and the transaction will be invalidated. With that in mind, we can start to think about the type signature of the validator function, something like:

`mkValidator :: Datum -> Redeemer -> Context -> ()`.

But what are the types of `Datum`, `Redeemer` and `Context`? It turns out that in Plutus, all three of the validation arguments need to come in a type of `Data`. We can explore the `haddock` pages to learn more about it: [https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore-Data.html](https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore-Data.html). We see that the `Data` type comes with several constructors, but the main takeaway is that it is a generic data type that can represent various things such as integers, byte strings, lists, and maps. Plutus also features a `BuiltinData` type ([https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#g:4](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#g:4)) that can be used directly in the on-chain code.

So we can now write the type signature of our validator function using the `BuiltinData` type for its arguments and returning `()`: `mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()`

```
mkValidator :: Prelude.BuiltinData -> Prelude.BuiltinData -> Prelude.BuiltinData -> ()
mkValidator _ _ _ = ()
```

Since this function always returns `()` regardless of its arguments, any UTxO belonging to the script will be spendable by any transaction.

We now need to do part 2 of our three steps, compiling this validator function to Plutus Core:

```
validator :: Plutus.Validator
validator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

The unusual syntax above is template Haskell: `$$([|| ||])`. The function `Plutus.mkValidatorScript` requires a Plutus Core argument so the `mkValidator` is first compiled to Plutus Core.

Part 3 of our three steps is arguably the simplest. We need to unwrap the validator to get the script. This is just a necessary step to conform with the expected types. Since `Plutus.Validator` is a wrapper around `Plutus.script` which is used as the actual validator in the ledger, we need to unwrap it.

```
script :: Plutus.Script
script = Plutus.unValidatorScript validator
```

We can now serialise the script to a `ShortByteString`:

```
successScriptShortBs :: SBS.ShortByteString
successScriptShortBs = SBS.toShort Prelude.. LBS.toStrict Prelude.$ serialise script
```

The next step is just a type conversion again:

```
successScriptSerialised :: PlutusScript PlutusScriptV2
successScriptSerialised = PlutusScriptSerialised successScriptShortBs
```

Finally, we expose a function that writes the `Plutus` script to a file that we will use with the actual blockchain:

```
writeSerialisedSuccessScript :: IO (Prelude.Either (FileError ()) ())
writeSerialisedSuccessScript = writeFileTextEnvelope "compiled/simplestSuccess.plutus" Prelude.Nothing successScriptSerialised
```

Load it up in `cabal repl` and compile the script:

```
Prelude SimplestSuccess> SimplestSuccess.writeSerialisedSuccessScript 
Right ()
```

We now have the compiled script in `compiled/simplestSuccess.plutus`. Another thing we need is to serialise a `datum`. We need to use datums on script outputs as any UTxO without a datum hash attached will be unspendable as we mentioned before. We need to write a utility function for converting Plutus data to JSON because `cardano-cli` expects JSON values. Create a new file under `src/helpers/Utils.hs`:

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils
  (
    plutusDataToJSON,
    writeJSONData
  ) 
where

import qualified PlutusTx
import PlutusTx.Prelude
import Data.Aeson (encode)

import Cardano.Api.Shelley (fromPlutusData, scriptDataToJson, ScriptDataJsonSchema (ScriptDataJsonDetailedSchema))

import qualified Data.ByteString.Lazy as LBS

import Prelude (IO, String)

plutusDataToJSON :: PlutusTx.ToData a => a ->  LBS.ByteString
plutusDataToJSON = encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData

writeJSONData :: PlutusTx.ToData a => String -> a -> IO ()
writeJSONData filePath pData = LBS.writeFile filePath $ plutusDataToJSON pData
```

We can now load and use this function to write a unit `()` datum file.

```
Prelude Utils> writeJSONData "compiled/assets/unit.json" ()
```

To use the script, we can make some `bash` scripts (it is also possible to utilise `cardano-cli` directly):

1\) `create-addresses.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet

# Build normal address 1
cardano-cli address key-gen \
--verification-key-file ../normal_address/01.vkey \
--signing-key-file ../normal_address/01.skey

cardano-cli address build \
--payment-verification-key-file ../normal_address/01.vkey \
--testnet-magic $NWMAGIC \
--out-file ../normal_address/01.addr

# Build normal address 2
cardano-cli address key-gen \
--verification-key-file ../normal_address/02.vkey \
--signing-key-file ../normal_address/02.skey

cardano-cli address build \
--payment-verification-key-file ../normal_address/02.vkey \
--testnet-magic $NWMAGIC \
--out-file ../normal_address/02.addr

# Build script address
cardano-cli address build \
--payment-script-file simplestSuccess.plutus \
--testnet-magic $NWMAGIC \
--out-file simplestSuccess.addr

echo "Before continuing, request faucet funds to address: $(cat 01.addr)!"
```

2\) `check-utxos.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

funds_normal=$(cardano-cli query utxo \
--address $(cat 01.addr) \
--testnet-magic $NWMAGIC)

funds_script=$(cardano-cli query utxo \
--address $(cat simplestSuccess.addr) \
--testnet-magic $NWMAGIC)

echo "Normal address:"
echo "${funds_normal}"

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
    --tx-in 29e96c103e6d26d9b8a110df9c8f82eaacbc53077d0b474b41fb4c0d0c0fca93#0 \
    --tx-out $(cat simplestSuccess.addr)+2000000 \
    --tx-out-datum-embed-file ../assets/unit.json \
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
    --change-address $(cat 01.addr) \
    --tx-in a34a0e1b93e258f3124614f990ce6751c3284963e840e2104e875fe39582638c#0 \
    --tx-in-script-file simplestSuccess.plutus \
    --tx-in-datum-file ../assets/unit.json \
    --tx-in-redeemer-file ../assets/unit.json \
    --tx-in-collateral 61931f9f949bf8d1ef1e8b6b004fb7ea4cd80db3319247e2355ed08399af94f4#0 \
    --protocol-params-file protocol.json \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 01.skey \
    --testnet-magic $NWMAGIC \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic $NWMAGIC \
    --tx-file tx.signed
```

It is important to practice on your own. Try using the materials above to write and test a Plutus script that always fails (a sort of token-burning script) from scratch.
