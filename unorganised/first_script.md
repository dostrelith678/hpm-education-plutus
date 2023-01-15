# Writing our first Plutus script

Creating and executing Plutus scripts can be summarised in a couple of steps:

 - Write your Plutus on-chain code.
 - Serialize your Plutus on chain code to the text envelope format (cardano-cli expects this format).
 - Create your transaction with the accompanying Plutus script(s). This should include a datum hash (any script UTxO without datum is UNSPENDABLE).
 - Submit transaction to execute Plutus script. This needs to include the collateral input to cover costs in case the Plutus script fails.


As mentioned in the EUTXO overview part, the validator script receives three arguments:
 1) datum
 2) redeemer
 3) context


The haddock documentation for Plutus specifies the main modules:
```
PlutusTx: Compiling Haskell to PLC (Plutus Core; on-chain code).
PlutusTx.Prelude: Haskell prelude replacement compatible with PLC.
Plutus.Contract: Writing Plutus apps (off-chain code).
Ledger.Constraints: Constructing and validating Plutus transactions. Built on PlutusTx and Plutus.Contract.
Ledger.Typed.Scripts: A type-safe interface for spending and producing script outputs. Built on PlutusTx.
Plutus.Trace.Emulator: Testing Plutus contracts in the emulator.
```
These are the modules that we will be importing in our Haskell files. We start with a new `SimplestSuccess.hs` file. We will write the simplest contract that successfully validates every attempt to spend its funds. First, we add some GHC extensions at the start of the file:
```
{-# LANGUAGE DataKinds         #-} -- make any type constructor into a type
{-# LANGUAGE NoImplicitPrelude #-} -- don't import Prelude by default
{-# LANGUAGE TemplateHaskell   #-} -- allows embedding domain specific language into the Haskell host language
```
Next, we define our module name (same as filename):
```
module SimplestSuccess
  (
    successScriptShortBs,
    successScript
  ) 
where
```

Next, we need to import packages required for the compilation of our script. Note that these must be defined in our `.cabal` file in order to be imported here. For now these will be:

```
import PlutusTx qualified -- main on-chain code module
import PlutusTx.Prelude qualified -- Prelude replacement for Plutus
import Plutus.V1.Ledger.Scripts qualified as Plutus -- functions for working with scripts on the ledger

-- this is not from Plutus but cardano-node https://input-output-hk.github.io/cardano-node/cardano-api/lib/Cardano-Api-Shelley.html
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV1)

-- Hackage packages
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Codec.Serialise
```

In general we want to write three major parts of our Haskell file:
1) The `mkValidator` function
2) Compilation of that function to a Plutus Core script (the on-chain language). This is done by using template Haskell.
3) Serialise the script and write the script to a `.plutus` file.
   
So when we write a validator we define a function that receives three arguments and returns a unit (`()`) if successful. Not returning a `()` means that the validation failed and the transaction will be invalidated. With that in mind, we can start to think about the type signature of the validator function, something like:
`mkValidator :: Datum -> Redeemer -> Context -> ()`.

But what are the types of `Datum`, `Redeemer` and `Context`? It turns out that in Plutus, all the three of the validation arguments need to come in a type of `Data`. We can explore the `haddock` pages to learn more about it: https://playground.plutus.iohkdev.io/doc/haddock/plutus-core/html/PlutusCore-Data.html. We see that the `Data` type comes with several constructors, but the main takeaway is that it is a generic data type that can represent various things such as integers, bytestrings, lists and maps. Plutus also features a `BuiltinData` type (https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Builtins.html#g:4) that can be used directly in the on-chain code.

So we can now write the type signature of our validator function using the `BuiltinData` type for its arguments and returning `()`:
`mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()`

```
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()
```
Since this function always returns `()` regardless of its arguments, any UTxO belonging to the script will be spendable by any transaction.

We now need to do part 2) of our three steps, compiling this validator function to Plutus Core:

```
-- compile mkValidator (template Haskell)
validator :: Plutus.Validator
validator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

The unusual syntax above is template Haskell: `$$([|| ||])`. The function `Plutus.mkValidatorScript` requires a Plutus Core argument so the `mkValidator` is first compiled to Plutus Core.

Part 3) of our three steps is arguably the simplest. We need to unwrap the validator to get the script. This is just a necessary step to conform with expected types:

```
-- serialise compiled validator
-- Validator is a wrapper around Scripts which are used as validators in transaction outputs. So we need to unwrap it.
script :: Plutus.Script
script = Plutus.unValidatorScript validator
```

We can now serialise the script to a `ShortByteString`:
```
successScriptShortBs :: SBS.ShortByteString
successScriptShortBs = SBS.toShort Prelude.. LBS.toStrict Prelude.$ serialise script
```

Next step is just a type conversion again:

```
successScriptSerialised :: PlutusScript PlutusScriptV1
successScriptSerialised = PlutusScriptSerialised successScriptShortBs
```

Finally, we expose a function that writes the `Plutus` script to a file that we will use with the actual blockchain:

```
writeSerialisedSuccessScript :: IO (Prelude.Either (FileError ()) ())
writeSerialisedSuccessScript = writeFileTextEnvelope "compiled/simplestSuccess.plutus" Prelude.Nothing successScriptSerialised
```

Load it up in `cabal repl` and compile the script:
```
Prelude SimplestSuccessV2> SimplestSuccessV2.writeSerialisedSuccessScript 
Right ()
```

We have the script now. Another thing we need is to serialise a datum. We need to use datums on script outputs as any UTxO without a datum hash attached will be unspendable as we mentioned before. We write a utility function for converting Plutus data to JSON because `cardano-cli` expects JSON values:
```
# Serialising Data types

plutusDataToJSON :: PlutusTx.ToData a => a ->  LBS.ByteString
plutusDataToJSON = encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData

# To write data to file:

writeJSONData :: PlutusTx.ToData a => String -> a -> IO ()
writeJSONData filePath pData = LBS.writeFile filePath $ plutusDataToJSON pData
```

We can use this function to write a unit `()` datum file. Make sure to remove unused imports and `scriptDataToJson`, `ScriptDataJsonDetailedSchema`, `fromPlutusData` are from `cardano-api`.

```
Prelude Utils> writeJSONData "compiled/assets/unit.json" ()
```

To use the script, I have made bash scripts:
```
create-addresses.sh
check-utxos.sh
spend-script-utxo.sh
```

homework: write one that always fails

Plutus V2:
  - reference inputs (look at datum without attempting to spend the utxo)
  - reference scripts (no need to upload plutus script on each transaction, you instead upload it as an inline datum and reference it)
  - inline datums (able to store simple datum on blockchain)



