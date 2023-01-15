# Typed validators

So far, we have worked only with the low-level untyped version of validator functions which use only the `BuiltinData` type for its arguments. With typed validators, we can define our own types for validation arguments, which makes things simpler. These types still get compiled down to the low-level `BuiltinData` type. For typed validators, we will use the `mkTypedValidator` instead of the `mkValidatorScript` that we have been using so far. `mkTypedValidator` uses the `ToData` typeclass (https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V2-Ledger-Api.html#t:ToData) to convert the given types to `BuiltinData` types.
In Haddock, we can find that the instances for this typeclass are already defined for regular Haskell and Plutus types. In the case of using those basic types, we just need to create a new data typed and make it an instance of `ValidatorTypes` class (https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger/html/Ledger-Typed-Scripts.html#t:ValidatorTypes):
```
data ArbitraryValidatorTypeName
instance Scripts.ValidatorTypes ArbitraryValidatorTypeName where
  type instance DatumType ArbitraryValidatorTypeName = Slot       # Datum type (basic Plutus)
  type instance RedeemerType ArbitraryValidatorTypeName = Integer # Redeemer type (basic Haskell)
```
Following that, we can create a *typed* validator:
```
typedValidator :: PSU.TypedValidator ArbitraryDataTypeName
typedValidator = PSU.V2.mkTypedValidator @ArbitraryDataTypeName
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PSU.mkUntypedValidator
```
Note: PSU is an import
```
import qualified Plutus.Script.Utils.Typed as PSU
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
```
PSU.TypedValidator is version agnostic, but we must use PSU.V2.mkTypedValidator if we want Plutus V2. We also need the `{-# LANGUAGE TypeApplications  #-}` and `{-# LANGUAGE TypeFamilies  #-}` extensions.

If we want to create custom data types for our datum, we have to also create them as instances of the aforementioned `ToData` typeclass. We can simply use the `PlutusTx.unstableMakeIsData` on our defined datum type. The difference between `unstableMakeIsData` and `makeIsDataIndexed` is that the latter ensures consistent indexing that is required to be specified.

```
data ExampleDatum = ExampleDatumConstr {
    field1 :: Field1Type,
    field2 :: Field2Type
  }
PlutusTx.unstableMakeIsData ''ExampleDatum
```
The rest is the same, we just use our data type for the datum/redeemer types:

```
data ArbitraryValidatorTypeName
instance Scripts.ValidatorTypes ArbitraryValidatorTypeName where
  type instance DatumType ArbitraryValidatorTypeName = ExampleDatum # Datum type (custom defined)
  type instance RedeemerType ArbitraryValidatorTypeName = Integer   # Redeemer type (basic Haskell)
```
When using typed validators, the validation function `mkValidator` uses a different type signature than before. We no longer use the primitive `BuiltinData` types for each of the arguments, but can distinguish their types better, such as:
`mkValidator :: DatumType -> RedeemerType -> ScriptContext -> Bool`
Notice also that the final result of the typed version is `Bool` rather than `()`, with `True` representing successful validation.

That is the general overview of how to create typed validators. Now, we will look at an example of a typed validator. It will act as a shared wallet between two trusted parties, where unlocking funds is allowed if either of their signatures is present in the spending transaction. For this example, we will define the public key hashes of the two parties in the *datum*, and the validator will check that either of those hashes signed the transaction. We do not need to use the redeemer in this example, only the `datum` and `context`.

We can start of by creating our datum type, `SharedDatum`, and the corresponding `ValidatorTypes`, `Shared`. `SharedDatum` will have two fields, each corresponding to a `PubKeyHash` of one of the parties.

```
-- create new datum type
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

Next, we need to write the `mkValidator` logic. As before, we need to destructure the transaction context to get `TxInfo`, and use the `txSignedBy` function from `Plutus.V2.Ledger.Contexts` to check for the signature. `txSignedBy` accepts two arguments, one of type `TxInfo` and the other `PubKeyHash`, returning `True` if the signature is present:
`txSignedBy :: TxInfo -> Plutus.V1.Ledger.Crypto.PubKeyHash -> Bool`

Our `mkValidator` type signature will be `mkValidator :: SharedDatum -> () -> Plutus.ScriptContext -> Bool`, and the main goal is to check for either or of the two signatures `mkValidator sdat _ ctx = checkSignature1 || checkSignature2`. We then write the helper functions as before, `signature1` and `signature2` for deconstructing our datum type to get each of the corresponding signatures and the `checkSignature1` and `checkSignature2` functions to check each of them with `txSignedBy`. We can also add some error logging with `traceIfFalse`. The full function looks like this:

```
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
Ok, now we have a function of type `SharedDatum -> () -> Plutus.ScriptContext -> Bool`, but remember that we always have to get down to the Plutus Core version of `BuiltinData -> BuiltinData -> BuiltinData -> ()`. So with typed validators we have to do some extra steps to get there. Instead of the simple `Plutus.mkValidatorScript`, we need to use `PSU.V2.mkTypedValidator` (from `import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2`). The definition of `PSU.V2.mkTypedValidator` is:
```
-- | Make a 'TypedValidator' from the 'CompiledCode' of a validator script and its wrapper.
mkTypedValidator ::
    CompiledCode (ValidatorType a)
    -- ^ Validator script (compiled)
    -> CompiledCode (ValidatorType a -> WrappedValidatorType)
    -- ^ A wrapper for the compiled validator
    -> TypedValidator a
```
It accepts a compiled code of a typed validator (with some `ValidatorType a`) and a wrapper that is a function of compiled code `ValidatorType a -> WrappedValidatorType`. The `WrappedValidatorType` is simply a synonym for the basic validator function `BuiltinData -> BuiltinData -> BuiltinData -> ()`. That wrapper function for us is simply `PSU.mkUntypedValidator` which has the type signature:
```
mkUntypedValidator
    :: forall d r
    . (PV1.UnsafeFromData d, PV1.UnsafeFromData r)
    => (d -> r -> sc -> Bool)
    -> UntypedValidator
```

What this means is simply that instead compiling just the validator function to Plutus core (`$$(PlutusTx.compile [|| mkValidator ||])`) we also need to compile this wrapper. So `PSU.V2.mkTypedValidator` ends up being applied to both of the compiled code instances:

```
typedValidator :: PSU.TypedValidator Shared
typedValidator = PSU.V2.mkTypedValidator @Shared
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PSU.mkUntypedValidator
```

From there, the rest is the same as before with only a minor difference in getting actual validator and its hash. The `TypedValidator` type has multiple fields so the following is pretty much just destructuring to get what we want:

```
validator :: Plutus.Validator  -- uses tvValidator to get the validator
validator = PSU.V2.validatorScript typedValidator

script :: Plutus.Script -- gets the hash
script = Plutus.unValidatorScript validator

-- could use just this on typedValidator:
-- | The hash of the validator.
-- validatorHash :: TypedValidator a -> Scripts.ValidatorHash
-- validatorHash = tvValidatorHash
```

Adjust the serialise/write file functions to correct names.

That's it for the script. We now need to create the correct datum, and learn how to construct valid transactions for this use case. First of all, how do we get a `PubKeyHash` of an address? We can use the `cardano-cli address key-hash` command:
`cardano-cli address key-hash --payment-verification-key-file ../normal_address/01.vkey`
`42f6fcc03996f6af623bd761845a4c3470623e5cdfd72fc20dee990b`
`cardano-cli address key-hash --payment-verification-key-file ../normal_address/02.vkey`
`1dbbab8486140e253674dd2af159c322c5c48232b1a358670b1ef5a7`

Ok, we got the `PubKeyHash`es. How do we write them to a valid datum file? We can create import the `SharedDatum` type (we have to export it first) from the `TypedValidator.hs` module and create an instance of it, then serialise and write it to a file. With our helper functions from before:
```
myDatum1 = SharedDatum {
  wallet1="42f6fcc03996f6af623bd761845a4c3470623e5cdfd72fc20dee990b" :: PubKeyHash,
  wallet2="1dbbab8486140e253674dd2af159c322c5c48232b1a358670b1ef5a7" :: PubKeyHash
}

plutusDataToJSON :: PlutusTx.ToData a => a ->  LBS.ByteString
plutusDataToJSON = encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData

writeJSONData :: String -> IO ()
writeJSONData filePath = LBS.writeFile filePath $ plutusDataToJSON myDatum1
```

in GHCi:

`writeJSONData compiled/assets/sharedDatum.json`

We end up with the following:
```
{"constructor":0,"fields":[{"bytes":"42f6fcc03996f6af623bd761845a4c3470623e5cdfd72fc20dee990b"},{"bytes":"1dbbab8486140e253674dd2af159c322c5c48232b1a358670b1ef5a7"}]}
```

As before we have the helper scripts for testing:
```
create-script-address.sh
check-utxos.sh
send-funds-to-script.sh
spend-script-utxo.sh
```

One important note is that in `spend-script-utxo.sh`, the transaction building command has to use the `--required-signer-hash` specifying one or the other key hashes on the datum of the UTxO we are spending. This is because the `build` command already has to run its validation for the script input, and unless we provide it with the required signer parameter, it cannot correctly check `txSignedBy` logic, as the transaction is not signed by any signature at this stage. It is a way of saying "evaluate this script under the assumption that the transaction WILL be signed by this public key hash!".



-- this corresponds to `PubKeyHash` (https://github.com/input-output-hk/plutus-apps/issues/16#issuecomment-933635128)
Note: OFc we need `--required-signer-hash` this because evaluation already happens here...