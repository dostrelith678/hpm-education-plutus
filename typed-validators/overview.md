# Overview

So far, we have worked only with the low-level untyped version of validator functions which use only the `BuiltinData` type for its arguments. With typed validators, we can define our own types for validation arguments, which makes things simpler. These types still get compiled down to the low-level `BuiltinData` type.

For typed validators, we will use the `mkTypedValidator` instead of the `mkValidatorScript` that we have been using so far. `mkTypedValidator` uses the `ToData` typeclass ([https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V2-Ledger-Api.html#t:ToData](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V2-Ledger-Api.html#t:ToData)) to convert the given types to `BuiltinData` types. In Haddock, we can find that the instances for this typeclass are already defined for regular Haskell and Plutus types. In the case of using those basic types, we just need to create a new data type and make it an instance of `ValidatorTypes` class ([https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger/html/Ledger-Typed-Scripts.html#t:ValidatorTypes](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger/html/Ledger-Typed-Scripts.html#t:ValidatorTypes)):

```haskell
data ArbitraryValidatorTypeName
instance Scripts.ValidatorTypes ArbitraryValidatorTypeName where
  type instance DatumType ArbitraryValidatorTypeName = Slot       -- Datum type (basic Plutus)
  type instance RedeemerType ArbitraryValidatorTypeName = Integer -- Redeemer type (basic Haskell)
```

Following that, we can create a _**typed**_** ** validator with `Plutus.Script.Utils`:

```haskell
import qualified Plutus.Script.Utils.Typed as PSU
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2

typedValidator :: PSU.TypedValidator ArbitraryValidatorTypeName
typedValidator = PSU.V2.mkTypedValidator @ArbitraryValidatorTypeName
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PSU.mkUntypedValidator
```

`PSU.TypedValidator` is version agnostic, but we must use `PSU.V2.mkTypedValidator` if we want Plutus V2. We also need the `{-# LANGUAGE TypeApplications #-}` and `{-# LANGUAGE TypeFamilies #-}` extensions for the above syntax to work properly.

### Creating Custom Data Types

If we want to create custom data types for our datum or redeemer, we must also create them as instances of the aforementioned `ToData` typeclass. We should simply use the `PlutusTx.unstableMakeIsData` on our defined datum/redeemer type. The difference between `unstableMakeIsData` and `makeIsDataIndexed` is that the latter ensures consistent indexing that is required to be specified and it is recommended to use `makeIsDataIndexed` in production because it ensures the same data structure results across different Plutus versions.

```haskell
data ExampleDatum = ExampleDatumConstr {
    field1 :: Field1Type,
    field2 :: Field2Type
  }
PlutusTx.unstableMakeIsData ''ExampleDatum
```

The rest is the same, we just use our data type for the datum/redeemer types:

```haskell
data ArbitraryValidatorTypeName
instance Scripts.ValidatorTypes ArbitraryValidatorTypeName where
  type instance DatumType ArbitraryValidatorTypeName = ExampleDatum -- Datum type (custom defined)
  type instance RedeemerType ArbitraryValidatorTypeName = Integer   -- Redeemer type (basic Haskell)
```

When using typed validators, the validation function `mkValidator` uses a different type signature than before. We no longer use the primitive `BuiltinData` types for each of the arguments, but can distinguish their types better, with the general form of:

`mkValidator :: DatumType -> RedeemerType -> ScriptContext -> Bool`

Notice also that the final result of the typed version is `Bool` rather than `()`, with `True` representing successful validation.

