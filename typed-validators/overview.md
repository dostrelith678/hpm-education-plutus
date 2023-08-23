# Overview

So far, we have worked only with the low-level untyped version of validator functions which use the `BuiltinData` type for its arguments. With typed validators, we can define our own types for validation arguments, simplifying things. These types still get compiled down to the low-level `BuiltinData` type, but our high-level code can be more understandable.

For typed validators, we will use the `mkTypedValidator` instead of the `mkValidatorScript` that we have been using so far. `mkTypedValidator` uses the `ToData` typeclass ([https://input-output-hk.github.io/plutus-apps/main/plutus-ledger-api/html/Plutus-V2-Ledger-Api.html#t:ToData](https://input-output-hk.github.io/plutus-apps/main/plutus-ledger-api/html/Plutus-V2-Ledger-Api.html#t:ToData)) to convert the given types to `BuiltinData` types. In Haddock, we can find that the instances for this typeclass are already defined for regular Haskell and Plutus types. In the case of using those basic types, we just need to create a new data type and make it an instance of the `ValidatorTypes` class ([https://input-output-hk.github.io/plutus-apps/main/plutus-ledger/html/Ledger-Typed-Scripts.html#t:ValidatorTypes](https://input-output-hk.github.io/plutus-apps/main/plutus-ledger/html/Ledger-Typed-Scripts.html#t:ValidatorTypes)). The basic syntax for creating a validator type is as follows:

```haskell
data ArbitraryValidatorTypeName

instance Scripts.ValidatorTypes ArbitraryValidatorTypeName where
  type instance DatumType ArbitraryValidatorTypeName = Slot       -- Datum type (basic Plutus)
  type instance RedeemerType ArbitraryValidatorTypeName = Integer -- Redeemer type (basic Haskell)
```

With a typed validator, we use our defined types for `Datum` and `Redeemer` instead of the `BuiltinData` so the type signature of the `mkValidator` changes. Besides using our defined types for `Datum` and `Redeemer`, it also now returns a `Bool` instead of a `()`, with `True` signifying successful validation and `False` signifying a failed one.

```haskell
mkValidator :: Slot -> Integer -> Plutus.ScriptContext -> Bool
mkValidator = ...
```

Following that, we can create a _**typed**_ validator with the validator we defined above using the  `Plutus.Script.Utils` package:

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
...

import qualified Plutus.Script.Utils.Typed as PSU
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2

typedValidator :: PSU.TypedValidator ArbitraryValidatorTypeName
typedValidator = PSU.V2.mkTypedValidator @ArbitraryValidatorTypeName
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PSU.mkUntypedValidator
```

`PSU.TypedValidator` is version agnostic, but we must use `PSU.V2.mkTypedValidator` if we want to compile to Plutus V2. We also need the `{-# LANGUAGE TypeApplications #-}` and `{-# LANGUAGE TypeFamilies #-}` extensions for the above syntax to work properly.

### Creating Custom Data Types

If we want to create custom data types for our datum or redeemer, we must also create them as instances of the aforementioned `ToData` typeclass. We should simply use the [`PlutusTx.unstableMakeIsData`](https://input-output-hk.github.io/plutus-apps/main/plutus-tx/html/PlutusTx.html#v:unstableMakeIsData) on our defined datum/redeemer type. The difference between `unstableMakeIsData` and `makeIsDataIndexed` is that the latter ensures consistent indexing that is required to be specified and it is **recommended to use `makeIsDataIndexed` in production** because it ensures the same data structure results across different Plutus versions.

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

