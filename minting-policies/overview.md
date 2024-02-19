# Overview

Minting policies are very similar to `StakeValidator`s in the sense that they also do not receive the `datum` argument for validation, only the `redeemer` and `context`. This type of policy is used for the `Minting CurrencySymbol` script purpose. It returns a `()` representing a valid minting or burning transaction or throws an error if validation fails. But when using typed minting policy scripts, the function should return a boolean value.

As introduced in the [Introduction to the EUTxO model section](../the-eutxo-model/introduction-to-the-eutxo-model/the-eutxo-model-extended-utxo-model.md#custom-native-tokens-in-the-eutxo-model), tokens on Cardano are made up of the [`CurrencySymbol`](https://intersectmbo.github.io/plutus/master/plutus-ledger-api/html/PlutusLedgerApi-V2.html#t:CurrencySymbol) and the token name. One `CurrencySymbol` can hold an infinite number of [`TokenName`](https://intersectmbo.github.io/plutus/master/plutus-ledger-api/html/PlutusLedgerApi-V2.html#t:TokenName)s. Token names are generally hexified ByteStrings but have an ASCII representation as well. `CurrencySymbol` is the hash of the minting policy that controls token minting/burning.

Before Plutus, these policies were introduced in the Mary era and simple timelocking minting policies were already possible. Now with Plutus, we can control the minting policy with arbitrary logic instead of just key witnesses and timelocks. So `CurrencySymbol` is the hash of the minting policy, whether it's a Plutus policy script or a Mary-era style policy (`CurrencySymbol` is also referred to as the `policyID`).

To create minting policies with Plutus, we use the [`mkMintingPolicyScript`](https://intersectmbo.github.io/plutus-apps/main/plutus-ledger/html/Ledger.html#v:mkMintingPolicyScript) function, which accepts a function of `BuiltinData -> BuiltinData -> ()` of `CompiledCode` and returns a [`MintingPolicy`](https://intersectmbo.github.io/plutus/master/plutus-ledger-api/html/PlutusLedgerApi-V2.html#t:TokenName) - a wrapper around the [`Script`](https://intersectmbo.github.io/plutus-apps/main/plutus-ledger/html/Ledger.html#t:Script) type. The function of type `BuiltinData -> BuiltinData -> ()` represents the _untyped_ version of the minting policy created with the [mkUntypedMintingPolicy](https://intersectmbo.github.io/plutus-apps/main/plutus-ledger/html/Ledger-Typed-Scripts.html#v:mkUntypedMintingPolicy) method.

```haskell
mkMintingPolicyScript :: 
    CompiledCode (BuiltinData -> BuiltinData -> ())
    -> MintingPolicy

mkUntypedMintingPolicy :: 
    UnsafeFromData r =>
        (r -> sc -> Bool) -> UntypedMintingPolicy
```

In the next section, we will write a pay-to-store minting policy script.

###
