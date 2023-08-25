# Overview

Minting policies are very similar to `StakeValidator`s in the sense that they also do not receive the `datum` argument for validation, only the `redeemer` and `context`. This type of policy is used for the `Minting CurrencySymbol` script purpose. It returns a `()` representing a valid mint or throws an error if validation fails. But when using typed minting policy scripts, the function should return a boolean value.

Tokens on Cardano are made up of the `CurrencySymbol` and the token name. One `CurrencySymbol` can hold an infinite number of `TokenName`s. Token names are generally hexlified bytestrings but have an ASCII representation as well. The `CurrencySymbol` is the hash of the minting policy that controls token minting/burning.

Before Plutus, in the Mary era, these policies were introduced and simple timelocking scripts were already possible. Now with Plutus, we can control the minting policy with arbitrary logic instead of just key witnesses and timelocks. So the `CurrencySymbol` is the hash of the minting policy, whether it's a Plutus policy script or a Mary-era style policy (`CurrencySymbol` is also referred to as the `policyID`).

The [`mkMintingPolicyScript`](https://input-output-hk.github.io/plutus-apps/main/plutus-ledger/html/Ledger.html#v:mkMintingPolicyScript) accepts a function of `BuiltinData -> BuiltinData -> ()` of `CompiledCode` and returns a `MintingPolicy` - a wrapper around the `Script` type:

```haskell
mkMintingPolicyScript :: 
    CompiledCode (BuiltinData -> BuiltinData -> ())
    -> MintingPolicy
```

In the next section, we will write a pay-to-store minting policy script.

###
