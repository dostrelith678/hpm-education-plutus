# Overview

So far, we have been using the `datum`, `redeemer` and `context` fields to express the validation logic of our scripts. However, there is a very valuable method of creating validators that do not rely only on those three fields. Recall our [shared wallet validator](../typed-validators/a-shared-wallet-script.md) where we had to put the public key hashes inside the datum to sit at the script UTxO. Imagine that instead of those hashes being specified in the datum, and then having the script look at the datum, we could instead _bake_ _in_ those public key hashes inside the script itself. In that case, the script would no longer need to look at the datum to determine whether a transaction is valid or not. Instead, it would already have that information inside the script itself. These types of validators are referred to as **parameterised validators**.

It is important to note that, as always, validator scripts are compiled down to the form of `BuiltinData -> BuiltinData -> BuiltinData -> ()`, even with parameterised validators. However, in this case, we can write our logic in a different type signature that allows us to write the validators in a more high-level way. The type signature of the `mkValidator` function in parameterised validators is:

`ValidatorParam(s) -> DatumType -> RedeemerType -> ScriptContext -> Bool`.

The new argument `ValidatorParam` allows us to bake in any data we want into the script itself. We can define it with as many fields as we want, but as before, unless we use types that are already instances of `ToData` typeclass, we have to instantiate them and additionally, with parameter types, we have to **lift** them. Lifting values or types means compiling the Haskell code to Plutus Core at runtime. The result of a lifted `Integer` for example is `CompiledCode Integer`.&#x20;

As with [typed validators](broken-reference), we can still define our own `datum` and `redeemer` types. And we have to create the `ValidatorTypes` instance to use with our validator as before.
