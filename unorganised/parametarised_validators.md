# Parameterised validators

So far, we have been using the `datum`, `redeemer` and `context` fields to express the validation logic of our scripts. However, there is a very valuable method of creating validators that doesn't rely only on those three fields. Recall our shared wallet validator where we had to put the public key hashes inside the datum to sit at the script UTxO. Imagine that instead of those hashes being specified in the datum, and then have the script look at the datum, we could instead *bake* in those public key hashes inside the script itself. In that case, the script would no longer need to look at the datum to determine whether a transaction is valid or not. Instead, it would already have that information inside the script itself. These types of validators are referred to as **parameterised validators**.

It is important to note that, as always, validator scripts are compiled down to the form of `BuiltinData -> BuiltinData -> BuiltinData -> ()`, even with parameterised validators. However, in this case, we can write our logic in a different type signature that allows us to write the validators in a more high-level way. The type signature of the `mkValidator` function in parameterised validators is: 
`ValidatorParam -> DatumType -> RedeemerType -> ScriptContext -> Bool`

The new argument `ValidatorParam` allows us to bake in any data we want into the script itself. We can define it with as many fields as we want, but as always, unless we use types that are already instances of `ToData` typeclass, we have to instantiate them and additionally with parameter types, we have to **lift** them. -- might need some more explaining here
As with typed validators we can still define our own `datum` and `redeemer` types. And we have to create the `ValidatorTypes` instance to use with our validator as before.

Let's write a parameterised validator that is a simple deadline function. Before the deadline, all transactions are validated, and after the deadline all are invalidated. The deadline will be our parameter for the validator that is baked in the script. This deadline needs to be expressed in the `Plutus.POSIXTime` type, which is measured as the number of miliseconds since `1970-01-01T00:00:00Z`. Plutus always works with POSIXTime, while the Cardano chain works on `slots`. The reason for this discrepancy is that the slot length of the chain is a parameter that can change over time!

We can start of by creating the simple `DeadlineValidator` type for the validator. Since we don't need the datum and redeemer in this case, we can ommit their definitions to apply the default `()` type for both of them.

```
data Deadline
instance Scripts.ValidatorTypes Deadline -- default types for datum and redeemer are ()
```

Since `POSIXTime` already is an instance of `ToData`, we don't need to worry about lifting values right now and can go straight to the `mkValidator` function. The signature will be `mkValidator :: Plutus.POSIXTime -> () -> () -> Plutus.ScriptContext -> Bool` and inside the function we want to check that the `txInfoValidRange` is contained in its entirety in the interval of negative infinity to the deadline. Important to note that the ENTIRE tx range must fall into this interval, because if the positive end of the valid range would go over the deadline, the transaction could be validated after the deadline even if the majority of the valid range is before the deadline!

```
mkValidator :: Plutus.POSIXTime -> () -> () -> Plutus.ScriptContext -> Bool
mkValidator deadline _ _ ctx =
  traceIfFalse "Invalid tx range" $ to deadline `contains` txRange
    where
      info :: Plutus.TxInfo
      info = scriptContextTxInfo ctx

      txRange :: Plutus.POSIXTimeRange
      txRange = txInfoValidRange info
```

Now, we need to compile the parameterised validator. We do this with the `PSU.V2.mkTypedValidatorParam` instead of `PSU.V2.mkTypedValidator`:
```
-- | Make a 'TypedValidator' from the 'CompiledCode' of a parameterized validator script and its wrapper.
mkTypedValidatorParam ::
  forall a param.
  Lift DefaultUni param =>
  -- | Validator script (compiled)
  CompiledCode (param -> ValidatorType a) ->
  -- | A wrapper for the compiled validator
  CompiledCode (ValidatorType a -> UntypedValidator) ->
  -- | The extra paramater for the validator script
  param ->
  TypedValidator a
mkTypedValidatorParam vc wrapper param =
  mkTypedValidator (vc `applyCode` liftCode param) wrapper
```

This function takes similar arguments as before. `CompiledCode (param -> ValidatorType a)` is our parametarised `mkValidator` function, `CompiledCode (ValidatorType a -> UntypedValidator)` is our wrapper to `BuiltinData -> BuiltinData -> BuiltinData -> ()`, and what we get is `param -> TypedValidator a` which will be the type signature of our `typedValidator` function. This makes sense, as our validator accepts a parameter to be baked in the validator. The validator can only be complete once that parameter is received:

```
typedValidator :: Plutus.POSIXTime -> PSU.V2.TypedValidator Deadline
typedValidator = PSU.V2.mkTypedValidatorParam @Deadline
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PSU.mkUntypedValidator
```

Next step is to get the script from our validator. Before we used simply `PSU.V2.validatorScript typedValidator` and `Plutus.unValidatorScript validator`, but since `typedValidator` now accepts one argument, we need to compose these functions together, and the `validator` and `script` functions also must receive the deadline parameter:
Note: we could write it a different way, this is using ETA reduction.
```
validator :: Plutus.POSIXTime -> Plutus.Validator
validator = PSU.V2.validatorScript . typedValidator

script :: Plutus.POSIXTime -> Plutus.Script
script = Plutus.unValidatorScript . validator
```

The last step is to write the serialised script to a file. Our writing script functions change a bit as a result of the additional parameter the `script` function must receive. At this stage, we simply need to apply the parameter to the `script` function. We will write it in a way so that `writeSerialisedDeadlineParamScript` accepts the deadline parameter and passes it on to create a script with our specified deadline. So using this function we can create many different scripts of the same family (same validation logic), but different deadline parameters.
```
deadlineParamShortBs :: Plutus.POSIXTime -> SBS.ShortByteString
deadlineParamShortBs deadline = SBS.toShort . LBS.toStrict $ serialise $ script deadline

deadlineParamScriptSerialised :: Plutus.POSIXTime -> PlutusScript PlutusScriptV2
deadlineParamScriptSerialised deadline = PlutusScriptSerialised $ deadlineParamShortBs deadline

writeSerialisedDeadlineParamScript :: Plutus.POSIXTime -> IO (Either (FileError ()) ())
writeSerialisedDeadlineParamScript deadline = writeFileTextEnvelope "compiled/deadlineParam.plutus" Nothing $ deadlineParamScriptSerialised deadline
```

In GHCi:
`writeSerialisedDeadlineParamScript 1673010000000`

As before we have the helper scripts for testing:
```
create-script-address.sh
check-utxos.sh
send-funds-to-script.sh
spend-script-utxo-invalidRange.sh
spend-script-utxo.sh
```


The previous example used a `POSIXTime` type as the script parameter. For the next example, we will look at custom defined parameter type as there is some extra work we need to do in that case. We will transform the *shared* script from earlier to be parameterised instead of relying on the UTxO datum. That means that either of the trusted parties can spend any UTxO sitting at the script address, regardless of what the datum associated is.

Firstly, we create the same type as before, this time calling it `sharedParam`:

```
data SharedParam = SharedParam {
    wallet1 :: PubKeyHash,
    wallet2 :: PubKeyHash
  }
```
As before, we need to make our parameter type an instance of `ToData`/`FromData` via `PlutusTx.unstableMakeIsData`:
```
PlutusTx.unstableMakeIsData ''SharedParam
```
In addition, when defining custom parameter types, we need to make them an instance of the `Lift` class (https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Lift-Class.html#t:Lift). This allows the type to be lifted into so-called *Plutus IR* (intermediate representation), which is then handled further to Plutus Core, but essentially, we just need to this to allow the compiler to compile our type to Plutus Core. This is done with the `PlutusTx.makeLift` function that automatically derives an instance for us:
```
PlutusTx.makeLift ''SharedParam
```

Note that `PubKeyHash` type is an instance of both `ToData`/`FromData` and `Lift`, but our defined type which contains these two fields is not.

The rest is pretty much the same as in the typed validator process:
```
data SharedValidator
instance Scripts.ValidatorTypes SharedValidator -- default types for datum and redeemer are ()
...
```

As before we have the helper scripts for testing:
```
create-script-address.sh
check-utxos.sh
send-funds-to-script.sh
spend-script-utxo.sh
```