# A Deadline Script

Let's write a parameterised validator that is a simple deadline function. Before the deadline, all transactions are validated, and after the deadline, all are invalidated. The deadline will be our parameter for the validator that is baked in the script. This deadline needs to be expressed in the `Plutus.POSIXTime` type, which is measured as the number of milliseconds since `1970-01-01T00:00:00Z`. Plutus always works with POSIXTime, while the Cardano chain works on `slots`. The reason for this discrepancy is that the slot length of the chain is a parameter that can change over time!

We can start off by creating the simple `DeadlineValidator` type for the validator. Since we don't need the datum and redeemer in this case, we can omit their definitions to apply the default `()` type for both of them.

```haskell
data Deadline
instance Scripts.ValidatorTypes Deadline
    -- default types for datum and redeemer are ()
```

Since `POSIXTime` already is an instance of `ToData`, we don't need to worry about lifting values right now and can go straight to the `mkValidator` function. The signature will be `mkValidator :: Plutus.POSIXTime -> () -> () -> Plutus.ScriptContext -> Bool` and inside the function, we want to check that the `txInfoValidRange` is contained in its entirety in the interval of negative infinity to the deadline. Important to note that the **ENTIRE** tx range must fall into this interval because if the positive end of the valid range would go over the deadline, the transaction could be validated after the deadline even if the majority of the valid range is before the deadline!

```haskell
mkValidator :: Plutus.POSIXTime -> () -> () -> Plutus.ScriptContext -> Bool
mkValidator deadline _ _ ctx =
  traceIfFalse "Invalid tx range" $ to deadline `contains` txRange
    where
      info :: Plutus.TxInfo
      info = scriptContextTxInfo ctx

      txRange :: Plutus.POSIXTimeRange
      txRange = txInfoValidRange info
```

Now, we need to compile the parameterised validator. We do this with the `PSU.V2.mkTypedValidatorParam` instead of `PSU.V2.mkTypedValidator`. Here is the `mkTypedValidatorParam` definition:

```haskell
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

This function takes similar arguments as before. `CompiledCode (param -> ValidatorType a)` is our parameterised `mkValidator` function, `CompiledCode (ValidatorType a -> UntypedValidator)` is our wrapper to `BuiltinData -> BuiltinData -> BuiltinData -> ()`, and what we get as a result is `param -> TypedValidator a` which will be the type signature of our `typedValidator` function. This makes sense, as our validator accepts a parameter to be baked in the validator. The validator can only be completed once that parameter is received and applied, finally resulting in `UntypedValidator`. Our `typedValidator` function now looks like this:

```haskell
typedValidator :: Plutus.POSIXTime -> PSU.V2.TypedValidator Deadline
typedValidator = PSU.V2.mkTypedValidatorParam @Deadline
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PSU.mkUntypedValidator
```

The next step is to get the script from our validator. Before we used simply `PSU.V2.validatorScript typedValidator` and `Plutus.unValidatorScript validator`, but since `typedValidator` now accepts one argument, we need to compose these functions together, and the `validator` and `script` functions also must receive the deadline parameter:&#x20;

```haskell
validator :: Plutus.POSIXTime -> Plutus.Validator
validator = PSU.V2.validatorScript . typedValidator

script :: Plutus.POSIXTime -> Plutus.Script
script = Plutus.unValidatorScript . validator

-- Note: we could write it a different way, this is using ETA reduction.
```

The last step is to write the serialised script to a file. Our writing script functions change a bit as a result of the additional parameter the `script` function must receive. At this stage, we simply need to apply the parameter to the `script` function. We will write it in a way so that `writeSerialisedDeadlineParamScript` accepts the deadline parameter and passes it on to create a script with our specified deadline. So using this function we can create many disfferent scripts of the same family (same validation logic), but with different deadline parameters.

```haskell
deadlineParamShortBs :: Plutus.POSIXTime -> SBS.ShortByteString
deadlineParamShortBs deadline = SBS.toShort . LBS.toStrict $ serialise $ script deadline

deadlineParamScriptSerialised :: Plutus.POSIXTime -> PlutusScript PlutusScriptV2
deadlineParamScriptSerialised deadline = PlutusScriptSerialised $ deadlineParamShortBs deadline

writeSerialisedDeadlineParamScript :: Plutus.POSIXTime -> IO (Either (FileError ()) ())
writeSerialisedDeadlineParamScript deadline = writeFileTextEnvelope "compiled/deadlineParam.plutus" Nothing $ deadlineParamScriptSerialised deadline
```

That's it! We can now load our new module in `cabal repl` and write the script to a file:

`ghci> writeSerialisedDeadlineParamScript 1673010000000`

As before we have the helper scripts for testing:

1\) `create-script-address.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet

# Build script address
cardano-cli address build \
--payment-script-file deadlineParam.plutus \
--testnet-magic $NWMAGIC \
--out-file deadlineParam.addr

echo "Script address: $(cat deadlineParam.addr)"
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
--address $(cat deadlineParam.addr) \
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
    --tx-in 892ff638586f24f2413bab71ff49acdef62af2e8d72ad223b67e11d2607e33d6#1 \
    --tx-out $(cat deadlineParam.addr)+20000000 \
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

4\) `spend-script-utxo-invalidRange.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../normal_address/02.addr) \
    --tx-in 7029f2bb2b538c32f862bf97fc4e607a362e1b3307a194a50c666499df0fcef9#0 \
    --tx-in-script-file deadlineParam.plutus \
    --tx-in-datum-file ../assets/unit.json \
    --tx-in-redeemer-file ../assets/unit.json \
    --tx-in-collateral 45337a0fb353dadc7e31f865378885207553b4471814384421e0fa1607271bf6#1 \
    --tx-out $(cat deadlineParam.addr)+10000000 \
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

5\) `spend-script-utxo.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../normal_address/02.addr) \
    --invalid-hereafter 6348747 \
    --tx-in 7029f2bb2b538c32f862bf97fc4e607a362e1b3307a194a50c666499df0fcef9#0 \
    --tx-in-script-file deadlineParam.plutus \
    --tx-in-datum-file ../assets/unit.json \
    --tx-in-redeemer-file ../assets/unit.json \
    --tx-in-collateral 45337a0fb353dadc7e31f865378885207553b4471814384421e0fa1607271bf6#1 \
    --tx-out $(cat deadlineParam.addr)+10000000 \
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
