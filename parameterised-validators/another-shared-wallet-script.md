# Another Shared Wallet Script

The previous example used a `POSIXTime` type as the script parameter. For the next example, we will look at custom defined parameter type as there is some extra work we need to do in that case. We will transform the _shared_ script from earlier to be parameterised instead of relying on the UTxO datum. That means that either of the trusted parties can spend any UTxO sitting at the script address, regardless of what the datum associated is.

Firstly, we create the same type as before, this time calling it `sharedParam`:

```haskell
data SharedParam = SharedParam {
    wallet1 :: PubKeyHash,
    wallet2 :: PubKeyHash
  }
```

As before, we need to make our parameter type an instance of `ToData`/`FromData` via `PlutusTx.unstableMakeIsData`:

```haskell
PlutusTx.unstableMakeIsData ''SharedParam
```

In addition, when defining custom parameter types, we need to make them an instance of the `Lift` class ([https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Lift-Class.html#t:Lift](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx-Lift-Class.html#t:Lift)). This allows the type to be lifted into so-called _Plutus IR_ (intermediate representation), which is then handled further to Plutus Core, but essentially, we just need this to allow the compiler to compile our type to Plutus Core. This is done with the `PlutusTx.makeLift` function that automatically derives an instance for us:

```haskell
PlutusTx.makeLift ''SharedParam
```

Note that `PubKeyHash` type is an instance of both `ToData`/`FromData` and `Lift`, but our defined type `SharedParam`  which contains two `PubKeyHash` fields is not.

The rest is pretty much the same as in the [typed validator version](../typed-validators/a-shared-wallet-script.md). We need to create a `ValidatorTypes` instance, but since we are using a parameter that will bake our `sharedParam` inside the script, we do not need datum and redeemer:

```haskell
data SharedValidator
instance Scripts.ValidatorTypes SharedValidator
    -- default types for datum and redeemer are ()
```

Our `mkValidator` function still for valid signatures in the transaction context, except this time the `PubKeyHash`es are deconstructed from the `SharedParam` argument:

```haskell
{-# INLINEABLE mkValidator #-}
mkValidator :: SharedParam -> () -> () -> Plutus.ScriptContext -> Bool
mkValidator sp () () ctx = checkSignature1 || checkSignature2
  where
    info :: Plutus.TxInfo
    info = Plutus.scriptContextTxInfo ctx

    signature1 :: PubKeyHash
    signature1 = wallet1 sp

    signature2 :: PubKeyHash
    signature2 = wallet2 sp

    checkSignature1 :: Bool
    checkSignature1 = traceIfFalse "signature 1 missing" $ txSignedBy info signature1

    checkSignature2 :: Bool
    checkSignature2 = traceIfFalse "signature 2 missing" $ txSignedBy info signature2
```

As with the [parameterised deadline script example](a-deadline-script.md), we use the `PSU.V2.mkTypedValidatorParam` to define a function that accepts the parameter to compile the validator:

```haskell
typedValidator :: SharedParam -> PSU.V2.TypedValidator SharedValidator
typedValidator =
  PSU.V2.mkTypedValidatorParam @SharedValidator
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = PSU.mkUntypedValidator

validator :: SharedParam -> Plutus.Validator
validator = PSU.V2.validatorScript . typedValidator

script :: SharedParam -> Plutus.Script
script = Plutus.unValidatorScript . validator

-- serialise
sharedParamShortBs :: SharedParam -> SBS.ShortByteString
sharedParamShortBs sp = SBS.toShort . LBS.toStrict $ serialise $ script sp

sharedParamScriptSerialised :: SharedParam -> PlutusScript PlutusScriptV2
sharedParamScriptSerialised sp = PlutusScriptSerialised $ sharedParamShortBs sp

writeSerialisedSharedParamScript :: SharedParam -> IO (Either (FileError ()) ())
writeSerialisedSharedParamScript sp = writeFileTextEnvelope "compiled/sharedParam.plutus" Nothing $ sharedParamScriptSerialised sp
```

And finally we define the functions to serialise and write the script file:

```haskell
sharedParamShortBs :: SharedParam -> SBS.ShortByteString
sharedParamShortBs sp = SBS.toShort . LBS.toStrict $ serialise $ script sp

sharedParamScriptSerialised :: SharedParam -> PlutusScript PlutusScriptV2
sharedParamScriptSerialised sp = PlutusScriptSerialised $ sharedParamShortBs sp

writeSerialisedSharedParamScript :: SharedParam -> IO (Either (FileError ()) ())
writeSerialisedSharedParamScript sp = writeFileTextEnvelope "compiled/sharedParam.plutus" Nothing $ sharedParamScriptSerialised sp
```



As before we have the helper scripts for testing:

1\) `create-script-address.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet

# Build script address
cardano-cli address build \
--payment-script-file sharedParam.plutus \
--testnet-magic $NWMAGIC \
--out-file sharedParam.addr

echo "Script address: $(cat sharedParam.addr)"
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
--address $(cat sharedParam.addr) \
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
    --tx-in 7029f2bb2b538c32f862bf97fc4e607a362e1b3307a194a50c666499df0fcef9#1 \
    --tx-out $(cat sharedParam.addr)+20000000 \
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
    --change-address $(cat ../normal_address/02.addr) \
    --tx-in e450e63fa7b8b828db44840c721579443a0fc09eb8a1ed47ec698a57673b9e88#0 \
    --tx-in-script-file sharedParam.plutus \
    --tx-in-datum-file ../assets/unit.json \
    --tx-in-redeemer-file ../assets/unit.json \
    --tx-in-collateral 4ff088fb16d00fe69cec559816151d7e3dbff32b6a837bc2e305d5c62be225c0#1 \
    --required-signer-hash 1dbbab8486140e253674dd2af159c322c5c48232b1a358670b1ef5a7 \
    --protocol-params-file ../normal_address/protocol.json \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file ../normal_address/02.skey \
    --testnet-magic $NWMAGIC \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic $NWMAGIC \
    --tx-file tx.signed
```
