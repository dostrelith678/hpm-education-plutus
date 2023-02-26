# A Stake Validator Script

In this `StakeValidator` example, we will create a simple script that controls staking actions via secret codes that the script is parameterised with. We will create the `CodeParam` as before:

```haskell
data CodeParam = CodeParam {
  cert :: Integer,
  reward :: Integer
}
PlutusTx.unstableMakeIsData ''CodeParam
PlutusTx.makeLift ''CodeParam
```

The `cert` field will contain an `Integer` code that must be specified in the redeemer for script certificates to be validated, and the `reward` code will have to be specified in order to withdraw rewards. We will be writing this as a typed validator so we need a `ValidatorTypes` instance as well:

```haskell
data CodeValidator
instance PSU.ValidatorTypes CodeValidator where
  type instance RedeemerType CodeValidator = Integer
  -- we only care about the redeemer type for the stake validator
```

We now have everything to create the `mkStakingValidator` function which will represent our stake validator logic:

```haskell
mkStakingValidator :: CodeParam -> Integer -> ScriptContext -> Bool
mkStakingValidator cp redeemer ctx = 
  case scriptContextPurpose ctx of
    Certifying _   -> redeemer == cert cp
    Rewarding  _   -> redeemer == reward cp
    _              -> False
```

We simply need to check that the `redeemer` matches the corresponding action. Any script purpose other than `Certifying` or `Rewarding` will always be `False`. Next, we need to compile that function into a `StakeValidator`. Since there is still no interface for `TypedStakeValidator`, we have to use somewhat explicit code with `mkStakeValidatorScript` and `mkUntypedStakeValidator`. This function is defined as:

```haskell
mkUntypedStakeValidator
    :: PV1.UnsafeFromData r
    => (r -> sc -> Bool)
    -> UntypedStakeValidator
mkUntypedStakeValidator f r p =
    check $ f (PV1.unsafeFromBuiltinData r) (PV1.unsafeFromBuiltinData p)
```

It receives a function `(r -> sc -> Bool)` and returns the `UntypedStakeValidator` which is the end result we want here. Since this is a parameterised contract, we have to apply our `CodeParam` to the `mkStakingValidator` function first in order to get just the `(Integer -> ScriptContext -> Bool)` function that is required. That is why we have to compose the two functions together and _apply_ the `CodeParam`. However, since this is all being compiled to Plutus IR (intermediate Plutus Core), we also have to first _lift_ the `CodeParam` value to Plutus IR for it to be applied.

We do this with `PlutusTx.liftCode cp`, and we are able to do it because we made `CodeParam` an instance of the `Lift` class! In the previous examples, this was abstracted for us via the `mkTypedValidator` function, but since one is not available for stake validators (yet), we have to do it manually here:

```haskell
validator :: CodeParam -> V2.StakeValidator
validator cp = V2.mkStakeValidatorScript $
  $$(PlutusTx.compile [|| mkUntypedStakeValidator . mkStakingValidator ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode cp
```

The rest is the same as before, the only difference being the function names, and instead of `unValidatorScript`, we use `unStakeValidatorScript` to get the `Script` type of the validator:

```haskell
script :: CodeParam -> V2.Script
script = V2.unStakeValidatorScript . validator

codeParamShortBs :: CodeParam -> SBS.ShortByteString
codeParamShortBs cp = SBS.toShort . LBS.toStrict $ serialise $ script cp

codeParamScriptSerialised :: CodeParam -> PlutusScript PlutusScriptV2
codeParamScriptSerialised cp = PlutusScriptSerialised $ codeParamShortBs cp

writeSerialisedCodeParamScript :: CodeParam -> IO (Either (FileError ()) ())
writeSerialisedCodeParamScript cp = writeFileTextEnvelope "compiled/codeParam.plutus" Nothing $ codeParamScriptSerialised cp
```

#### Helper scripts

1\) `create-script-address.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet

# Build script stake address
cardano-cli stake-address build \
  --testnet-magic ${NWMAGIC} \
  --stake-script-file codeParam.plutus \
  --out-file codeParam.stake

# Build address with 01.vkey for payment part and script stake part
cardano-cli address build \
--payment-verification-key-file ../normal_address/01.vkey \
--stake-script-file codeParam.plutus \
--testnet-magic $NWMAGIC \
--out-file codeParam.addr

echo "Script address: $(cat codeParam.addr)"
```

2\) `check-utxos.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

funds_normal1=$(cardano-cli query utxo \
--address $(cat ../normal_address/01.addr) \
--testnet-magic $NWMAGIC)


funds_script=$(cardano-cli query utxo \
--address $(cat codeParam.addr) \
--testnet-magic $NWMAGIC)

echo "Normal address1:"
echo "${funds_normal1}"
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
    --tx-in 1b38e5cd5683a1c6595e2d62dd2c797e15a43440f6106b72533fb91cb6d762fa#1 \
    --tx-out $(cat codeParam.addr)+200000000 \
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

4\) `register-and-delegate-script.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli stake-address registration-certificate \
  --stake-script-file codeParam.plutus \
  --out-file reg.cert

cardano-cli stake-address delegation-certificate \
  --stake-script-file codeParam.plutus \
  --stake-pool-id pool1vzqtn3mtfvvuy8ghksy34gs9g97tszj5f8mr3sn7asy5vk577ec \
  --out-file deleg.cert

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat codeParam.addr) \
    --tx-in 0ecbdedf2f535c9931e4b36aa2b13fac93e9afa84d9b4797cc93ee24d42922fc#2 \
    --tx-in-collateral 0ecbdedf2f535c9931e4b36aa2b13fac93e9afa84d9b4797cc93ee24d42922fc#2 \
    --certificate-file reg.cert \
    --certificate-file deleg.cert \
    --certificate-script-file codeParam.plutus \
    --certificate-redeemer-file ../assets/1.json \
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

5\) `regelegate-invalid.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli stake-address delegation-certificate \
  --stake-script-file codeParam.plutus \
  --stake-pool-id pool10c40pnzz3e00kuej05xfcs2ptkekhys48q7qc4jjcsysypj46qv \
  --out-file redeleg.cert

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat codeParam.addr) \
    --tx-in 313ba91e11efac7827d057613251273e193b71d995592ff0108252b19c2d0e26#0 \
    --tx-in-collateral 313ba91e11efac7827d057613251273e193b71d995592ff0108252b19c2d0e26#0 \
    --certificate-file redeleg.cert \
    --certificate-script-file codeParam.plutus \
    --certificate-redeemer-file ../assets/2.json \
    --protocol-params-file ../normal_address/protocol.json \
    --out-file tx.body


```

6\) `redelegate-valid.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli stake-address delegation-certificate \
  --stake-script-file codeParam.plutus \
  --stake-pool-id pool1vzqtn3mtfvvuy8ghksy34gs9g97tszj5f8mr3sn7asy5vk577ec \
  --out-file deleg.cert

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat codeParam.addr) \
    --tx-in 0ecbdedf2f535c9931e4b36aa2b13fac93e9afa84d9b4797cc93ee24d42922fc#2 \
    --tx-in-collateral 0ecbdedf2f535c9931e4b36aa2b13fac93e9afa84d9b4797cc93ee24d42922fc#2 \
    --certificate-file deleg.cert \
    --certificate-script-file codeParam.plutus \
    --certificate-redeemer-file ../assets/1.json \
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

7\) `check-script-rewards.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

rewards_script=$(cardano-cli query stake-address-info \
--address $(cat codeParam.stake) \
--testnet-magic $NWMAGIC)

echo "Script address:"
echo "${rewards_script}"
```

8\) `withdraw-rewards.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


rewards=$(cardano-cli query stake-address-info \
    --address $(cat codeParam.stake) \
    --testnet-magic $NWMAGIC | jq -r ".[0].rewardAccountBalance")

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat codeParam.addr) \
    --tx-in 092f5b8926f8bac5e50ee864e0aaeab7b37d3329cf07e27dcd48fff8a1e636db#0 \
    --tx-in-collateral 45337a0fb353dadc7e31f865378885207553b4471814384421e0fa1607271bf6#1 \
    --withdrawal $(cat codeParam.stake)+${rewards} \
    --withdrawal-script-file codeParam.plutus \
    --withdrawal-redeemer-file ../assets/2.json \
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
