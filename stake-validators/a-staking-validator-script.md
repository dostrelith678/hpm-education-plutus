# A Stake Validator Script

In this `StakeValidator` example, we will create a simple script that controls staking actions via secret codes that the script is parameterised with.&#x20;

### Writing the validator

First, we will use the same GHC extensions as with the [parameterised shared wallet script](../parameterised-validators/another-shared-wallet-script.md) and the same module imports (except we don't need `txSignedBy` here).

```haskell
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}

module StakingValidator
  (
    scriptSerialised,
    writeSerialisedScript,
    CodeParam (..)
  )
where

import qualified PlutusTx
import PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as Plutus

import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2, writeFileTextEnvelope)
import Cardano.Api (FileError)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise

import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.Script.Utils.Typed as PSU

import Prelude (IO)

...

```

Then, we need to create a parameter for the script as before, calling it `CodeParam`, and making it an instance of `Data` and `Lift` classes.

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
  -- We only care about the redeemer type for the stake validator
```

We now have everything to create the `mkStakingValidator` function which will represent our stake validator logic:

```haskell
mkStakingValidator :: CodeParam -> Integer -> Plutus.ScriptContext -> Bool
mkStakingValidator cp redeemer ctx = 
  case Plutus.scriptContextPurpose ctx of
    Plutus.Certifying _   -> redeemer == cert cp
    Plutus.Rewarding  _   -> redeemer == reward cp
    _                     -> False
```

We simply need to check that the `redeemer` matches the corresponding action. Any script purpose other than `Certifying` or `Rewarding` will always be `False`.

Next, we need to compile that function into a `StakeValidator` type with the end goal of the `UntypedStakeValidator` that we looked at earlier. Since there is still no interface `TypedStakeValidator`, we have to use somewhat explicit code with `mkStakeValidatorScript` and [`mkUntypedStakeValidator`](https://input-output-hk.github.io/plutus-apps/main/plutus-script-utils/html/Plutus-Script-Utils-Typed.html#v:mkUntypedStakeValidator). This function is defined as:

```haskell
mkUntypedStakeValidator
    :: PV1.UnsafeFromData r
    => (r -> sc -> Bool)
    -> UntypedStakeValidator
mkUntypedStakeValidator f r p =
    check $ f (tracedUnsafeFrom "Redeemer decoded successfully" r)
              (tracedUnsafeFrom "Script context decoded successfully" p)
```

It receives a function `(r -> sc -> Bool)` and returns the `UntypedStakeValidator` which is the end result we want here. Since this is a parameterised contract, we have to apply our `CodeParam` to the `mkStakingValidator` function first in order to get just the `(Integer -> ScriptContext -> Bool)` function that is required here.

That is why we have to compose the two functions together and _apply_ the `CodeParam`. However, since this is all being compiled to Plutus IR (intermediate Plutus Core), we also have to first _lift_ the `CodeParam` value to Plutus IR for it to be applied.

We do this with `PlutusTx.liftCode cp`, and we are able to do it because we made `CodeParam` an instance of the `Lift` class. In the previous examples, this was abstracted for us via the `mkTypedValidatorParam` function, but since one is not available for stake validators (yet), we have to do it manually here:

```haskell
validator :: CodeParam -> PSU.V2.StakeValidator
validator cp = Plutus.mkStakeValidatorScript $
  $$(PlutusTx.compile [|| PSU.mkUntypedStakeValidator . mkStakingValidator ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode cp
```

The rest is the same as before, the only difference being that instead of `unValidatorScript`, we use `unStakeValidatorScript` to get the `Script` type of the validator:

```haskell
script :: CodeParam -> Plutus.Script
script = Plutus.unStakeValidatorScript . validator

scripShortBs :: CodeParam -> SBS.ShortByteString
scripShortBs cp = SBS.toShort . LBS.toStrict $ serialise $ script cp

scriptSerialised :: CodeParam -> PlutusScript PlutusScriptV2
scriptSerialised cp = PlutusScriptSerialised $ scripShortBs cp

writeSerialisedScript :: CodeParam -> IO (Either (FileError ()) ())
writeSerialisedScript cp = writeFileTextEnvelope "compiled/StakingValidator.plutus" Nothing $ scriptSerialised cp
```

All that is left to do is come up with a concrete instance of the `CodeParam` and pass it to the `writeSerialisedScript` function to compile the validator. For this example, we will just use the integers `1` and `2` as our secret codes. In the `cabal repl` of our `nix-shell`, we can do that with:&#x20;

<pre class="language-bash"><code class="lang-bash"><strong>Prelude> :l src/StakingValidator.hs  
</strong>[1 of 1] Compiling StakingValidator ( src/StakingValidator.hs, /home/plutus/hpm-plutus/hpm-validators/dist-newstyle/build/x86_64-linux/ghc-8.10.7/hpm-validators-0.1.0.0/build/StakingValidator.o )
Ok, one module loaded.
Prelude StakingValidator> myCodeParam = CodeParam 1 2
Prelude StakingValidator> writeSerialisedScript myCodeParam 
Right ()
</code></pre>

### Testing the validator

To test a staking validator, we will need to slightly change our usual way of creating a script address with `create-script-address.sh`. So far we have only been using the payment credentials part to create an address, either a verification key or a validator script hash. Here, we want to add the optional staking credential part and use our stake validator script to provide the validation logic.

We can first use the `cardano-cli stake-address build` to get the stake address for this validator. When we do that, this address will be just a "Reward account address" (terminology from [Cardano docs](https://docs.cardano.org/learn/cardano-addresses/)), unable to receive any UTxO, but could be used as a rewards address for delegation.

As mentioned above, we usually want to combine payment and staking credentials into one address, so let's do that next. We will specify the `01.vkey` for the payment credential, and use our stake validator script to provide the staking credential. The full Bash script looks like this:

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet


# Build script stake address
cardano-cli stake-address build \
  --testnet-magic ${NWMAGIC} \
  --stake-script-file ../../compiled/StakingValidator.plutus \
  --out-file StakingValidator.stake

echo "Script stake address: $(cat StakingValidator.stake)"

# Build address with 01.vkey for payment part and script stake part
cardano-cli address build \
  --payment-verification-key-file ../address/01.vkey \
  --stake-script-file ../../compiled/StakingValidator.plutus \
  --testnet-magic ${NWMAGIC} \
  --out-file StakingValidator.addr

echo "Full address: $(cat StakingValidator.addr)"
```

Running the script provides us with the two addresses, the first one being just the stake address and the second a fully combined address:

```bash
./create-script-address.sh
Script stake address: stake_test17peya46y0tymw8cq6hgdlzdlrys58acwsww4luzk0yur9vgy0xqrc
Full address: addr_test1yzjaxxx6m76jamlmyc9wp9lcg6h2pjncumxyleqx6n8wmsrjfmt5g7kfku0sp4wsm7ym7xfpg0msaquatlc9v7fcx2cs65ntf3
```

As usual, we need a way of checking the UTxO on our addresses, so we update the `check-utxos.sh` script for this `StakingValidator.addr` we just created.

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

We can now send some funds to the script with `send-funds-to-script.sh` that we will delegate to a pool.

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
  --testnet-magic $NWMAGIC \
  --change-address $(cat ../address/01.addr) \
  --tx-in abe233c49d4162d886a0e38e2ed03739cc9feb0b5f38eb54d8417eb9821f039b#1 \
  --tx-out $(cat StakingValidator.addr)+200000000 \
  --tx-out-datum-embed-file ../../compiled/assets/unit.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file ../address/01.skey \
  --testnet-magic $NWMAGIC \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic $NWMAGIC \
  --tx-file tx.signed
```

```bash
./send-funds-to-script.sh 
Estimated transaction fee: Lovelace 170341
Transaction successfully submitted.

./check-utxos.sh 
Normal address1:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5dc5111e257f8e68b0978c9619e57bbb12d365c0ec45d879115866bb674156ae     0        1826915 lovelace + TxOutDatumNone
9f24a56e249b86b3216cd337d9ada7fe3a030e339c97ab00191bc496b03132ed     1        9624465215 lovelace + TxOutDatumNone
ede24e9e40ca82830c75d827b5c3b090132c1afaebd3a4256655fb5d2382474a     0        9649776 lovelace + TxOutDatumNone
ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1     0        1829006 lovelace + TxOutDatumNone

Script address:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9f24a56e249b86b3216cd337d9ada7fe3a030e339c97ab00191bc496b03132ed     0        200000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
```

It's time to finally register and delegate this address to a pool. Besides submitting the registration certificate, we also have to find a pool to delegate to. We can get a list of stake pools from `cardano-cli` with:

```bash
cardano-cli query stake-pools --testnet-magic 2
pool1q95luz38nhsw6h7mxud8tptc6mxvnsczhanw4j5htk8h2ltlf3k
pool1qxcz3zxnye8g9ejsqslhl0ljevdx895uc80lr89ulf92gcv40ng
pool1qncwwllw9nwtu7sl7zqw3fpyh4t3q6nhludryfwv0jyqjygd46d
pool1qal80uhlj949mgv0ecvdkmgqjdn5q27wmpaj4crnr5e9v6qmsv7
pool1p9xu88dzmpp5l8wmjd6f5xfs9z89mky6up86ty2wz4aavmm8f3m
pool1p835jxsj8py5n34lrgk6fvpgpxxvh585qm8dzvp7ups37vdet5a
...
```

We can use [preview.cardanoscan.io](https://preview.cardanoscan.io) to check pool information. Let's pick the second pool from the list [`pool1qxcz3zxnye8g9ejsqslhl0ljevdx895uc80lr89ulf92gcv40ng`](https://preview.cardanoscan.io/pool/01b02888d3264e82e650043f7fbff2cb1a63969cc1dff19cbcfa4aa4) because it is regularly creating blocks so we know we will get rewards. We can submit both the address registration and delegation certificate in the same transaction. Our validator defined two secret integer codes (we used `1` and `2`) that need to be specified as the redeemer for transactions, the first one required to validate certificates being submitted (registration, delegation, deregistration), and the second one for validating rewards withdrawals.

Let's create a `register-and-delegate-script.sh` script that will do that for us. We create the registration and delegation certificates and attach them to the `transaction build` command. Then we have to specify the `--certificate-script-file` argument since our certificates are validated by our script rather than regular keys. The script must receive a redeemer and since our redeemer is very simple (just the integer value `1`), we can use `--certificate-redeemer-value 1`. Besides that, we can specify the `--change-address $(cat StakingValidator.addr)` to send any remaining funds after the transaction fees are substracted to the staking address as well to be delegated.&#x20;

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli stake-address registration-certificate \
  --stake-script-file ../../compiled/StakingValidator.plutus \
  --out-file reg.cert

cardano-cli stake-address delegation-certificate \
  --stake-script-file ../../compiled/StakingValidator.plutus \
  --stake-pool-id pool1qxcz3zxnye8g9ejsqslhl0ljevdx895uc80lr89ulf92gcv40ng \
  --out-file deleg.cert

cardano-cli transaction build \
  --testnet-magic $NWMAGIC \
  --change-address $(cat StakingValidator.addr) \
  --tx-in 9f24a56e249b86b3216cd337d9ada7fe3a030e339c97ab00191bc496b03132ed#01 \
  --tx-in-collateral ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1#0 \
  --certificate-file reg.cert \
  --certificate-file deleg.cert \
  --certificate-script-file ../../compiled/StakingValidator.plutus \
  --certificate-redeemer-value 1 \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file ../address/01.skey \
  --testnet-magic $NWMAGIC \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic $NWMAGIC \
  --tx-file tx.signed
```

```bash
./register-and-delegate.sh 
Estimated transaction fee: Lovelace 313896
Transaction successfully submitted.
```

We can check [Cardanoscan](https://preview.cardanoscan.io/stakekey/f0724ed7447ac9b71f00d5d0df89bf192143f70e839d5ff056793832b1) again to look for our script address and see that the transaction did what was expected. It is now registered and delegated to the pool we specified.

You are free to try redelegating now by creating a new `delegation-certificate` with the wrong redeemer or an absent redeemer but our staking validator will invalidate the transaction.

We can create a `check-script-rewards.sh` next, but it will be empty for a few days until the first rewards start coming in (epochs are 1 day long on the Preview testnet).

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

rewards_script=$(cardano-cli query stake-address-info \
--address $(cat StakingValidator.stake) \
--testnet-magic $NWMAGIC)

echo "Script address:"
echo "${rewards_script}"
```

```bash
./check-script-rewards.sh 
Script address:
[
    {
        "address": "stake_test17peya46y0tymw8cq6hgdlzdlrys58acwsww4luzk0yur9vgy0xqrc",
        "delegation": "pool1qxcz3zxnye8g9ejsqslhl0ljevdx895uc80lr89ulf92gcv40ng",
        "rewardAccountBalance": 0
    }
]
```

Once a few days have passed and we have some rewards, we can withdraw them with the correct redeemer code in our `withdraw-rewards.sh` script.

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


rewards=$(cardano-cli query stake-address-info \
    --address $(cat StakingValidator.stake) \
    --testnet-magic $NWMAGIC | jq -r ".[0].rewardAccountBalance")

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat StakingValidator.addr) \
    --tx-in cf72524e68d5884b2b1fb402494cf81de60aec6fbefd610af606adcddc7e4837#0 \
    --tx-in-collateral ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1#0 \
    --withdrawal $(cat StakingValidator.stake)+${rewards} \
    --withdrawal-script-file ../../compiled/StakingValidator.plutus \
    --withdrawal-redeemer-value 2 \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file ../address/01.skey \
    --testnet-magic $NWMAGIC \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic $NWMAGIC \
    --tx-file tx.signed
```

Actually, the pool we initially delegated `pool1qxcz3zxnye8g9ejsqslhl0ljevdx895uc80lr89ulf92gcv40ng` to is not producing blocks anymore so we need to redelegate to another pool. We can write a new `redelegate.sh` script for that.

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli stake-address delegation-certificate \
  --stake-script-file ../../compiled/StakingValidator.plutus \
  --stake-pool-id pool1x9xkvkrfw6htmnflpad0z2aqsxx50f5mwkyzpylw0tlsk9z5uff \
  --out-file deleg.cert

cardano-cli transaction build \
  --testnet-magic $NWMAGIC \
  --change-address $(cat StakingValidator.addr) \
  --tx-in fbbf7a532ff30176087966c129f8fe44aa9e3462e4224e4fbfe3e162b1569ded#2 \
  --tx-in-collateral ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1#0 \
  --certificate-file deleg.cert \
  --certificate-script-file ../../compiled/StakingValidator.plutus \
  --certificate-redeemer-value 1 \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file ../address/01.skey \
  --testnet-magic $NWMAGIC \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic $NWMAGIC \
  --tx-file tx.signed
```

```bash
./redelegate.sh 
Estimated transaction fee: Lovelace 309296
Transaction successfully submitted.
```
