# Another Shared Wallet Script

The previous example used the `POSIXTime` type as the script parameter. For the following example, we will look at custom-defined parameter types as there is some extra work we need to do in that case. We will transform the _SharedWallet_ script from earlier to be parameterised instead of relying on the UTxO datum. That means that either of the trusted parties can spend any UTxO sitting at the script address, regardless of what the datum associated is.

### Writing the validator

Firstly, we create the same type as before, this time calling it `sharedParam`:

```haskell
data SharedWalletParam = SharedWalletParam {
    wallet1 :: Plutus.PubKeyHash,
    wallet2 :: Plutus.PubKeyHash
}
```

As before, we need to make our parameter type an instance of `ToData`/`FromData` via `PlutusTx.unstableMakeIsData`:

```haskell
PlutusTx.unstableMakeIsData ''SharedWalletParam
```

In addition, when defining custom parameter types, we need to make them an instance of the [`Lift` class](https://input-output-hk.github.io/plutus/master/plutus-tx/html/PlutusTx.html#t:Lift). This allows the type to be lifted into so-called _Plutus IR_ (intermediate representation), which is then handled further to Plutus Core, but essentially, we just need this to allow the compiler to compile our type to Plutus Core. This is done with the `PlutusTx.makeLift` function that automatically derives an instance for us:

```haskell
PlutusTx.makeLift ''SharedWalletParam
```

Note that `PubKeyHash` type is an instance of both `ToData`/`FromData` and `Lift`, but our defined type `SharedParam`  which contains two `PubKeyHash` fields is not.

The rest is pretty much the same as in the [typed validator version](../typed-validators/a-shared-wallet-script.md). We need to create a `ValidatorTypes` instance, but since we are using a parameter that will bake our `sharedParam` inside the script, we do not need datum and redeemer:

```haskell
data SharedWalletParamValidator
instance Scripts.ValidatorTypes SharedWalletParamValidator
    -- default types for datum and redeemer are ()
```

Our `mkValidator` function still for valid signatures in the transaction context, except this time the `PubKeyHash`es are deconstructed from the `SharedParam` argument:

```haskell
{-# INLINEABLE mkValidator #-}
mkValidator :: SharedWalletParam -> () -> () -> Plutus.ScriptContext -> Bool
mkValidator swp () () ctx = checkSignature1 || checkSignature2
  where
    info :: Plutus.TxInfo
    info = Plutus.scriptContextTxInfo ctx

    signature1 :: PubKeyHash
    signature1 = wallet1 swp

    signature2 :: PubKeyHash
    signature2 = wallet2 swp

    checkSignature1 :: Bool
    checkSignature1 = traceIfFalse "signature 1 missing" $ txSignedBy info signature1

    checkSignature2 :: Bool
    checkSignature2 = traceIfFalse "signature 2 missing" $ txSignedBy info signature2
```

As with the [parameterised deadline script example](a-deadline-script.md), we use the `PSU.V2.mkTypedValidatorParam` to define a function that accepts the parameter to compile the validator.

```haskell
typedValidator :: SharedWalletParam -> PSU.V2.TypedValidator SharedWalletParamValidator
typedValidator =
  PSU.V2.mkTypedValidatorParam @SharedWalletParamValidator
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = PSU.mkUntypedValidator

validator :: SharedWalletParam -> Plutus.Validator
validator = PSU.V2.validatorScript . typedValidator

script :: SharedWalletParam -> Plutus.Script
script = Plutus.unValidatorScript . validator
```

Besides the `DataKind` extension we enabled when working with typed validators, for parameterised validators we also need to enable `ScopedTypeVariables` and `MultiParamTypeClasses`.

```haskell
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
```

Finally, we define the functions to serialise and write the script file:

```haskell
sharedWalletParamShortBs :: SharedWalletParam -> SBS.ShortByteString
sharedWalletParamShortBs swp = SBS.toShort . LBS.toStrict $ serialise $ script swp

scriptSerialised :: SharedWalletParam -> PlutusScript PlutusScriptV2
scriptSerialised swp = PlutusScriptSerialised $ sharedWalletParamShortBs swp

writeSerialisedScript :: SharedWalletParam -> IO (Either (FileError ()) ())
writeSerialisedScript swp = writeFileTextEnvelope "compiled/SharedWalletParam.plutus" Nothing $ scriptSerialised swp
```

In order to compile this validator, we need to provide it with a parameter of type `SharedWalletParam`. Again, we can export this type and its constructor by adding it to the module interface.

```haskell
module SharedWalletParam
  (
    scriptSerialised,
    writeSerialisedScript,
    SharedWalletParam (..)
  )
where
...
```

Now, when we load the module in a REPL, we can construct a valid `SharedWalletParam` type. Let's do that by specifying the two public key hashes from our addresses as we did with the [typed version of the shared wallet validator](../typed-validators/a-shared-wallet-script.md). When we have our parameter for the script, we can pass it to the `writeSerialisedScript` function.

<pre class="language-haskell"><code class="lang-haskell"><strong>Prelude> :l SharedWalletParam 
</strong>[1 of 1] Compiling SharedWalletParam ( src/SharedWalletParam.hs, /home/plutus/hpm-plutus/hpm-validators/dist-newstyle/build/x86_64-linux/ghc-8.10.7/hpm-validators-0.1.0.0/build/SharedWalletParam.o )
Ok, one module loaded.
Prelude SharedWalletParam> :set -XOverloadedStrings
Prelude SharedWalletParam> myParam = SharedWalletParam "a5d318dadfb52eeffb260ae097f846aea0ca78e6cc4fe406d4ceedc0" "1b1e5895b03302b248e8c459817bab49471c4013a0806ac52cb73f9b"
Prelude SharedWalletParam> writeSerialisedScript myParam
Right ()
</code></pre>

### Testing the validator

As always, we start off by creating the script address in `testnet/SharedWalletParam/create-script-address.sh`.

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet

# Build script address
cardano-cli address build \
--payment-script-file ../../compiled/SharedWalletParam.plutus \
--testnet-magic $NWMAGIC \
--out-file SharedWalletParam.addr

echo "Script address: $(cat SharedWalletParam.addr)"
```

```bash
./create-script-address.sh 
Script address: addr_test1wqjvw5a437sarsrexzezx04cl4rtxsd895cx4s3ncn7qg6gevx55f
```

Next, let's get an overview of the UTxOs with `check-utxos.sh`.

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

funds_normal1=$(cardano-cli query utxo \
--address $(cat ../address/01.addr) \
--testnet-magic $NWMAGIC)

funds_normal2=$(cardano-cli query utxo \
--address $(cat ../address/02.addr) \
--testnet-magic $NWMAGIC)

funds_script=$(cardano-cli query utxo \
--address $(cat SharedWalletParam.addr) \
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

```bash
./check-utxos.sh 
Normal address1:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5dc5111e257f8e68b0978c9619e57bbb12d365c0ec45d879115866bb674156ae     0        1826915 lovelace + TxOutDatumNone
6924903343947231af6c56a5d2d25b3256a513dee77e7966b6f8a47b09913188     2        9844804665 lovelace + TxOutDatumNone
ede24e9e40ca82830c75d827b5c3b090132c1afaebd3a4256655fb5d2382474a     0        9649776 lovelace + TxOutDatumNone
ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1     0        1829006 lovelace + TxOutDatumNone

Normal address2:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
59590fab00fb430d205151c59ca7e00af38e9945d778abdae6897f368aa39591     0        19682109 lovelace + TxOutDatumNone
667f81c9a89946d83f5975d9d97534df42be85a5a5aa1161b7af0ecb3d6592d0     0        19673177 lovelace + TxOutDatumNone

Script address:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

We can now send some funds to the script. Remember, we don't care about the datum as the script was parameterised at compilation so it knows about our public key hashes. However, we always have to attach a datum to any script UTxO or it will be _unspendable_, so we can attach our `unit.json` in the transaction constructed by the `send-funds-to-script.sh` script.

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/01.addr) \
    --tx-in 6924903343947231af6c56a5d2d25b3256a513dee77e7966b6f8a47b09913188#2 \
    --tx-out $(cat SharedWalletParam.addr)+20000000 \
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
Estimated transaction fee: Lovelace 169109
Transaction successfully submitted.

./check-utxos.sh 

...

Script address:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
abe233c49d4162d886a0e38e2ed03739cc9feb0b5f38eb54d8417eb9821f039b     0        20000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
```

We see the UTxO at the script address so we can try spending it by simply signing the transaction with one of our private keys. Let's use the key from the `02.addr` and use one of its UTxOs for collateral as well in `spend-script-funds.sh`.

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/02.addr) \
    --tx-in abe233c49d4162d886a0e38e2ed03739cc9feb0b5f38eb54d8417eb9821f039b#0 \
    --tx-in-script-file ../../compiled/SharedWalletParam.plutus \
    --tx-in-datum-file ../../compiled/assets/unit.json \
    --tx-in-redeemer-file ../../compiled/assets/unit.json \
    --tx-in-collateral 667f81c9a89946d83f5975d9d97534df42be85a5a5aa1161b7af0ecb3d6592d0#0 \
    --required-signer-hash 1b1e5895b03302b248e8c459817bab49471c4013a0806ac52cb73f9b \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file ../address/02.skey \
    --testnet-magic $NWMAGIC \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic $NWMAGIC \
    --tx-file tx.signed
```

And we can successfully spend the funds from the script using the `02.addr` signing key!

```bash
./spend-script-funds.sh 
Estimated transaction fee: Lovelace 315499
Transaction successfully submitted.
```
