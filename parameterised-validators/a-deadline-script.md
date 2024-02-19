# A Deadline Script

Let's write a parameterised validator that is a simple deadline function. Before the deadline, all transactions are validated, and after the deadline, all are invalidated. The deadline will be our parameter for the validator that is baked in the script. This deadline needs to be expressed in the `Plutus.POSIXTime` type, which is measured as the number of _**milliseconds**_ since `1970-01-01T00:00:00Z`. Plutus always works with POSIXTime, while the Cardano chain works with `slots`. The reason for this discrepancy is that the slot length of the chain is a parameter that can change over time.

### Writing the validator

Let's create a new file `DeadlineParam.hs`, define the module `DeadlineParam` and add the same imports as before. We can start off by creating the simple `DeadlineValidator` type for the validator. Since we don't need the datum and redeemer in this case, we can omit their definitions to apply the default `()` type for both of them.

```haskell
data Deadline
instance Scripts.ValidatorTypes Deadline
    -- default types for datum and redeemer are ()
```

Since `POSIXTime` already is an instance of `ToData`, we don't need to worry about lifting values right now and can go straight to the `mkValidator` function. The signature will be:

```haskell
mkValidator :: Plutus.POSIXTime -> () -> () -> Plutus.ScriptContext -> Bool
```

Inside the function, we want to check that the `txInfoValidRange` is contained in its entirety in the interval of negative infinity to the deadline.

{% hint style="warning" %}
Important to note that the _**ENTIRE**_ transaction validity range must fall into this interval because if the positive end of the valid range would go over the deadline, the transaction would be validated after the deadline even if the majority of the valid range is before the deadline!
{% endhint %}

We will need to use some functions from the [`Interval` module](https://intersectMBO.github.io/plutus/master/plutus-ledger-api/html/PlutusLedgerApi-V1-Interval.html) to determine that, namely `contains` and `to`.&#x20;

```haskell
import Plutus.V1.Ledger.Interval (contains, to)

...

mkValidator :: Plutus.POSIXTime -> () -> () -> Plutus.ScriptContext -> Bool
mkValidator deadline _ _ ctx =
  traceIfFalse "Invalid tx range" $ to deadline `contains` txRange
    where
      info :: Plutus.TxInfo
      info = Plutus.scriptContextTxInfo ctx

      txRange :: Plutus.POSIXTimeRange
      txRange = Plutus.txInfoValidRange info
```

We destructure our `ctx` as before to get to the `txInfoValidRange` field. Then we state the predicate ``to deadline `contains` txRange`` which reads as _"the interval from negative infinity to our deadline (inclusive) contains the entire interval of the transaction validity range"_.

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
scriptShortBs :: Plutus.POSIXTime -> SBS.ShortByteString
scriptShortBs deadline = SBS.toShort . LBS.toStrict $ serialise $ script deadline

scriptSerialised :: Plutus.POSIXTime -> PlutusScript PlutusScriptV2
scriptSerialised deadline = PlutusScriptSerialised $ scriptShortBs deadline

writeSerialisedScript :: Plutus.POSIXTime -> IO (Either (FileError ()) ())
writeSerialisedScript deadline = writeFileTextEnvelope "compiled/DeadlineParam.plutus" Nothing $ scriptSerialised deadline
```

That's it! We can now load our new module in `cabal repl` and write the script to a file. Of course, we need to provide the actual `POSIXTime` parameter to specify the deadline. For testing, we can get the current time with `date %s` and add 20 minutes to it to give us time for testing:

{% hint style="info" %}
Remember, we are always working with POSIXTime in _miliseconds_ when it comes to Plutus.&#x20;
{% endhint %}

```bash
expr $(date +%s000) + 1200000
1692181336000
```

That gives us our deadline in POSIXTime one hour from now. Every transaction we submit until then will be validated!

`ghci> writeSerialisedScript 1692181336000`

### Testing the validator

As always, we create the script address first in `testnet/DeadlineParam/create-script-address.sh`.

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

```bash
./create-script-address.sh 
Script address: addr_test1wqc0caz44aluw7wcsxct7annp680k2ucklv6r8vgzwqnvnsxd04jf
```

Create the `check-utxos.sh` for this script. Let's print out UTxOs for both of our normal addresses.

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
--address $(cat DeadlineParam.addr) \
--testnet-magic $NWMAGIC)

echo "Normal address 1:"
echo "${funds_normal1}"

echo "Normal address 2:"
echo "${funds_normal2}"

echo "Script address:"
echo "${funds_script}"
```

```bash
./check-utxos.sh 
Normal address 1:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5dc5111e257f8e68b0978c9619e57bbb12d365c0ec45d879115866bb674156ae     0        1826915 lovelace + TxOutDatumNone
ede24e9e40ca82830c75d827b5c3b090132c1afaebd3a4256655fb5d2382474a     0        9649776 lovelace + TxOutDatumNone
ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1     0        1829006 lovelace + TxOutDatumNone
f455f1d31f2de72a25ccfb3874ca7702401297bb72ceba8625773dfb348d2bc5     2        9884977118 lovelace + TxOutDatumNone
Normal address 2:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
59590fab00fb430d205151c59ca7e00af38e9945d778abdae6897f368aa39591     0        19682109 lovelace + TxOutDatumNone
Script address:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

Time to send some funds to the script. Let's create two outputs, one that we intend to spend before the deadline and another to test that we cannot spend it after the deadline. Don't forget to attach a datum to both of these or they will be unspendable in any case!

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/01.addr) \
    --tx-in f455f1d31f2de72a25ccfb3874ca7702401297bb72ceba8625773dfb348d2bc5#2 \
    --tx-out $(cat DeadlineParam.addr)+20000000 \
    --tx-out-datum-embed-file ../../compiled/assets/unit.json \
    --tx-out $(cat DeadlineParam.addr)+20000000 \
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

After our transaction is processed, we can check the UTxO balance again and see the two UTxOs at the script address.

```bash
./send-funds-to-script.sh 
Estimated transaction fee: Lovelace 172453
Transaction successfully submitted.

./check-utxos.sh 
Normal address 1:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5dc5111e257f8e68b0978c9619e57bbb12d365c0ec45d879115866bb674156ae     0        1826915 lovelace + TxOutDatumNone
6924903343947231af6c56a5d2d25b3256a513dee77e7966b6f8a47b09913188     2        9844804665 lovelace + TxOutDatumNone
ede24e9e40ca82830c75d827b5c3b090132c1afaebd3a4256655fb5d2382474a     0        9649776 lovelace + TxOutDatumNone
ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1     0        1829006 lovelace + TxOutDatumNone
Normal address 2:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
59590fab00fb430d205151c59ca7e00af38e9945d778abdae6897f368aa39591     0        19682109 lovelace + TxOutDatumNone
Script address:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
6924903343947231af6c56a5d2d25b3256a513dee77e7966b6f8a47b09913188     0        20000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
6924903343947231af6c56a5d2d25b3256a513dee77e7966b6f8a47b09913188     1        20000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
```

Time to successfully spend one of the outputs before the deadline. We need to set the correct valid range for this transaction (remember the default is infinite which would be invalidated by our validator). We can get the current slot of the chain with:

```bash
cardano-cli query tip --testnet-magic 2 --socket-path $CNODE_HOME/sockets/node0.socket
{
    "block": 1117738,
    "epoch": 295,
    "era": "Babbage",
    "hash": "f3c6f5cabd28845159c1044e4dffb4b7a18170352fe4d79671fc0181aca1e2be",
    "slot": 25524308, # This is our current slot
    "slotInEpoch": 36308,
    "slotsToEpochEnd": 50092,
    "syncProgress": "100.00"
}
```

Using the `--invalid-hereafter` option in the `transaction build` command, we can set the upper limit of the transaction validity range to the current slot plus some time to allow the transaction to be processed, but not passed the deadline. For example, our current slot is `25524308` and we will add 300 seconds to it for the transaction to make it `25524608` in our `spend-script-funds.sh` for this validator:

{% hint style="info" %}
Remember, we are always working with _slots_ when it comes to `cardano-cli` or `cardano-node`.
{% endhint %}

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/02.addr) \
    --invalid-hereafter 25524608 \
    --tx-in 6924903343947231af6c56a5d2d25b3256a513dee77e7966b6f8a47b09913188#0 \
    --tx-in-script-file ../../compiled/DeadlineParam.plutus \
    --tx-in-datum-file ../../compiled/assets/unit.json \
    --tx-in-redeemer-file ../../compiled/assets/unit.json \
    --tx-in-collateral ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1#0 \
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

Running `spend-script-funds.sh` will submit the transaction successfully and the script UTxO will be spent with the change sent to our `02.addr`.&#x20;

```bash
./spend-script-funds.sh 
Estimated transaction fee: Lovelace 326823
Transaction successfully submitted.
```

To test that we cannot just spend a script UTxO with any `--invalid-hereafter` option, let's try to submit a transaction in which the right side of the validity range falls after the deadline. To make sure it is after the deadline, we can simply add one hour to the current slot, since we know that will be past the deadline. So instead of `25524608`, let's use `25528208` (`25524608 + 3600`). We can call this file `spend-script-funds-past-deadline.sh`. We also have to specify the other UTxO on the script address, since we already spent the first one.

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/02.addr) \
    --invalid-hereafter 25528208 \
    --tx-in 6924903343947231af6c56a5d2d25b3256a513dee77e7966b6f8a47b09913188#1 \
    --tx-in-script-file ../../compiled/DeadlineParam.plutus \
    --tx-in-datum-file ../../compiled/assets/unit.json \
    --tx-in-redeemer-file ../../compiled/assets/unit.json \
    --tx-in-collateral ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1#0 \
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

Attempting to build this transaction will fail instantly since the validator will run locally and invalidate the transaction with:

```bash
./spend-script-funds-past-deadline.sh
Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 0 (in ascending order of the TxIds) failed with: 
The Plutus script evaluation failed: An error has occurred:  User error:
The machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.
Script debugging logs: Invalid tx range
```
