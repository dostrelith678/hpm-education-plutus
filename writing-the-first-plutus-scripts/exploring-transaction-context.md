# Exploring Script Context

### Introduction to Script Context

We will now take a closer look at the third argument to Plutus validator functions which is the _**script context**_. As mentioned before, the script context contains the entire transaction being validated including all its inputs and outputs, fees, and certificates.

{% hint style="info" %}
You may hear _script context_ being called _transaction context_. Don't be confused as they mean the same thing.
{% endhint %}

In Plutus, this script context corresponds to the `ScriptContext` type. The best way to explore the structure of the `ScriptContext` type is through Haddock documentation: [https://input-output-hk.github.io/plutus-apps/main/plutus-ledger/html/Ledger.html#t:ScriptContext](https://input-output-hk.github.io/plutus-apps/main/plutus-ledger/html/Ledger.html#t:ScriptContext).

{% hint style="info" %}
Not all Plutus documentation is on the [Plutus repository](https://github.com/input-output-hk/plutus). Here, we are looking at the [plutus-apps repository documentation](https://input-output-hk.github.io/plutus-apps/main/).
{% endhint %}

We can see that it consists of two fields:

1. `scriptContextTxInfo :: TxInfo`
2. `scriptContextPurpose :: ScriptPurpose`

The `scriptContextTxInfo` field is a type of `TxInfo` for which we can find further information in Haddock:	&#x20;

```haskell
txInfoInputs :: [TxInInfo]	                 -- Transaction inputs

txInfoReferenceInputs :: [TxInInfo]              -- Transaction reference inputs

txInfoOutputs :: [TxOut]	                 -- Transaction outputs

txInfoFee :: Value	                         -- The fee paid by this transaction.

txInfoMint :: Value                              -- The Value minted by this transaction.

txInfoDCert :: [DCert]	                         -- Digests of certificates included in this transaction

txInfoWdrl :: Map StakingCredential Integer	 -- Withdrawals

txInfoValidRange :: POSIXTimeRange	         -- The valid range for the transaction.

txInfoSignatories :: [PubKeyHash]	         -- Signatures provided with the transaction, attested that they all signed the tx

txInfoRedeemers :: Map ScriptPurpose             -- Redeemer

txInfoData :: Map DatumHash                      -- Datum
 
txInfoId :: TxId	                         -- Hash of the pending transaction (excluding witnesses)
```

In other words, it contains everything that we attached to the transaction when we built it.

The other field `scriptContextPurpose` is of type `ScriptPurpose` which can be one of:

```haskell
Minting    CurrencySymbol
Spending   TxOutRef
Rewarding  StakingCredential
Certifying DCert
```

### A ScriptContext Exploration Script&#x20;

Now that we have a theoretical overview of the script context, let's write a simple script utilising it in practice. It will be just a simple example with the sole purpose of exploring the use of script context inside the validator. We will create a script that validates only if the transaction attempts to create less than or exactly **three** outputs and if its valid time range is **infinite** (this is the default range applied if no valid range is specified during the transaction building stage).

#### A note on Cardano transaction validity ranges

Cardano transactions contain a  `txInfoValidRange`, which defines the range of _slots_ between which the transaction is valid. There are two layers of checking the valid range for a transaction. The first one happens when a `cardano-node` receives the transaction. The first thing the node does when considering a transaction is check its valid range - if the current slot does not fall into the valid range of the transaction, it is immediately discarded without doing anything else (including running a possible validator script in the transaction). The second layer is optional and can be specified in the validator script itself. This is done by accessing the `txInfoValidRange` from the `ScriptContext` and performing arbitrary checks against it.

#### Writing the validator

Create a new file `src/ExploringScriptContext.hs` for this validator. Our imports stay the same as in the previous scripts so copy those in. Firstly, we need to change the way we look at arguments in the `mkValidator` function. We are not interested in the `datum` and `redeemer` fields so we can ignore them now, and instead, look only at the `context` field:

```haskell
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ ctx = ...
```

We want to check two things in the validation as mentioned above, so we will create helper functions that check each one individually, `checkOutputs` and `checkRange`. The main function will check that both of these helper functions evaluate to `True`:

```haskell
mkValidator _ _ ctx =
  if checkOutputs && checkRange 
    then ()
    else error ()
```

Now we need to write the helper functions which is a bit more complicated as they have to work with the script `context`. Firstly, to even access the `context` in the structure of `ScriptContext` we explored, we have to build that structure from the third argument `ctx`, which is of type `BuiltinData`. We can do this transformation by using [`unsafeFromBuiltinData` ](https://input-output-hk.github.io/plutus-apps/main/plutus-ledger-api/html/Plutus-V2-Ledger-Api.html#v:unsafeFromBuiltinData)function on `ctx` (the difference between `unsafeFromBuiltinData` and `fromBuiltinData` is that the former will error if it fails which is faster, while the latter will return `Nothing`). This leads to our first helper function:

`valCtx = Plutus.unsafeFromBuiltinData ctx`

We now have the `valCtx` of type `ScriptContext`, so we can destructure it accordingly. Remember that it consists of two fields `scriptContextTxInfo :: TxInfo` and `scriptContextPurpose :: ScriptPurpose`. For our use case, we are only interested in the `TxInfo`, and we can get it with our second helper function:

`info = Plutus.scriptContextTxInfo valCtx`

The `info` is now of type `TxInfo`, which contains all the information we need for our validation. Specifically, we are interested in the fields `txInfoOutputs :: [TxOut]` and `txInfoValidRange :: POSIXTimeRange`. To create the validation logic for the number of UTxOs we can simply use the `length` function to count the number of UTxOs in the transaction since `txInfoOutputs` is a `[List]`. We also combine that with the Plutus Prelude `traceIfFalse` function to provide us with debugging info in case of invalid transactions:

`checkOutputs = traceIfFalse "4 or more outputs in tx!" $ length (Plutus.txInfoOutputs info) <= 3`

The final part of our validator logic is to check that the submitted transaction's valid range is infinite. We first destruct the `txInfoValidRange` field of `TxInfo`, and compare it with the `Plutus.always` pre-defined time interval which corresponds to the infinite time range. Again, we use the `traceIfFalse` as before:

`checkRange = traceIfFalse "Tx does not have infinite range!" $ Plutus.txInfoValidRange info == Plutus.always`

Now we have all the pieces of the validation done and the full validator looks like this:

```haskell
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ ctx =
  if checkOutputs P.&& checkRange 
    then ()
    else error ()
  where
    valCtx = Plutus.unsafeFromBuiltinData ctx
    info = Plutus.scriptContextTxInfo valCtx
    checkOutputs = traceIfFalse "4 or more outputs in tx!" $ length (Plutus.txInfoOutputs info) <= 3
    checkRange = traceIfFalse "Tx does not have infinite range!" $ Plutus.txInfoValidRange info == Plutus.always
```

The rest of the functions for serialising and writing the script stay the same as with previous scripts (`validator`, `script`, `scriptShortBs`, `scriptSerialised`). We just need to change the compilation result destination in the `writeSerialisedScript` function:

```haskell
writeSerialisedScript :: IO (Either (FileError ()) ())
writeSerialisedScript = writeFileTextEnvelope "compiled/ExploringScriptContext.plutus" Nothing scriptSerialised
```

If we try to compile this script as is, we will get an error:

```haskell
Couldn't match expected type ‘BuiltinString’
    with actual type ‘[ghc-prim-0.6.1:GHC.Types.Char]’
    • In the first argument of ‘traceIfFalse’, namely
        ‘"Tx does not have infinite range!"’
```

The `traceIfFalse` function is expecting a `BuiltinString` but we are passing it a regular Haskell string. We can solve this with the `stringToBuiltinString` function from [`PlutusTx.Builtins.Class`](https://input-output-hk.github.io/plutus/master/plutus-tx/html/PlutusTx-Builtins-Class.html#v:stringToBuiltinString). Add an import for this function and apply it to the trace message string in the `checkOutputs` and `checkRange` functions:

```haskell
import PlutusTx.Builtins.Class (stringToBuiltinString)

...

checkOutputs = traceIfFalse (stringToBuiltinString "4 or more outputs in tx!")
    $ length (Plutus.txInfoOutputs txInfo) <= 3

checkRange = traceIfFalse (stringToBuiltinString "Tx does not have infinite range!")
    $ Plutus.txInfoValidRange txInfo == Plutus.always
    
...
```

The module will compile okay now. However, there is another way to get through this issue, with a GHC extension called `OverloadedStrings`. This extension lets GHC try to transform regular Haskell strings into the required types. We can remove the `PlutusTx.Builtins.Class` import and revert our functions to just using a normal Haskell string as before, with the addition of this extension to the top of the file.

```haskell
...
{-# LANGUAGE OverloadedStrings #-}
...
```

{% hint style="info" %}
Every time we want to automatically load a module we write when launching a `cabal repl`, we can add them to our `.cabal` file in the `exposed-modules` field.

```haskell
-- hpm-validators.cabal

...
library
    hs-source-dirs:       src
    exposed-modules:      SimplestSuccess
                        , GuessingGame
                        , ExploringScriptContext
                        , Helpers.Utils
...
```
{% endhint %}

### Testing the validator

Compile the validator as before by launching a `cabal repl` and calling the write function.

```
Prelude> ExploringScriptContext.writeSerialisedScript
Right ()
```

Firstly, create a script address for the validator. We will use `src/testnet/ExploringScriptContext` as the testing directory.

```bash
# testnet/ExploringScriptContext/create-script-address.sh

#!/usr/bin/env bash

NWMAGIC=2 # preview testnet

# Build script address
cardano-cli address build \
--payment-script-file ../../compiled/ExploringScriptContext.plutus \
--testnet-magic $NWMAGIC \
--out-file ExploringScriptContext.addr

echo "Script address: $(cat ExploringScriptContext.addr)"
```

As before, we need a way to check the UTxOs. It's likely that this script address will not have any UTxOs on it when checked.&#x20;

```bash
# testnet/ExploringScriptContext/check-utxos.sh

#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

funds_normal=$(cardano-cli query utxo \
--address $(cat ../address/01.addr) \
--testnet-magic $NWMAGIC)

funds_script=$(cardano-cli query utxo \
--address $(cat ExploringScriptContext.addr) \
--testnet-magic $NWMAGIC)

echo "Normal address:"
echo "${funds_normal}"

echo "Script address:"
echo "${funds_script}"
```

```bash
./check-utxos.sh

Normal address:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5dc5111e257f8e68b0978c9619e57bbb12d365c0ec45d879115866bb674156ae     0        1826915 lovelace + TxOutDatumNone
8fc7a4fda80ad379811c44591a9fc4bae7fcd9a4ddda1574df910adc0143ac7a     1        9995661298 lovelace + TxOutDatumNone
ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1     0        1829006 lovelace + TxOutDatumNone
Script address:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```

We need to fund the script with some tADA before testing it. Create a `send-funds-to-script.sh` script.

<pre class="language-bash"><code class="lang-bash"># testnet/ExploringScriptContext/send-funds-to-script.sh

<strong>#!/usr/bin/env bash
</strong>
NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/01.addr) \
    --tx-in 8fc7a4fda80ad379811c44591a9fc4bae7fcd9a4ddda1574df910adc0143ac7a#1 \
    --tx-out $(cat ExploringScriptContext.addr)+10000000 \
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
</code></pre>

```bash
./send-funds-to-script.sh
Estimated transaction fee: Lovelace 169109
Transaction successfully submitted.
```

The funds are on the script, and we want to start actually testing the validator logic. Let's create the two transactions that should _fail_. These two cases are:

1. The transaction tries to spend the script UTxO by creating _4 or more outputs_.
2. The transaction validity range is _not_ infinite.

Let's start with the validity range case. We can query the tip of the chain with `cardano-cli` to get the current slot, and we can add an `--invalid-before` argument to the `transaction build` command with any slot before the current tip. This will define the transaction validity range from that slot to infinity. This will make the transaction pass the first validity range check that the node performs, as the current slot will fall into the transactions' valid range, but our logic inside the validator regarding the transaction range should fail.&#x20;

```bash
cardano-cli query tip --testnet-magic 2
{
    "block": 1099151,
    "epoch": 290,
    "era": "Babbage",
    "hash": "31323e6507cf03e5668ab714be923535b01aee73b04ebb175c9c744472d573a4",
    "slot": 25086665,  # This is our current chain tip
    "slotInEpoch": 30665,
    "slotsToEpochEnd": 55735,
    "syncProgress": "100.00"
}
```

Select the script UTxO for the `--tx-in` and create a valid number of outputs (in the below example two).

<pre class="language-bash"><code class="lang-bash"><strong># testnet/ExploringScriptContext/spend-script-funds-invalid-range.sh
</strong>
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/01.addr) \
    --invalid-before 25086665 \
    --tx-in e4a68d9cb4e58d47085c74426441445eaa30c2a60a9d102217d27ec0b0664db8#0 \
    --tx-in-script-file ../../compiled/ExploringScriptContext.plutus \
    --tx-in-datum-file ../../compiled/assets/unit.json \
    --tx-in-redeemer-file ../../compiled/assets/unit.json \
    --tx-in-collateral ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1#0 \
    --tx-out $(cat ../address/01.addr)+1500000 \
    --tx-out $(cat ../address/01.addr)+1500000 \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file ../address/01.skey \
    --testnet-magic $NWMAGIC \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic $NWMAGIC \
    --tx-file tx.signed
</code></pre>

Trying to build this transaction gives us a nice error message under `Script debugging logs:` since we used `traceIfFalse`.

```bash
./spend-script-funds-invalid-range.sh

Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 0 (in ascending order of the TxIds) failed with: 
The Plutus script evaluation failed: An error has occurred:  User error:
The machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.
Script debugging logs: Tx does not have infinite range!
```

Okay, now let's see if the script fails when we try to create an invalid number of outputs. Create a `spend-script-utxo-invalid-utxos.sh` script to test this. This time, we omit the `--invalid-before` as we want the transaction to have infinite range to make sure the failure is from the number of outputs trying to be created.

```bash
# testnet/ExploringScriptContext/spend-script-utxo-invalid-utxos.sh

#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../normal_address/01.addr) \
    --tx-in 1886d7f5af349199fd8fb74f37372b473c888cd40f19f5885ba470d8e25fb571#0 \
    --tx-in-script-file exploringContext.plutus \
    --tx-in-datum-file ../assets/unit.json \
    --tx-in-redeemer-file ../assets/unit.json \
    --tx-in-collateral 45337a0fb353dadc7e31f865378885207553b4471814384421e0fa1607271bf6#1 \
    --tx-out $(cat ../normal_address/01.addr)+1500000 \
    --tx-out $(cat ../normal_address/01.addr)+1500000 \
    --tx-out $(cat ../normal_address/01.addr)+1500000 \
    --tx-out $(cat ../normal_address/01.addr)+1500000 \
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

Again, we get a nice error message explaining the failure.

```bash
./spend-script-funds-invalid-utxos.sh

Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 0 (in ascending order of the TxIds) failed with: 
The Plutus script evaluation failed: An error has occurred:  User error:
The machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.
Script debugging logs: 4 or more outputs in tx!
```

The only thing left to test is whether a valid transaction will work. That is one with infinite range and less than four outputs. Let's create a `spend-script-funds.sh` to test it. The below example has just one output to be created specified via `--change-address`. With no other outputs present, all the tADA will go to this address after the transaction fees are paid.&#x20;

<pre class="language-bash"><code class="lang-bash"># testnet/ExploringScriptContext/spend-script-utxo.sh

<strong>#!/usr/bin/env bash
</strong>
NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/01.addr) \
    --tx-in e4a68d9cb4e58d47085c74426441445eaa30c2a60a9d102217d27ec0b0664db8#0 \
    --tx-in-script-file ../../compiled/ExploringScriptContext.plutus \
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
</code></pre>

With this transaction, we can successfully spend the UTxO from the script address.

```bash
./spend-script-funds.sh

Estimated transaction fee: Lovelace 350224
Transaction successfully submitted.
```

