# Exploring Transaction Context

We will now take a closer look at the third argument to Plutus validator functions which is the _**transaction context**_. As mentioned before, the transaction context contains the entire transaction being validated including all its inputs and outputs, fees, and certificates.

In Plutus, this transaction context corresponds to the `ScriptContext` type. The best way to explore the structure of the `ScriptContext` type is through Haddock documentation: [https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V2-Ledger-Contexts.html#t:ScriptContext](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V2-Ledger-Contexts.html#t:ScriptContext) (focusing on V2 here, but differences with V1 are minor).

We can see that it consists of two fields:

1. `scriptContextTxInfo :: TxInfo`
2. `scriptContextPurpose :: ScriptPurpose`

The `scriptContextTxInfo` field is a type of `TxInfo` for which we can find further information in Haddock:

```haskell
txInfoInputs :: [TxInInfo]  -- Transaction inputs

txInfoOutputs :: [TxOut]  -- Transaction outputs

txInfoFee :: Value  -- The fee paid by this transaction.

txInfoMint :: Value  -- The Value minted by this transaction.

txInfoDCert :: [DCert]  -- Digests of certificates included in this transaction

txInfoWdrl :: Map StakingCredential Integer   -- Withdrawals

txInfoValidRange :: POSIXTimeRange  -- The valid range for the transaction.

txInfoSignatories :: [PubKeyHash]  -- Signatures provided with the transaction, attested that they all signed the tx

txInfoRedeemers :: Map ScriptPurpose Redeemer

txInfoData :: Map DatumHash Datum

txInfoId :: TxId  -- Hash of the pending transaction (excluding witnesses)
```

In other words, it contains everything that we attached to the transaction when we built it.

The other field `scriptContextPurpose` is of type `ScriptPurpose` which can be one of:

```haskell
Minting CurrencySymbol
Spending TxOutRef
Rewarding StakingCredential
Certifying DCert
```

Now that we have a theoretical overview of the script context, let's write a simple script utilising it in practice. It will be just a simple example with the sole purpose of exploring the use of script context inside the validator. We will create a script that validates only if the transaction attempts to create less than or exactly **ten** outputs and if its valid time range is **infinite** (this is the default range applied if no valid range is specified during the transaction building stage).

#### Note: might need an understanding time in Plutus section here

Firstly, we need to change the way we look at arguments in the `mkValidator` function. We are not interested in the `datum` and `redeemer` fields so we can ignore them now, and instead, look only at the `context` field:

```haskell
mkValidator :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
mkValidator _ _ ctx = ...
```

We want to check two things in the validation as mentioned above, so we will create helper functions that check each one individually, `checkOutputs` and `checkRange`. The main function will check that both of these helper functions evaluate to `True`:

```haskell
mkValidator _ _ ctx =
  if checkOutputs P.&& checkRange 
    then ()
    else P.error ()
```

Now we need to write the helper functions which is a bit more complicated as they have to work with the transaction `context`. Firstly, to even access the `context` in the structure of `ScriptContext` we explored, we have to build that structure from the third argument `ctx`, which is of type `P.BuiltinData`. We can do this transformation by using [`unsafeFromBuiltinData` ](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V2-Ledger-Api.html#t:UnsafeFromData)function on `ctx` (the difference between `unsafeFromBuiltinData` and `fromBuiltinData` is that the former will error if it fails which is faster, while the latter will return `Nothing`). This leads to our first helper function:

`valCtx = Plutus.unsafeFromBuiltinData ctx`

We now have the `valCtx` of type `ScriptContext`, so we can destructure it accordingly. Remember that it consists of two fields `scriptContextTxInfo :: TxInfo` and `scriptContextPurpose :: ScriptPurpose`. For our use case, we are only interested in the `TxInfo`, and we can get it with our second helper function:

`info = Plutus.scriptContextTxInfo valCtx`

The `info` is now of type `TxInfo`, which contains all the information we need for our validation. Specifically, we are interested in the fields `txInfoOutputs :: [TxOut]` and `txInfoValidRange :: POSIXTimeRange`. To create the validation logic for the number of UTxOs we can simply use the `length` function to count the number of UTxOs in the transaction since `txInfoOutputs` is a `[List]`. We also combine that with the Plutus Prelude `traceIfFalse` function to provide us with debugging info in case of invalid transactions:

`checkOutputs = P.traceIfFalse "Not less than 10 outputs!" P.$ P.length (Plutus.txInfoOutputs info) P.<= 10`

The final part of our validator logic is to check that the submitted transaction's valid range is infinite. We first destruct the `txInfoValidRange` field of `TxInfo`, and compare it with the `Plutus.always` pre-defined time interval which corresponds to the infinite time range. Again, we use the `traceIfFalse` as before:

`checkRange = P.traceIfFalse "Not infinite range!" P.$ Plutus.txInfoValidRange info P.== Plutus.always`

Now we have all the pieces of the validation done and the full validator looks like this:

```haskell
mkValidator :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
mkValidator _ _ ctx =
  if checkOutputs P.&& checkRange 
    then ()
    else P.error ()
  where
    valCtx = Plutus.unsafeFromBuiltinData ctx
    info = Plutus.scriptContextTxInfo valCtx
    checkOutputs = P.traceIfFalse "Not less than 10 outputs!" P.$ P.length (Plutus.txInfoOutputs info) P.<= 10
    checkRange = P.traceIfFalse "Not infinite range!" P.$ Plutus.txInfoValidRange info P.== Plutus.always
```

The rest is just a simple renaming of functions according to our new module name.

Helper scripts for testing:

1\) `create-script-address.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet

# Build script address
cardano-cli address build \
--payment-script-file exploringContext.plutus \
--testnet-magic $NWMAGIC \
--out-file exploringContext.addr

echo "Script address: $(cat exploringContext.addr)"
```

2\) `check-utxos.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

funds_normal=$(cardano-cli query utxo \
--address $(cat ../normal_address/01.addr) \
--testnet-magic $NWMAGIC)

funds_script=$(cardano-cli query utxo \
--address $(cat exploringContext.addr) \
--testnet-magic $NWMAGIC)

echo "Normal address:"
echo "${funds_normal}"
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
    --tx-in 61931f9f949bf8d1ef1e8b6b004fb7ea4cd80db3319247e2355ed08399af94f4#0 \
    --tx-out $(cat exploringContext.addr)+20000000 \
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

4\) `spend-script-utxo-invalid-range.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../normal_address/01.addr) \
    --invalid-before 6093433 \
    --tx-in 1886d7f5af349199fd8fb74f37372b473c888cd40f19f5885ba470d8e25fb571#0 \
    --tx-in-script-file exploringContext.plutus \
    --tx-in-datum-file ../assets/unit.json \
    --tx-in-redeemer-file ../assets/unit.json \
    --tx-in-collateral 45337a0fb353dadc7e31f865378885207553b4471814384421e0fa1607271bf6#1 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
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

5\) `spend-script-utxo-invalid-utxos.sh`

```bash
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
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
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

6\) `spend-script-utxo.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket


cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../normal_address/01.addr) \
    --tx-in 45337a0fb353dadc7e31f865378885207553b4471814384421e0fa1607271bf6#0 \
    --tx-in-script-file exploringContext.plutus \
    --tx-in-datum-file ../assets/unit.json \
    --tx-in-redeemer-file ../assets/unit.json \
    --tx-in-collateral 45337a0fb353dadc7e31f865378885207553b4471814384421e0fa1607271bf6#1 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
    --tx-out $(cat ../normal_address/01.addr)+1300000 \
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
