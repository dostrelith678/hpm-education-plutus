# Exploring tx context

We will now take a closer look at the third argument to Plutus validator functions which is the *transaction context*.As we have already stated, the transaction context contains the entire transaction being validated including all its inputs and outputs, fees, certificates etc...

In Plutus, this transaction context corresponds to the `ScriptContext` type.
The best way to explore the structure of the `ScriptContext` type is through Haddock documentation: https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V2-Ledger-Contexts.html#t:ScriptContext (focusing on V2 here, but differences with V1 are minor).

We can see that it consists of two fields:
1) `scriptContextTxInfo :: TxInfo`
2) `scriptContextPurpose :: ScriptPurpose`

The `scriptContextTxInfo` field is a type of `TxInfo` for which we can find further information in Haddock:
```
txInfoInputs :: [TxInInfo]  # Transaction inputs

txInfoOutputs :: [TxOut]  # Transaction outputs

txInfoFee :: Value  # The fee paid by this transaction.

txInfoMint :: Value  # The Value minted by this transaction.

txInfoDCert :: [DCert]  # Digests of certificates included in this transaction

txInfoWdrl :: Map StakingCredential Integer   # Withdrawals

txInfoValidRange :: POSIXTimeRange  # The valid range for the transaction.

txInfoSignatories :: [PubKeyHash]  # Signatures provided with the transaction, attested that they all signed the tx

txInfoRedeemers :: Map ScriptPurpose Redeemer

txInfoData :: Map DatumHash Datum

txInfoId :: TxId  # Hash of the pending transaction (excluding witnesses)
```

In other words, it contains everything that we attached to the transaction when we built it.

The other field `scriptContextPurpose` is of type `ScriptPurpose` which can be one of:
```
Minting CurrencySymbol
Spending TxOutRef
Rewarding StakingCredential
Certifying DCert
```

Now that we have a theoretical overview of the script context, let's write a simple script utilising it in practice. It is a silly example with the sole purpose of exploring the use of script context inside the validator. We will create a script that validates only if attempts to create less than or exactly **ten** outputs and if its valid range is **infinite** (this is the default range applied if no valid range is specified during the transaction building stage).

Note: I wanted to make a better example here of creating a shared wallet where the script validates if one or the other of the specified signatures (`PubKeyHash`es) is present as the signatory of the transaction. However, it seems that there is no way to *bake* the pubkeyhashes inside the script at the moment: https://github.com/input-output-hk/plutus/issues/4404.

## Might need understanding time in Plutus section here

Firstly, we need to change the way we look at arguments in the `mkValidator` function. We are not interested in the `datum` and `redeemer` fields so we can ignore them now, and instead look only at the `context` field:
```
mkValidator :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
mkValidator _ _ ctx =
```

We want to check two things in the validation as mentioned above, so we will create helper functions that check each one individually, `checkOutputs` and `checkRange`. The main function will check that both of these helper functions evaluate to `True`:
```
mkValidator _ _ ctx =
  if checkOutputs P.&& checkRange 
    then ()
    else P.error ()
```
Now we need to write the helper functions which is a bit more complicated as they have to work with the transaction `context`. Firstly, to even access the `context` in the structure of `ScriptContext` we explored, we have to build that structure from the third argument `ctx`, which is of type `P.BuiltinData`. We can do this by using `unsafeFromBuiltinData` function on `ctx` (the difference between `unsafeFromBuiltinData` and `fromBuiltinData` is that the former will error if it fails which is faster, while the latter will return `Nothing`). This leads to our first helper function:

`valCtx = Plutus.unsafeFromBuiltinData ctx`

We now have the `valCtx` of type `ScriptContext`, so we can destructure it accordingly. Remember that it consists of two fields `scriptContextTxInfo :: TxInfo` and `scriptContextPurpose :: ScriptPurpose`. For our use case, we are only interested in the `TxInfo`, and we can get it with our second helper function:

`info = Plutus.scriptContextTxInfo valCtx`

The `info` is now of type `TxInfo`, which contains all the information we need for our validation. Specifically, we are interested in the fields `txInfoOutputs :: [TxOut]` and `txInfoValidRange :: POSIXTimeRange`. To create the validation logic for the number of UTxOs we can simply use the `length` function to count the number of UTxOs in the transaction, since `txInfoOutputs` is a `List`. We also combine that with the Plutus Prelude `traceIfFalse` function to provide us with debugging info in case of invalid transactions:

`checkOutputs = P.traceIfFalse "Not less than 10 outputs!" P.$ P.length (Plutus.txInfoOutputs info) P.<= 10`

The final part of our validator logic is to check that the submitted transaction's valid range is infinite. We first destruct the `txInfoValidRange` field of `TxInfo`, and compare it with the `Plutus.always` pre-defined time interval which corresponds to the infinite time range. Again, we use the `traceIfFalse` as before:

`checkRange = P.traceIfFalse "Not infinite range!" P.$ Plutus.txInfoValidRange info P.== Plutus.always`

Now we have all the pieces of the validation done and the full validator looks like this:
```
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

The rest is just simple renaming of functions accordingly.

Helper scripts for testing:
```
create-script-address.sh
check-utxos.sh
send-funds-to-script.sh
spend-script-utxo-invalid-range.sh
spend-script-utxo-invalid-utxos.sh
spend-script-utxo.sh
```

Required for converting context in untyped validators:
https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V2-Ledger-Api.html#t:UnsafeFromData