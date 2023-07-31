# Testing a validator with the emulator

Now, we will test a validator that we created earlier with the Plutus emulator. We will use the `GuessingGame.hs` validator to define two emulator traces, one for a valid script spend and one for an invalid script spend. We will edit the `GuessingGame.hs` file directly, adding the new tracing functions. As part of this exercise, we will also learn about the `Contract` monad, which we will use to create and submit transactions in the emulation.

Since `EmulatorTrace` is a monad, we will use do-notation to define its actions. In general, what we want to do is have a contract function written which describes the sequence of transactions that will be generated and submitted. We then call the `activateContractWallet` function with a wallet (we can use `knownWallet 1` - corresponding to the first of the 10 default wallets) and the contract:

```haskell
emulatorTrace :: EmulatorTrace ()
emulatorTrace = do
    void Prelude.$ activateContractWallet (knownWallet 1) contract
    void Prelude.$ Emulator.waitNSlots 2
```

We can also define a helper function for running the trace:

```haskell
runTrace :: IO ()
runTrace = runEmulatorTraceIO emulatorTrace
```

### Writing the contract

To run a meaningful `EmulatorTrace`, we have to define a contract that can be used for emulation. The contract type is `Contract w s e a`:

* `w` is the _state_ type of the contract. The state can be updated from inside the contract and is generally used for communication between contract instances. It should not be confused with general logging which is always available through the `Contract.logInfo` function.
* `s` stands for _schema_, a list of endpoints available to the contract
* `e` is the type of error that will be generated if an exception is thrown
* `a` is the type of final value the contract produces if no exception is thrown

For simple examples, such as ours, we do not need to use a contract state, an endpoint schema, or produce a final value. Therefore, a simple type signature for our contract for the `GuessingGame` validator will be:

`contract :: Contract () Empty Text ()`

`Empty` for `s` means no endpoints are available, and we just use `Text` to report error messages. Since `Contract` is a monad, we can use do-notation again. We can start off with some logging:

```haskell
contract :: Contract () Empty Text ()
contract = do
    now <- currentNodeClientTimeRange
    Contract.logInfo @String Prelude.$ "Logging from inside the contract, contract time is: " Prelude.++ show now
    Contract.logInfo @String Prelude.$ "First transaction: send funds to script and set a datum to be guessed"
```

Ok, now we need to create the first transaction. This is done by using the `Ledger.Constraints` module. We define the constraints of the transaction, i.e. what we want it to do, and the contract constructs a valid transaction based on its constraints. We want the first transaction to send some ADA to the script address along with a datum that will need to be guessed to spend it later, so we use the `mustPayToOtherScriptWithDatumInTx` function:

```haskell
    ...
    let tx1 = Constraints.mustPayToOtherScriptWithDatumInTx valHash unitDatum Prelude.$ Ada.lovelaceValueOf 25000000
    ledgerTx1 <- submitTx tx1
    awaitTxConfirmed Prelude.$ getCardanoTxId ledgerTx1
    Contract.logInfo @String Prelude.$ "tx1 successfully submitted"
```

Next, we want to try to spend the newly generated script UTxO by matching the datum with the redeemer. This is a bit more tricky because we have to tell the contract where to find the UTxO via `lookups`. In this case, we are spending a script output, so the lookup must know the validator behind the script address (as we have seen before with `cardano-cli`, to construct a valid transaction spending the script output, we must supply the validator). To make it a bit clearer, we will add a logging line to inspect the `utxos` and `lookups` in the contract and inspect it later.

First, we have to get the UTxO(s) at the script address with `utxosAt scrAddress`. Note that this will get all the UTxOs and for simplicity (since we know there will always be only one), we can take just one with the `head` function. The `lookups` we need for this transaction are the `validator` itself, which we define with `Constraints.plutusV2OtherScript validator` and the UTxO(s) that are sitting at the script address that we can get with `Constraints.unspentOutputs utxos`. We join these two monoidal values together with `<>` (`mappend`):

```haskell
    Contract.logInfo @String Prelude.$ "Second transaction: attempt to spend script output with wrong redeemer"
    utxos <- utxosAt scrAddress
    let oref = Prelude.head (Prelude.fst Prelude.<$> Map.toList utxos)
        lookups =
            Constraints.plutusV2OtherScript validator
            Prelude.<> Constraints.unspentOutputs utxos
```

Now, we just need to construct the transaction with the correct redeemer that matches the datum (in our case just the `unitRedeemer`). We use the `Constraints.mustSpendScriptOutput` function and specify the output reference that we defined `oref` along with the `unitRedeemer`. We also must include the `unitDatum` in the transaction via `Constraints.mustIncludeDatumInTx unitDatum`:

```haskell
        tx2 =
            Constraints.mustSpendScriptOutput oref unitRedeemer
            Prelude.<> Constraints.mustIncludeDatumInTx unitDatum
```

The final part is simply submitting the transaction with our given constraints. We use `submitTxConstraintsWith @Void lookups tx2` and `awaitTxConfirmed`. Before we do, we can log the `oref` and `lookups` as mentioned before:

```haskell
    Contract.logInfo @String Prelude.$ "Oref: " Prelude.++ show oref Prelude.++ ", Lookups: " Prelude.++ show lookups
    ledgerTx2 <- submitTxConstraintsWith @Void lookups tx2
    Contract.logInfo @String Prelude.$ "waiting for tx2 confirmed..."
    awaitTxConfirmed Prelude.$ getCardanoTxId ledgerTx2
    Contract.logInfo @String Prelude.$ "tx2 successfully submitted"
```

Finally, we can load the module and run the trace:

```
ghci> :l src/GuessingGame.hs
ghci> runTrace

Slot 00000: TxnValidate 43ba666cc8a22a04b63a3b605ce14146dfa5ed999986625ad90c1bc16dabdd84 []
Slot 00000: SlotAdd Slot 1
Slot 00001: W[7]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[8]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[6]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[4]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[2]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[1]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[10]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[9]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[3]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[5]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: 00000000-0000-4000-8000-000000000000 {Wallet W[1]}:
  Contract instance started
Slot 00001: *** CONTRACT LOG: "Logging from inside the contract, contract time is: (POSIXTime {getPOSIXTime = 1596059092000},POSIXTime {getPOSIXTime = 1596059092999})"
Slot 00001: *** CONTRACT LOG: "First transaction: send funds to script and set a datum to be guessed"
Slot 00001: 00000000-0000-4000-8000-000000000000 {Wallet W[1]}:
  Contract log: Object (fromList [("mkTxLogLookups",Object (fromList [("slOtherData",Array []),("slOtherScripts",Array []),("slOwnPaymentPubKeyHash",Null),("slOwnStakingCredential",Null),("slPaymentPubKeyHashes",Array []),("slTxOutputs",Array []),("slTypedValidator",Null)])),("mkTxLogResult",Object (fromList [("Right",Object (fromList [("tag",String "UnbalancedEmulatorTx"),("unBalancedEmulatorTx",Object (fromList [("txCertificates",Array []),("txCollateralInputs",Array []),("txData",Array [Array [String "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",Object (fromList [("getDatum",String "d87980")])]]),("txFee",Object (fromList [("getValue",Array [])])),("txInputs",Array []),("txMetadata",Null),("txMint",Object (fromList [("getValue",Array [])])),("txMintingWitnesses",Array []),("txOutputs",Array [Object (fromList [("getTxOut",Object (fromList [("address",String "addr_test1wz3r0qhn7u38h7scq2k65vewg9fr6ztrnet904l3e4aft0chgqzep"),("datum",Object (fromList [("constructor",Number 0.0),("fields",Array [])])),("datumhash",String "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"),("inlineDatum",Null),("referenceScript",Null),("value",Object (fromList [("lovelace",Number 2.5e7)]))]))])]),("txReferenceInputs",Array []),("txReturnCollateral",Null),("txScripts",Array []),("txSignatures",Array []),("txTotalCollateral",Null),("txValidRange",Object (fromList [("ivFrom",Array [Object (fromList [("tag",String "NegInf")]),Bool True]),("ivTo",Array [Object (fromList [("tag",String "PosInf")]),Bool True])])),("txWithdrawals",Array [])])),("unBalancedTxRequiredSignatories",Array []),("unBalancedTxUtxoIndex",Array [])]))])),("mkTxLogTxConstraints",Object (fromList [("txConstraintFuns",Array []),("txConstraints",Array [Object (fromList [("contents",Array [Object (fromList [("addressCredential",Object (fromList [("contents",String "a23782f3f7227bfa1802adaa332e41523d09639e5657d7f1cd7a95bf"),("tag",String "ScriptCredential")])),("addressStakingCredential",Null)]),Object (fromList [("contents",Object (fromList [("getDatum",String "d87980")])),("tag",String "TxOutDatumInTx")]),Null,Object (fromList [("getValue",Array [Array [Object (fromList [("unCurrencySymbol",String "")]),Array [Array [Object (fromList [("unTokenName",String "")]),Number 2.5e7]]]])])]),("tag",String "MustPayToAddress")])]),("txOwnInputs",Array []),("txOwnOutputs",Array [])]))])
Slot 00001: W[1]: TxSubmit: ac35b5e8f3649d55ae589a19e26a0413e6e8fd8911fcbf7371c4174fbb6c599c
Slot 00001: TxnValidate ac35b5e8f3649d55ae589a19e26a0413e6e8fd8911fcbf7371c4174fbb6c599c []
Slot 00001: SlotAdd Slot 2
Slot 00002: W[7]: InsertionSuccess: New tip is Tip(Slot 2, BlockId ce08edfdc283bf3735f357e13a7cabf68923867717483467cf38b87997732b3b, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[8]: InsertionSuccess: New tip is Tip(Slot 2, BlockId ce08edfdc283bf3735f357e13a7cabf68923867717483467cf38b87997732b3b, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[6]: InsertionSuccess: New tip is Tip(Slot 2, BlockId ce08edfdc283bf3735f357e13a7cabf68923867717483467cf38b87997732b3b, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[4]: InsertionSuccess: New tip is Tip(Slot 2, BlockId ce08edfdc283bf3735f357e13a7cabf68923867717483467cf38b87997732b3b, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[2]: InsertionSuccess: New tip is Tip(Slot 2, BlockId ce08edfdc283bf3735f357e13a7cabf68923867717483467cf38b87997732b3b, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[1]: InsertionSuccess: New tip is Tip(Slot 2, BlockId ce08edfdc283bf3735f357e13a7cabf68923867717483467cf38b87997732b3b, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[10]: InsertionSuccess: New tip is Tip(Slot 2, BlockId ce08edfdc283bf3735f357e13a7cabf68923867717483467cf38b87997732b3b, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[9]: InsertionSuccess: New tip is Tip(Slot 2, BlockId ce08edfdc283bf3735f357e13a7cabf68923867717483467cf38b87997732b3b, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[3]: InsertionSuccess: New tip is Tip(Slot 2, BlockId ce08edfdc283bf3735f357e13a7cabf68923867717483467cf38b87997732b3b, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[5]: InsertionSuccess: New tip is Tip(Slot 2, BlockId ce08edfdc283bf3735f357e13a7cabf68923867717483467cf38b87997732b3b, BlockNumber 1). UTxO state was added to the end.
Slot 00002: *** CONTRACT LOG: "tx1 successfully submitted"
Slot 00002: *** CONTRACT LOG: "Second transaction: attempt to spend script output with wrong redeemer"
Slot 00002: *** CONTRACT LOG: "Oref: TxOutRef {txOutRefId = ac35b5e8f3649d55ae589a19e26a0413e6e8fd8911fcbf7371c4174fbb6c599c, txOutRefIdx = 0}, Lookups: ScriptLookups {slTxOutputs = fromList [(TxOutRef {txOutRefId = ac35b5e8f3649d55ae589a19e26a0413e6e8fd8911fcbf7371c4174fbb6c599c, txOutRefIdx = 0},ScriptDecoratedTxOut {_decoratedTxOutValidatorHash = a23782f3f7227bfa1802adaa332e41523d09639e5657d7f1cd7a95bf, _decoratedTxOutStakingCredential = Nothing, _decoratedTxOutValue = Value (Map [(,Map [(\"\",25000000)])]), _decoratedTxOutScriptDatum = (923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec,DatumInBody (Datum {getDatum = Constr 0 []})), _decoratedTxOutReferenceScript = Nothing, _decoratedTxOutValidator = Nothing})], slOtherScripts = fromList [(a23782f3f7227bfa1802adaa332e41523d09639e5657d7f1cd7a95bf,Versioned {unversioned = <Script>, version = PlutusV2})], slOtherData = fromList [], slPaymentPubKeyHashes = fromList [], slTypedValidator = Nothing, slOwnPaymentPubKeyHash = Nothing, slOwnStakingCredential = Nothing}"
Slot 00002: 00000000-0000-4000-8000-000000000000 {Wallet W[1]}:
  Contract log: Object (fromList [("mkTxLogLookups",Object (fromList [("slOtherData",Array []),("slOtherScripts",Array [Array [Object (fromList [("getScriptHash",String "a23782f3f7227bfa1802adaa332e41523d09639e5657d7f1cd7a95bf")]),Object (fromList [("unversioned",String "581a0100002225335333573466ebc00c008488008488004448004581"),("version",String "PlutusV2")])]]),("slOwnPaymentPubKeyHash",Null),("slOwnStakingCredential",Null),("slPaymentPubKeyHashes",Array []),("slTxOutputs",Array [Array [Object (fromList [("txOutRefId",Object (fromList [("getTxId",String "ac35b5e8f3649d55ae589a19e26a0413e6e8fd8911fcbf7371c4174fbb6c599c")])),("txOutRefIdx",Number 0.0)]),Object (fromList [("_decoratedTxOutReferenceScript",Null),("_decoratedTxOutScriptDatum",Array [String "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",Object (fromList [("contents",Object (fromList [("getDatum",String "d87980")])),("tag",String "DatumInBody")])]),("_decoratedTxOutStakingCredential",Null),("_decoratedTxOutValidator",Null),("_decoratedTxOutValidatorHash",String "a23782f3f7227bfa1802adaa332e41523d09639e5657d7f1cd7a95bf"),("_decoratedTxOutValue",Object (fromList [("getValue",Array [Array [Object (fromList [("unCurrencySymbol",String "")]),Array [Array [Object (fromList [("unTokenName",String "")]),Number 2.5e7]]]])])),("tag",String "ScriptDecoratedTxOut")])]]),("slTypedValidator",Null)])),("mkTxLogResult",Object (fromList [("Right",Object (fromList [("tag",String "UnbalancedEmulatorTx"),("unBalancedEmulatorTx",Object (fromList [("txCertificates",Array []),("txCollateralInputs",Array []),("txData",Array [Array [String "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec",Object (fromList [("getDatum",String "d87980")])]]),("txFee",Object (fromList [("getValue",Array [])])),("txInputs",Array [Object (fromList [("txInputRef",Object (fromList [("txOutRefId",Object (fromList [("getTxId",String "ac35b5e8f3649d55ae589a19e26a0413e6e8fd8911fcbf7371c4174fbb6c599c")])),("txOutRefIdx",Number 0.0)])),("txInputType",Object (fromList [("contents",Array [Object (fromList [("getRedeemer",String "d87980")]),Object (fromList [("Left",String "a23782f3f7227bfa1802adaa332e41523d09639e5657d7f1cd7a95bf")]),String "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"]),("tag",String "TxScriptAddress")]))])]),("txMetadata",Null),("txMint",Object (fromList [("getValue",Array [])])),("txMintingWitnesses",Array []),("txOutputs",Array []),("txReferenceInputs",Array []),("txReturnCollateral",Null),("txScripts",Array [Array [Object (fromList [("getScriptHash",String "a23782f3f7227bfa1802adaa332e41523d09639e5657d7f1cd7a95bf")]),Object (fromList [("unversioned",String "581a0100002225335333573466ebc00c008488008488004448004581"),("version",String "PlutusV2")])]]),("txSignatures",Array []),("txTotalCollateral",Null),("txValidRange",Object (fromList [("ivFrom",Array [Object (fromList [("tag",String "NegInf")]),Bool True]),("ivTo",Array [Object (fromList [("tag",String "PosInf")]),Bool True])])),("txWithdrawals",Array [])])),("unBalancedTxRequiredSignatories",Array []),("unBalancedTxUtxoIndex",Array [Array [Object (fromList [("txOutRefId",Object (fromList [("getTxId",String "ac35b5e8f3649d55ae589a19e26a0413e6e8fd8911fcbf7371c4174fbb6c599c")])),("txOutRefIdx",Number 0.0)]),Object (fromList [("getTxOut",Object (fromList [("address",String "addr_test1wz3r0qhn7u38h7scq2k65vewg9fr6ztrnet904l3e4aft0chgqzep"),("datum",Null),("datumhash",String "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"),("inlineDatum",Null),("referenceScript",Null),("value",Object (fromList [("lovelace",Number 2.5e7)]))]))])]])]))])),("mkTxLogTxConstraints",Object (fromList [("txConstraintFuns",Array []),("txConstraints",Array [Object (fromList [("contents",Array [Object (fromList [("txOutRefId",Object (fromList [("getTxId",String "ac35b5e8f3649d55ae589a19e26a0413e6e8fd8911fcbf7371c4174fbb6c599c")])),("txOutRefIdx",Number 0.0)]),Object (fromList [("getRedeemer",String "d87980")]),Null]),("tag",String "MustSpendScriptOutput")]),Object (fromList [("contents",Object (fromList [("getDatum",String "d87980")])),("tag",String "MustIncludeDatumInTx")])]),("txOwnInputs",Array []),("txOwnOutputs",Array [])]))])
Slot 00002: W[1]: TxSubmit: 3287e0dc25c58ad1550995deb3f4931ff642fc6545ea66c9e0432c0fc78f8808
Slot 00002: *** CONTRACT LOG: "waiting for tx2 confirmed..."
Slot 00002: TxnValidate 3287e0dc25c58ad1550995deb3f4931ff642fc6545ea66c9e0432c0fc78f8808 []
Slot 00002: SlotAdd Slot 3
Slot 00003: W[7]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 64c51d0cd6ce8034738a548b1271c8285fe310d851054e2f68bf9057fa44ec36, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[8]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 64c51d0cd6ce8034738a548b1271c8285fe310d851054e2f68bf9057fa44ec36, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[6]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 64c51d0cd6ce8034738a548b1271c8285fe310d851054e2f68bf9057fa44ec36, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[4]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 64c51d0cd6ce8034738a548b1271c8285fe310d851054e2f68bf9057fa44ec36, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[2]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 64c51d0cd6ce8034738a548b1271c8285fe310d851054e2f68bf9057fa44ec36, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[1]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 64c51d0cd6ce8034738a548b1271c8285fe310d851054e2f68bf9057fa44ec36, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[10]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 64c51d0cd6ce8034738a548b1271c8285fe310d851054e2f68bf9057fa44ec36, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[9]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 64c51d0cd6ce8034738a548b1271c8285fe310d851054e2f68bf9057fa44ec36, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[3]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 64c51d0cd6ce8034738a548b1271c8285fe310d851054e2f68bf9057fa44ec36, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[5]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 64c51d0cd6ce8034738a548b1271c8285fe310d851054e2f68bf9057fa44ec36, BlockNumber 2). UTxO state was added to the end.
Slot 00003: *** CONTRACT LOG: "tx2 successfully submitted"
Slot 00003: 00000000-0000-4000-8000-000000000000 {Wallet W[1]}:
  Contract instance stopped (no errors)
Slot 00003: SlotAdd Slot 4
Slot 00004: W[7]: InsertionSuccess: New tip is Tip(Slot 4, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 3). UTxO state was added to the end.
Slot 00004: W[8]: InsertionSuccess: New tip is Tip(Slot 4, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 3). UTxO state was added to the end.
Slot 00004: W[6]: InsertionSuccess: New tip is Tip(Slot 4, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 3). UTxO state was added to the end.
Slot 00004: W[4]: InsertionSuccess: New tip is Tip(Slot 4, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 3). UTxO state was added to the end.
Slot 00004: W[2]: InsertionSuccess: New tip is Tip(Slot 4, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 3). UTxO state was added to the end.
Slot 00004: W[1]: InsertionSuccess: New tip is Tip(Slot 4, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 3). UTxO state was added to the end.
Slot 00004: W[10]: InsertionSuccess: New tip is Tip(Slot 4, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 3). UTxO state was added to the end.
Slot 00004: W[9]: InsertionSuccess: New tip is Tip(Slot 4, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 3). UTxO state was added to the end.
Slot 00004: W[3]: InsertionSuccess: New tip is Tip(Slot 4, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 3). UTxO state was added to the end.
Slot 00004: W[5]: InsertionSuccess: New tip is Tip(Slot 4, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 3). UTxO state was added to the end.
Final balances
Wallet 7: 
    {, ""}: 100000000
Wallet 8: 
    {, ""}: 100000000
Wallet 6: 
    {, ""}: 100000000
Wallet 4: 
    {, ""}: 100000000
Wallet 2: 
    {, ""}: 100000000
Wallet 1: 
    {, ""}: 99647071
Wallet 10: 
    {, ""}: 100000000
Wallet 9: 
    {, ""}: 100000000
Wallet 3: 
    {, ""}: 100000000
Wallet 5: 
    {, ""}: 100000000
```

Besides seeing that the script output was successfully spent in the second transaction, we can see that our `lookups` contain information about the output ref and the script itself:

```
Slot 00002: *** CONTRACT LOG: "
Oref: 
    TxOutRef {txOutRefId = ac35b5e8f3649d55ae589a19e26a0413e6e8fd8911fcbf7371c4174fbb6c599c, txOutRefIdx = 0},
Lookups:
    ScriptLookups {slTxOutputs = fromList [(TxOutRef {txOutRefId = ac35b5e8f3649d55ae589a19e26a0413e6e8fd8911fcbf7371c4174fbb6c599c, txOutRefIdx = 0},
    ScriptDecoratedTxOut {_decoratedTxOutValidatorHash = a23782f3f7227bfa1802adaa332e41523d09639e5657d7f1cd7a95bf,
    _decoratedTxOutStakingCredential = Nothing,
    _decoratedTxOutValue = Value (Map [(,Map [(\"\",25000000)])]),
    _decoratedTxOutScriptDatum = (923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec,
    DatumInBody (Datum {getDatum = Constr 0 []})),
    _decoratedTxOutReferenceScript = Nothing,
    _decoratedTxOutValidator = Nothing})],
    slOtherScripts = fromList [(a23782f3f7227bfa1802adaa332e41523d09639e5657d7f1cd7a95bf,
    Versioned {unversioned = <Script>, version = PlutusV2})],
    slOtherData = fromList [], slPaymentPubKeyHashes = fromList [],
    slTypedValidator = Nothing, slOwnPaymentPubKeyHash = Nothing,
    slOwnStakingCredential = Nothing}
"
```