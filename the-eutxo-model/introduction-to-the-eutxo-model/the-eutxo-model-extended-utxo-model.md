# The EUTxO model (Extended UTxO) model

## Extensions to the UTxO model

To allow more expressive validators, the EUTxO model extends the UTxOs format by adding a _**datum**_ (`δ`) field to transaction outputs. A datum is an arbitrary piece of data sitting at the specific UTxO. The datum generally represents a state and allows contracts to maintain their state, still avoiding a shared global state of the entire system. During validation, the datum of the UTxO in question is passed to the validator. Therefore, the UTxO format is extended from `(ν, value)` to `(ν, value, δ)`.

In addition to the above extension in the EUTxO model, **the validating function** or **validator** or **validator script** also **receives the entire transaction** being validated. We call this the transaction _**context**_. This allows the validator to enforce validity constraints based not only on the input in question but the entire consuming transaction with all of its inputs, outputs, signatures, and certificates.

Therefore, the validating function from the UTxO model which received just the redeemer (a simple signature): `ν(value, ρ) = true`; becomes more complex: `ν(value, δ, ρ, tx) = true`. It now receives the **datum** `δ` sitting at the UTxO being spent in the transaction, the **redeemer** `ρ`, which now generally contains the instruction or the intent of the spending transaction rather than the spenders' signature. The redeemer itself is no longer required to carry the signature, as any signatures in the transaction will be included and available for inspection in its **context** `tx`.

The final change in the EUTxO model is the addition of a _**validity interval**_ for transactions. This interval specifies the length of time during which the transaction _**can**_ be processed. It is a fundamental check that happens before the validator script even runs. In Cardano, the validity intervals are represented via **slots**, and each slot currently represents 1 second.

### Where are the validator and datum stored?

We mentioned that in the EUTxO model, transaction outputs can have an arbitrary piece of data attached to them called `datum`. In the ledger, they are actually stored as cryptographic hashes of the actual `datum` object to keep outputs as small as possible memory-wise. When an output with a `datum` attached to it is being spent in a transaction, the transaction must provide the actual `datum` object in its context. The transaction will only be able to pass validation if the provided `datum` matches the hash that sits at the corresponding output. Note that there is also the _**inlinable datum**_ functionality on Cardano now (part of Plutus V2), which is limited to simple datum objects but allows storing them in the ledger in full rather than just the hash. Therefore, outputs with inline datums do not require the datum object in the transaction itself, as it is already accessible from the ledger.

The same principle is applied to validator scripts, which are stored in the ledger as their cryptographic hashes. When a transaction wants to use the validator script, it must provide it as part of the transaction (in the case of Cardano as a **Plutus script file**). The hash of the script again must match the hash that exists in the ledger for the transaction to be able to pass validation. Another note here is that with Plutus V2 came the _**reference script**_ functionality. Much like inlinable datums, it allows a validator script to be attached to an UTxO that can be referenced in future transactions. Before reference scripts, the validator script needed to be attached to each transaction that wants to use it, which greatly increased the transaction size.&#x20;

### Validity rules for transactions in the EUTxO model

Any transaction in the EUTxO model must satisfy the following validity rules in order to be successful:

1. The current tick is within the validity interval\
   `currentTick ∈ t.validityInterval`
2. All outputs have non-negative values\
   `For all o ∈ t.outputs, o.value ≥ 0`
3. All inputs refer to unspent outputs\
   `{i.outputRef : i ∈ t.inputs} ⊆ unspentOutputs(l)`
4. Value is preserved\
   `Sum of all input values = Sum of all output values - Tx fee`
5. No output is double spent\
   `If i1, i2 ∈ t.inputs and i1.outputRef = i2.outputRef then i1 = i2`
6. All inputs validate\
   `For all i ∈ t.inputs, [i.validator] (i.datum, i.redeemer , toData(toContext(t, i, l))) = true`
7. Validator scripts match output addresses\
   `For all i ∈ t.inputs, scriptAddr(i.validator) = getSpentOutput(i, l).addr`
8. Each datum matches its output hash\
   `For all i ∈ t.inputs, dataHash(i.datum) = getSpentOutput(i, l).datumHash`

### Custom Native Tokens in the EUTxO model

The EUTxO model ledger running on Cardano directly supports native custom tokens, including their **forging (minting)**, **burning,** and **transfer**. In IOG research papers, this model is referred to as _**EUTxOma**_ (_**ma**_ stands for _multi-asset_) and is the combination of the above-described EUTxO model and the _**UTxOma**_ model, which was introduced in Cardano in the Mary era, and supported custom native tokens on the ledger level via their _**forging policy scripts**_.

The UTxOma model generalised the `value` field by making it a two-level structure of _**asset group**_ and _**currency**_ together called a _**token bundle**_. One asset group can have multiple currencies. An example of a token bundle would be: `{CoinGroup1 → {CoinCurrency → 3}, CoinGroup2 → {t1 → 1, t2 → 1}}`, which contains three `CoinCurrency` tokens of the `CoinGroup1` asset group, and one of `t1` and `t2` currencies of the `CoinGroup2` asset group.

### Constraint Emitting machines (CEMs)

Arguably the most important functionality of the EUTxO model is the possibility of implementing validators as _**Constraint Emitting Machines**_. These are [state machines](https://en.wikipedia.org/wiki/Finite-state\_machine) that can have a finite or infinite number of valid states. Their important feature is the possibility of emitting constraints that transactions must fulfil to interact with the validator. That is, given a certain CEM state of the validator, the validator on-chain code can tell the transaction-building client which transitions to new states are allowed and in what fashion.

### State Thread Tokens

Specifically for smart contracts, the merge of the EUTxO and UTxOma models allows validators to create so-called _**state thread tokens**_ (a form of NFTs), allowing even more functionality to be transferred from off-chain to on-chain code, increasing security and simplicity of application implementations. In particular, state thread tokens are used as unique identifiers of specific validator runs, i.e. used to distinguish between different output threads created by the same validator. Moreover, the minting transaction of the state thread token specifies the definite state that the validator thread was initiated with.

This is done in a way that a validator implementing a CEM also implements a forging policy for the state thread token. The forging policy enforces rules to ensure that the token is locked by the same validator and that it can only be minted in a valid initial state (specifically, that the datum attached to the NFT output corresponds to a valid initial state). Upon any further interaction, the validator will check that the state thread token is still present and propagated to the new state if a valid transition occurs. Finally, the state thread token is burnt if the spending transaction transitions the CEM into a final state at which point no further interaction is possible with that specific thread.
