# Introduction to the EUTxO model

Gentle reader, welcome to this Plutus course. Before starting any practical work with so-called smart contracts on Cardano, it is extremely important to have a good understanding of the underlying accounting model being utilised.

In this section, you will find an overview of the EUTxO accounting model that is used by the Cardano blockchain. We will start with a brief overview of the previously used UTxO model originating from Bitcoin, and then proceed to the extensions of that model that Cardano implemented along with some of its key feature relating to smart contracts.

## The UTxO (Unspent Transaction Output) model

The two papers:
  - https://iohk.io/en/research/library/papers/native-custom-tokens-in-the-extended-utxo-model/
  - https://iohk.io/en/research/library/papers/the-extended-utxo-model/

To get started with Plutus and smart contracts on Cardano, it is important to understand the underlying account (ledger) model it uses. Before smart contracts, Cardano used the Bitcoin's UTxO (Unspent Transaction Output) model.

In the UTxO model, transactions consist of a list of *inputs* and a list of *outputs*. Transaction inputs are existing and unspent transaction outputs that are being spent by the transaction. Each output is connected to an *address* and can only be spent once, i.e. it can only be an input to a transaction once. **Every input in a spending transaction is spent in its entirety** and is no longer an *unspent* transaction output and **cannot** be used as an input anymore. Transactions can have a different number of inputs (*n*) and outputs (*m*). For example, a transaction with one input (`n = 1`) could create one hundred outputs (`m = 100`).

Every UTxO has a value (`value`), which represents a cryptocurrency value, and a validating function attached to it. This  function serves as its *validator* (`ν`), which enforces rules to decide whether a transaction that attempts to spend it has the right to do so. The transaction provides *redeemers* (`ρ`) that are passed to the validator functions of outputs. As each UTxO is connected to an *address* with a corresponding private and public key pair, the redeemers are signatures by which the transaction is signed using the private keys. If a transaction that attempts to spend and output is signed by that output's address private key, the validation will be successful. In other words, the validation function will evaluate to true:
  `ν(value, ρ) = true`
If the transaction is not signed by the required signature(s), it will not pass validation and will fail.

The UTxO model is a great fit for a distributed ledger as it does not require any shared global state for its transactions. Transactions only need to know about their inputs, and multiple transactions dealing with separate inputs are independent and can be executed concurrently. However, the transaction functionalities of these simple validators are limited, hence Cardano has implemented an extension to the UTxO model that allows more expressive validators while preserving the core properties of the UTxO model, called the *EUTxO (Extended UTxO) model*.

## The EUTxO model (Extended UTxO)

To allow more expressive validators, the EUTxO model extends the UTxOs format by adding a *datum* (`δ`) field to outputs. A datum is an arbitrary piece of data sitting at the specific UTxO. The datum generally represents a state, and allows contracts to maintain their state, still avoiding a shared global state of the entire system. During validation, the datum of the UTxO in question is passed to the validator. Therefore, the UTxO is format is extended from `(ν, value)` to `(ν, value, δ)`.

In addition to the above extension in the EUTxO model, the validating function or validator or validator script also receives the entire transaction being validated. We call this the transaction `context`. This allows the validator to enforce validity constraints based not only on the input in question, but the entire consuming transaction with all of its inputs, outputs, signatures, certificates, etc.

Therefore, the validating function from the UTxO model that receives just the redeemer, which is simply a signature in this case: `ν(value, ρ) = true`; becomes more complex: `ν(value, δ, ρ, tx ) = true`. It now receives the datum `δ` sitting at the UTxO being spent in the transaction, the redeemer `ρ`, which in general now contains the instruction or the intent of the spending transaction rather than the spenders' signature. The redeemer itself is no longer required to carry the signature, as any signatures in the transaction will be included and available for inspection in its context `tx`.

The final change in the EUTxO model is the addition of a *validity interval* for transactions. This interval specifies the length of time during which the transaction **can** be processed. It is a fundamental check that happens before the validator script runs. In Cardano, the validity intervals are represented via slots, and each slot currently represents 1 second.

### Where are the validator and datum stored?

We mentioned that in the EUTxO model, transaction outputs can have an arbitrary piece of data attached to them called `datum`. In the ledger, they are actually stored as cryptographic hashes of the actual `datum` object to keep outputs as small as possible memory-wise. Once an output with a `datum` attached is being spent in a transaction, the transaction must provide the actual `datum` object as its input. The transaction will only be able to pass validation if the provided `datum` matches the hash that sits at the corresponding output.

The same principle is applied to validator scripts, which are stored in the ledger as their cryptographic hashes. When a transaction wants to use the validator script, it must provide it (in the case of Cardano as a Plutus script file) as part of the transaction. The hash of the script again must match the hash that exists in the ledger for the transaction to be able to pass validation.

### Validity rules for transactions in the EUTxO model

1. The current tick is within the validity interval
  `currentTick ∈ t.validityInterval`
2. All outputs have non-negative values
  `For all o ∈ t.outputs, o.value ≥ 0`
3. All inputs refer to unspent outputs
  `{i.outputRef : i ∈ t.inputs} ⊆ unspentOutputs(l)`
4. Value is preserved
  `Sum of all input values = Sum of all output values - Tx fee`
5. No output is double spent
  `If i1, i2 ∈ t.inputs and i1.outputRef = i2.outputRef then i1 = i2`
6. All inputs validate
  `For all i ∈ t.inputs, [i.validator] (i.datum, i.redeemer , toData(toContext(t, i, l))) = true`
7. Validator scripts match output addresses
  `For all i ∈ t.inputs, scriptAddr(i.validator) = getSpentOutput(i, l).addr`
8. Each datum matches its output hash
  `For all i ∈ t.inputs, dataHash(i.datum) = getSpentOutput(i, l).datumHash`

### Custom Native Tokens in the EUTxO model

The EUTxO model ledger running on Cardano directly supports native custom tokens, including their forging (minting), burning and transfer. In IOG research papers, this model is referred to as EUTxO<sub>ma</sub> (<sub>ma</sub> stands for *multi-asset*), and is the combination of the above described EUTxO model and the UTxO<sub>ma</sub> model, which was introduced in Cardano in the Mary era, that supported custom native tokens on the ledger level via their *forging policy scripts*.

The UTxO<sub>ma</sub> model generalised the `value` field by making it a two-level structure of *asset group* and *currency*, together called *token bundle*. One asset group can have multiple currencies. An example of a token bundle would be: `{CoinGroup → {CoinCurrency → 3}, g → {t1 → 1, t2 → 1}}`, which contains three `CoinCurrency` tokens of the `CoinGroup` asset group, and one of `t1` and `t2` currencies of the `g` asset group.

### Constraint Emitting machines (CEMs)

Arguably the most important functionality of the EUTxO model is the possibility of implementing validators as *Constraint Emitting Machines*. These are [state machines](https://en.wikipedia.org/wiki/Finite-state_machine) which can have a finite or infinite number of valid states. Their important feature is the possibility of emitting constraints that transactions must fulfill to interact with the validator. That is, given a certain CEM state of the validator, the validator on-chain code can tell the transaction-building client which transitions to new states are allowed and in what fashion.

* Could do some state machine here

### State Thread Tokens

Specifically for smart contracts, this merge of the EUTxO and UTxO<sub>ma</sub> models allows validators to create so-called *state thread tokens* (a form of NFTs), allowing even more functionality to be transferred from off-chain to on-chain code, increasing security and simplicity of application implementations. In particular, state thread tokens are used as unique identifiers of specific validator runs, i.e. used to distinguish between different outputs threads created by the same validator. Moreover, the minting transaction of the state thread token specifies the definite state that the validator thread was initiated in.

This is done in a way that a validator implementing a CEM also implements a forging policy for the state thread token. The forging policy enforces rules to ensure that the token is locked by the same validator and that it can only be minted in a valid initial state (specifically, that the datum attached to the NFT output corresponds to a valid initial state). Upon any further interaction, the validator will check that the state thread token is still present and propagated to the new state if a valid transition occurs. Finally, the state thread token is burnt if the spending transaction transitions the CEM into a final state at which point no further interaction is possible with that specific thread.

* Could do a parallel runs figure here



