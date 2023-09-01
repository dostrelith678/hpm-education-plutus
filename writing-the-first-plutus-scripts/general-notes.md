# General Notes

### General steps for development

Creating and executing Plutus scripts can be summarised in a couple of steps:

* Write your Plutus on-chain code.
* Serialize your Plutus on-chain code to the [text envelope format](https://input-output-hk.github.io/plutus-apps/main/cardano-api/html/Cardano-Api.html#v:writeFileTextEnvelope) (`cardano-cli` expects this format).
* Create your transaction with the accompanying Plutus script(s). This must include a datum, either through a datum hash or an inlinable datum. Important: any script UTxO without a datum is **UNSPENDABLE**.
* Submit the transaction to execute and test the Plutus script. This needs to include the collateral input to cover costs in case the transaction fails. The transaction should never fail this way except in a very particular edge case where the UTxO trying to be spent has been spent in the meantime before the block with the transaction propagates through the network. Important: Only _regular_ address inputs can be used as collateral (those with an associated key pair). **Script address inputs cannot be used as collateral.**
* We will also look at another way of testing Plutus scripts by emulating the blockchain at the end of the course.

### PlutusV2

As mentioned before, we will be writing PlutusV2. The significant changes from PlutusV1 to PlutusV2 are listed below ([https://www.youtube.com/watch?v=0TEKLRR5XPU](https://www.youtube.com/watch?v=0TEKLRR5XPU)):

* Reference inputs (possibility of looking at a `datum` without attempting to spend the associated UTxO)
* Inline datums (able to store a simple datum on the ledger)
* Reference scripts (no need to upload the Plutus script on each transaction, instead reference it from a previous transaction)
