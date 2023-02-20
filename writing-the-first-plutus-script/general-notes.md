# General Notes

### General steps for development

Creating and executing Plutus scripts can be summarised in a couple of steps:

* Write your Plutus on-chain code.
* Serialize your Plutus on-chain code to the text envelope format (`cardano-cli` expects this format).
* Create your transaction with the accompanying Plutus script(s). This must include a datum hash (Important: any script UTxO without a datum is **UNSPENDABLE**).
* Submit the transaction to execute the Plutus script. This needs to include the collateral input to cover costs in case the Plutus script fails.

### PlutusV2

As mentioned before, we will be writing PlutusV2. The significant changes from PlutusV1 to PlutusV2 are listed below ([https://www.youtube.com/watch?v=0TEKLRR5XPU](https://www.youtube.com/watch?v=0TEKLRR5XPU)):

* reference inputs (possibility to look at a `datum` without attempting to spend the associated UTxO)
* inline datums (able to store a simple datum on the ledger)
* reference scripts (no need to upload the Plutus script on each transaction, instead reference it from a previous transaction)
