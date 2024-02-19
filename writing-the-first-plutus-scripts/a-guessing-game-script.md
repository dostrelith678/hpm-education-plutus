# A Guessing Game Script

## Exploring datum and redeemer in scripts

For our next script, we will use the `datum` and `redeemer` arguments instead of ignoring them. We will still ignore the third argument, the _transaction context_, for now. The goal of this script is to create a guessing game, where the UTxO sitting at the script address is unlocked if the submitting transaction sends a `redeemer` that matches the `datum` present at that UTxO. It is quite a simple re-rewrite from our first script - we just need to add a bit of logic to the `mkValidator` function and replace the function names accordingly. Create a new file `src/GuessingGame.hs` for this validator and paste the code from `SimplestSuccess.hs` into it. Our extensions and imports stay exactly the same, let's just remove the `qualified` from the `PlutusTx.Prelude` import so that we do not have to prefix every `Prelude` function with `Prelude.`:

```haskell
import PlutusTx.Prelude
```

### Writing the validator

We rename our module and exposed functions. Let's remove the script name from the exposed generic functions for serialising and writing the script to disk and just call them `scriptSerialised` and `writeSerialisedScript`:

```haskell
module GuessingGame
  (
    scriptSerialised,
    writeSerialisedScript
  )
where
```

Our `mkValidator` function becomes:

```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator datum redeemer _ =
    if datum == redeemer then ()
    else error ()
```

We use the comparison function to check whether the received `redeemer` matches the `datum` sitting at the UTxO. If that's the case, we return `()` as a sign of successful validation. Otherwise, we use the `error ()` from `Prelude` to signify failed validation. Our `validator` and `script` functions stay exactly the same, but we need to update the names of generic functions for serialising and writing the script to disk, and set the write filename to `GuessingGame.plutus`:

```haskell
scriptShortBs :: SBS.ShortByteString
scriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

scriptSerialised :: PlutusScript PlutusScriptV2
scriptSerialised = PlutusScriptSerialised scriptShortBs

writeSerialisedScript :: IO (Either (FileError ()) ())
writeSerialisedScript = writeFileTextEnvelope "compiled/GuessingGame.plutus" Nothing scriptSerialised
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
                        , Helpers.Utils
...
```
{% endhint %}

### Testing the validator

#### Serialising string-like datums

To test this script, we could use the compiled `unit.json` as our datum, but let's instead create a more interesting one. Again launch the `cabal repl` and load the `Utils` module. Let's say we want to create a secret in the `String` format. We can try:

```haskell
Prelude> :l src/helpers/Utils.hs 
Ok, one module loaded.
Prelude Utils> writeJSONData "compiled/assets/secretGuess.json" "I am a secret"
```

But we will get the following error:

```haskell
<interactive>:3:1: error:
    • No instance for (PlutusTx.IsData.Class.ToData Char)
        arising from a use of ‘writeJSONData’
```

It seems that `PlutusTx.toData` class does not implement an instance for the `String` type. Indeed, if we check the [documentation](https://intersectMBO.github.io/plutus/master/plutus-tx/html/PlutusTx-IsData-Class.html), we see that only a `ToData BuiltinByteString` is defined when it comes to string-like values. So we need to convert our Haskell `String` to a Plutus `BuiltinByteString`. Again we need to look through the [documentation](https://intersectMBO.github.io/plutus/master/plutus-tx/html/PlutusTx-Builtins-Class.html#v:stringToBuiltinByteString) to find the function we need (located in the `PlutusTx.Builtins.Class` module):

```haskell
stringToBuiltinByteString :: String -> BuiltinByteString
```

Let's load up this module and apply this function to our string before serialising it:

```haskell
Prelude Utils> import PlutusTx.Builtins.Class
Prelude PlutusTx.Builtins.Class Utils> writeJSONData "compiled/assets/secretGuess.json" $ stringToBuiltinByteString "I am
 a secret"
```

No error message, and our datum is compiled under `compiled/assets/secretGuess.json`. It looks like this:

```json
{"bytes":"4920616d206120736563726574"}
```

We are now ready to test the validator! Create a new directory `testnet/GuessingGame` for this purpose.

Let's compile the validator as well.

```haskell
-- The following line is not necessary if the module was added to exposed-modules in the .cabal file
Prelude> :l src/GuessingGame.hs
Prelude> GuessingGame.writeSerialisedScript
Right ()
```

Now, we need to create an address for this validator like before:

```sh
# testnet/GuessingGame/create-script-address.sh

#!/usr/bin/env bash

NWMAGIC=2 # preview testnet

# Build script address
cardano-cli address build \
--payment-script-file ../../compiled/GuessingGame.plutus \
--testnet-magic $NWMAGIC \
--out-file GuessingGame.addr

echo "Script address: $(cat GuessingGame.addr)"
```

Our `check-utxos.sh` script remains the same, but we updated the script address:

```bash
# testnet/GuessingGame/check-utxos.sh

#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

funds_normal=$(cardano-cli query utxo \
--address $(cat ../address/01.addr) \
--testnet-magic $NWMAGIC)

funds_script=$(cardano-cli query utxo \
--address $(cat GuessingGame.addr) \
--testnet-magic $NWMAGIC)

echo "Normal address:"
echo "${funds_normal}"

echo "Script address:"
echo "${funds_script}"
```

Again, we will see existing UTxOs present on the script address, as someone has already compiled and used it. Let's send some value to the script along with our secret datum.

```bash
# testnet/GuessingGame/set-guess-utxo.sh

#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/01.addr) \
    --tx-in ea340a31a9ad4dd059e6743274607e2cc7bdb7b12b5345be8bc81988d9a6ea86#1 \
    --tx-out $(cat GuessingGame.addr)+2000000 \
    --tx-out-datum-embed-file ../../compiled/assets/secretGuess.json \
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

If we check the UTxOs again, we'll see a new UTxO sitting at the script address. Only transactions with the matching redeemer can spend it. Let's try to first spend it with an invalid redeemer. Since the datum of the UTxO we are trying to spend needs to be specified in the transaction regardless, this is slightly pointless. But to show that the validator works as it should, let's specify the correct datum, but the wrong redeemer:

```bash
# testnet/GuessingGame/spend-script-utxo-invalid.sh

#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/01.addr) \
    --tx-in 8fc7a4fda80ad379811c44591a9fc4bae7fcd9a4ddda1574df910adc0143ac7a#0 \
    --tx-in-script-file ../../compiled/GuessingGame.plutus \
    --tx-in-datum-file ../../compiled/assets/secretGuess.json \
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

Trying to execute it gives us a script execution failure:

```bash
./spend-script-utxo-invalid.sh

Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 0 (in ascending order of the TxIds) failed with: 
The Plutus script evaluation failed: An error has occurred:  User error:
The machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.
Script debugging logs:
```

The logs are empty as we have not configured any logging, nor did we give an error message. But we still know that the script failed to execute successfully for this transaction because the redeemer does not match the datum. Let's create a valid transaction this time. We just need to change the `--tx-in-redeemer-file` line to point to our secret guess:

4\) `spend-script-utxo.sh`

```bash
# testnet/GuessingGame/spend-script-utxo.sh

#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/01.addr) \
    --tx-in 8fc7a4fda80ad379811c44591a9fc4bae7fcd9a4ddda1574df910adc0143ac7a#0 \
    --tx-in-script-file ../../compiled/GuessingGame.plutus \
    --tx-in-datum-file ../../compiled/assets/secretGuess.json \
    --tx-in-redeemer-file ../../compiled/assets/secretGuess.json \
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

We see that the transaction is successful this time, and we are able to spend the script UTxO!

```bash
./spend-script-utxo.sh
Estimated transaction fee: Lovelace 173085
Transaction successfully submitted.
```
