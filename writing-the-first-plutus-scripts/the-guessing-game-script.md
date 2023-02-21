# The Guessing Game Script

## Exploring datum and redeemer in scripts

For our next script, we will use the `datum` and `redeemer` arguments instead of ignoring them. We will still ignore the third argument, the _transaction context_ for now. The goal of this script is to create a guessing game, where the UTxO sitting at the script address is unlocked if the submitting transaction sends a `redeemer` that matches the `datum` present at that UTxO. It is quite a simple re-rewrite from our first script - we just need to add a bit of logic to the `mkValidator` function and replace the function names accordingly. Our `mkValidator` function becomes:

```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: Prelude.BuiltinData -> Prelude.BuiltinData -> Prelude.BuiltinData -> ()
mkValidator datum redeemer _ = if datum Prelude.== redeemer then () else Prelude.error ()
```

We use the comparison function to check whether the received `redeemer` matches the `datum` sitting at the UTxO. If that's the case, we return `()` as a sign of successful validation. Otherwise, we use the `Prelude.error ()` to signify failed validation.

To test this script, the same (slightly modified) bash scripts help:

1\) `create-addresses.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet

# Build script address
cardano-cli address build \
--payment-script-file guessingGame.plutus \
--testnet-magic $NWMAGIC \
--out-file guessingGame.addr

echo "Script address: $(cat guessingGame.addr)"
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
--address $(cat guessingGame.addr) \
--testnet-magic $NWMAGIC)

echo "Normal address:"
echo "${funds_normal}"

echo "Script address:"
echo "${funds_script}"


```

3\) `set-guess-utxo.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../normal_address/01.addr) \
    --tx-in 29e96c103e6d26d9b8a110df9c8f82eaacbc53077d0b474b41fb4c0d0c0fca93#0 \
    --tx-out $(cat guessingGame.addr)+2000000 \
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

4\) `spend-script-utxo.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../normal_address/01.addr) \
    --tx-in 056bdfbe601fc0a27b883a79da227dd545c52180cc98eae06a0375427048fea8#0 \
    --tx-in-script-file guessingGame.plutus \
    --tx-in-datum-file ../assets/unit.json \
    --tx-in-redeemer-file ../assets/unit.json \
    --tx-in-collateral 056bdfbe601fc0a27b883a79da227dd545c52180cc98eae06a0375427048fea8#1 \
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
