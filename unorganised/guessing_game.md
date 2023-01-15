# Exploring datum and redeemer in scripts

For our next script, we will use the `datum` and `redeemer` arguments instead of ignoring them. We will still ignore the third argument, the transaction context, for now. We want to create a guessing game script, where the UTxO sitting at the script address is unlocked if the submitting transaction sends a `redeemer` that matches that `datum` on that UTxO. It is quite a simple re-rewrite from our first script - we just need to add a bit of logic to the `mkValidator` function and replace the function names accordingly. Our `mkValidator` function becomes:

```
{-# INLINABLE mkValidator #-}
mkValidator :: Prelude.BuiltinData -> Prelude.BuiltinData -> Prelude.BuiltinData -> ()
mkValidator datum redeemer _ = if datum Prelude.== redeemer then () else Prelude.error ()
```

We use the comparison function to check whether the received `redeemer` matches the `datum` sitting at the UTxO. If that's the case, we return `()` as a sign of successful validation. Otherwise, we use the `PlutusTx.Prelude.error ()` to signify failed validation.

To test this script, the same (slightly modified) bash scripts help:

```
create-addresses.sh
check-utxos.sh
spend-script-utxo.sh
```