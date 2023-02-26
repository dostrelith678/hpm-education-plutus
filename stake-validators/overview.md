# Overview

In this section, we will look at validator scripts as they relate to staking on Cardano. The scripts we looked at so far were only related to the payment part of addresses, i.e. used for the `Spending` part of `ScriptPurpose`. We will now look at `StakeValidator`s which correspond to `Certifying` and `Rewarding` script purposes.

The `Rewarding` script purpose is related to validating reward withdrawals, i.e. whether a transaction is allowed to withdraw rewards from the script stake address.

The `Certifying` script purpose is related to validating certificates present in the transaction which control the registration and delegation of the script stake address, i.e. validating whether the transaction can register/de-register the given script stake address and whether its funds to the specified stake pool.

Here we see a nice overview of Plutus scripts on Cardano, where arbitrary logic takes over key witnesses. A Cardano base address (Shelley-based) is composed of two parts, the payment part (which controls spending associated UTxOs) and the _optional_ staking part (which controls delegation-related actions). These two parts have separate keys (payment and stake keys), that are used to sign transactions to prove that they are valid. For example, if we want to register and delegate a stake address to a pool, we must submit a transaction that is signed by the address's stake key to validate it.

Now, with Plutus, the Shelley-based model of addresses remains the same, but we are no longer limited to controlling address validation with just the keys. Instead, we can create addresses that are composed of two scripts, the payment script (which are scripts that we have been writing so far), and the stake script (which is what we will be writing here). This way, we can have arbitrary logic of scripts take over both the payment and the stake part of addresses. We can also make all other possible permutations to build addresses. For example, we can use a payment script and a staking key to create an address that uses script logic to spend its UTxO, but the staking actions of the address are controlled by a staking key. We can also make an address that uses a payment key and a stake validator and so on (there are only 2 other possible options). Beautiful.

To confirm this, check the `cardano-cli address build` command:

```bash
Build a Shelley payment address, with optional delegation to a stake address.

Available options:
  --payment-verification-key STRING
                           Payment verification key (Bech32-encoded)
  --payment-verification-key-file FILE
                           Filepath of the payment verification key.
  --payment-script-file FILE
                           Filepath of the payment script.
  --stake-verification-key STRING
                           Stake verification key (Bech32 or hex-encoded).
  --stake-verification-key-file FILE
                           Filepath of the staking verification key.
  --stake-script-file FILE Filepath of the staking script.
  --mainnet                Use the mainnet magic id.
  --testnet-magic NATURAL  Specify a testnet magic id.
  --out-file FILE          Optional output file. Default is to write to stdout.
  -h,--help                Show this help text
```

It gives the option to provide either the keys or a script file for both payment and stake parts (`--payment-script-file`/`--stake-script-file`).

`StakeValidator` has a slightly different type signature than the regular validators because it does not accept `datum` as the first argument. That makes sense since the datum sits at the UTxO, and we are not dealing with spending UTxO here, only with validating staking actions. Therefore, it only receives the `redeemer` and `context` to return a boolean value:

`type UntypedStakeValidator = BuiltinData -> BuiltinData -> ()`

So when writing our `mkStakingValidator` function, we would use that type signature. Principles of [typed ](broken-reference)and [parameterised ](broken-reference)validators still apply here so we can write a function in this way: `mkStakingValidator :: MyParam -> MyRedeemer -> ScriptContext -> Bool`

Going back to the `Certifying` and `Rewarding` script purposes, we can see that they are constructed as `Rewarding StakingCredential` and `Certifying DCert`:

```haskell
data ScriptPurpose
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert
```

If we dive into the `Rewarding StakingCredential`, we will find the `StakingHash` constructor with the generic `Credential` type (the other is a pointer address, which we will not go into here, but here is a reference for those interested: [https://docs.cardano.org/learn/cardano-addresses](https://docs.cardano.org/learn/cardano-addresses)):

```haskell
data StakingCredential
    = StakingHash Credential
    | StakingPtr Integer Integer Integer
```

The `Credential` type is either a `PubKeyHash` or a `ValidatorHash` depending on whether it's a _normal_ or _script_ address:

```haskell
data Credential
  = PubKeyCredential PubKeyHash
  | ScriptCredential ValidatorHash
```

For `Certifying DCert`, we dive into `DCert` definition:

```haskell
data DCert
  = DCertDelegRegKey StakingCredential
  | DCertDelegDeRegKey StakingCredential
  | DCertDelegDelegate
      StakingCredential
      -- ^ delegator
      PubKeyHash
      -- ^ delegatee
  | -- | A digest of the PoolParams
    DCertPoolRegister
      PubKeyHash
      -- ^ poolId
      PubKeyHash
      -- ^ pool VFR
  | -- | The retiremant certificate and the Epoch N
    DCertPoolRetire PubKeyHash Integer -- NB: Should be Word64 but we only have Integer on-chain
  | -- | A really terse Digest
    DCertGenesis
  | -- | Another really terse Digest
    DCertMir
```

This is the full definition of `DCert` and it contains fields not really related to delegation per se (`DCertGenesis` and `DCertMir`). We can also see certificates related to pool registration/de-registration which we are not interested in from the POV of `StakeValidator` (`DCertPoolRetire` and `DCertPoolRegister`). This leaves just the ones related to the delegation from the delegator's POV, which is the `StakeValidator` POV:

```haskell
data DCert
  = DCertDelegRegKey StakingCredential
  | DCertDelegDeRegKey StakingCredential
  | DCertDelegDelegate
      StakingCredential
      -- ^ delegator
      PubKeyHash
      -- ^ delegatee
```

`DCertDelegRegKey StakingCredential` -> certificate for registering a stake address. In our case, a script stake address where `StakingCredential` will correspond to our script's credential, i.e. the validator hash.

`DCertDelegDeRegKey StakingCredential` -> certificate for de-registering a stake address.

`DCertDelegDelegate StakingCredential PubKeyHash` -> certificate for delegating the `StakingCredential` to the pool with the specified `PubKeyHash`.
