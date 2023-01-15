# Stake validators

In this section, we will look at validator scripts as they relate to staking on Cardano. The scripts we looked at so far were only related to payment part of addresses, i.e. used for the `Spending` part of `ScriptPurpose`. We will now look at `StakeValidator`s which correspond to `Certifying` and `Rewarding` script purposes. The `Rewarding` script purpose is related to validating reward withdrawals, i.e. is a transaction allowed to withdraw rewards from the script stake address. The `Certifying` script purpose is related to validating certificates present in the transaction which control the registration and delegation of the script stake address, i.e. validating whether the transaction can register/de-register the given script stake address and whether its funds to the specified stake pool.

Here we see a nice overview of Plutus scripts on Cardano, where arbitrary logic takes over key witnesses. A Cardano base address (Shelley-based) is composed of two parts, the payment part (which controls spending associated UTxOs) and the *optional* staking part (which controls delegation-related actions). These two parts have separate keys (payment and stake keys), that are used to sign transactions to prove that they are valid. For example, if we want to register and delegate a stake address to a pool, we must submit a transaction that is signed by the address's stake key to validate it.

Now, with Plutus, the Shelley-based model of addresses remains the same, but we are no longer limited to control address validation with just the keys. Instead, we can create addresses that are composed of two scripts, the payment script (which are scripts that we have been writing so far), and the stake script (which is what we will be writing here). This way, we can have arbitrary logic of scripts take over both the payment and the stake part of addresses. We can also make all other possible permutations to build addresses. For example, we can use a payment script and a staking key to create an address that uses script logic to spend its UTxO, but the staking actions of the address are controlled by a staking key. We can also make an address that uses a payment key and a stake validator and so on (there are only 2 other possible options). Beautiful.

To confirm this, check the `cardano-cli address build` command:
```
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

`StakeValidator` has a slightly different type signature than the regular because it does not accept `datum` as the first argument. That makes sense since datum sits at the UTxO, we are not dealing with spending UTxO here, only with validating staking actions. Therefore, it only receives the `redeemer` and `context` to return a boolean value:

`type UntypedStakeValidator = BuiltinData -> BuiltinData -> ()`

So when writing our `mkStakingValidator` function, we would use that type signature. Principles of typed and parameterised validators still apply here so we can write a function in this way:
`mkStakingValidator :: MyParam -> MyRedeemer -> ScriptContext -> Bool`

Going back to the `Certifying` and `Rewarding` script purposes, we can see that they are constructed as `Rewarding StakingCredential` and `Certifying DCert`:
```
data ScriptPurpose
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert
```
If we dive into the `Rewarding StakingCredential`, we will find the `StakingHash` constructor with the generic `Credential` type (the other is a pointer address, not going into it here, but here is some reference: https://docs.cardano.org/learn/cardano-addresses):
```
data StakingCredential
    = StakingHash Credential
    | StakingPtr Integer Integer Integer
```

The `Credential` type is either a `PubKeyHash` or a `ValidatorHash` depending on whether it's a *normal* or *script* address:
```
data Credential
  = PubKeyCredential PubKeyHash
  | ScriptCredential ValidatorHash
```

For `Certifying DCert`, we dive into `DCert` definition:

```
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
This is the full definition of `DCert` and it contains fields not really related to delegation per-se (`DCertGenesis` and `DCertMir`). We can also see certificates related to pool registration/de-registration which we are not interested in from the POV of `StakeValidator` (`DCertPoolRetire` and `DCertPoolRegister`). This leaves just the ones related to delegation from the delegator POV, which is the `StakeValidator` POV:
```
data DCert
  = DCertDelegRegKey StakingCredential
  | DCertDelegDeRegKey StakingCredential
  | DCertDelegDelegate
      StakingCredential
      -- ^ delegator
      PubKeyHash
      -- ^ delegatee
```

`DCertDelegRegKey StakingCredential` -> certificate for registering a stake address. In our case a script stake address where `StakingCredential` will correspond to our scripts credential, i.e. the validator hash.

`DCertDelegDeRegKey StakingCredential` -> certificate for de-registering a stake address.

`DCertDelegDelegate StakingCredential PubKeyHash` -> certificate for delegating the `StakingCredential` to the pool with the specified `PubKeyHash`.

## Example stake validator script

In this `StakeValidator` example, we will create a simple script that controls staking actions via secret codes that the script is parameterised with. We will create the `CodeParam` as before:
```
data CodeParam = CodeParam {
  cert :: Integer,
  reward :: Integer
}
PlutusTx.unstableMakeIsData ''CodeParam
PlutusTx.makeLift ''CodeParam
```
The `cert` field will contain an `Integer` code that must be specified in the redeemer for script certificates to be validated, and the `reward` code will have to be specified in order to withdraw rewards. We will be writing this as a typed validator so we need a `ValidatorTypes` instance as well:
```
data CodeValidator
instance PSU.ValidatorTypes CodeValidator where
  type instance RedeemerType CodeValidator = Integer
  -- we only care about the redeemer type for the stake validator
```

We have everything to create the `mkStakingValidator` function which will represent our stake validator logic:
```
mkStakingValidator :: CodeParam -> Integer -> ScriptContext -> Bool
mkStakingValidator cp redeemer ctx = 
  case scriptContextPurpose ctx of
    Certifying _   -> redeemer == cert cp
    Rewarding  _   -> redeemer == reward cp
    _              -> False
```
We simply need check that the `redeemer` matches the corresponding action. Any script purpose other than `Certifying` or `Rewarding` will always be `False`. Next, we need to compile that function into a `StakeValidator`. Since there is still no interface for `TypedStakeValidator`, we have to use somewhat explicit code with `mkStakeValidatorScript` and `mkUntypedStakeValidator`. This function is defined as:
```
mkUntypedStakeValidator
    :: PV1.UnsafeFromData r
    => (r -> sc -> Bool)
    -> UntypedStakeValidator
mkUntypedStakeValidator f r p =
    check $ f (PV1.unsafeFromBuiltinData r) (PV1.unsafeFromBuiltinData p)
```
It receives a function `(r -> sc -> Bool)` and returns the `UntypedStakeValidator` which is our end result we want here. Since this is a parameterised contract, we have to apply our `CodeParam` to the `mkStakingValidator` function first in order to get just the `(Integer -> ScriptContext -> Bool)` function that is required. That is why we have to compose the two functions together and *apply* the `CodeParam`. However, since this is all being compiled to Plutus IR (intermediate Plutus Core), we also have to first *lift* the `CodeParam` value to Plutus IR for it to be applied. We do this with `PlutusTx.liftCode cp`, and we are able to do it because we made `CodeParam` and instance of the `Lift` class! In the previous examples, this was abstracted for us via the `mkTypedValidator` function, but since one is not available for stake validators (yet), we have to do it manually here:

```
validator :: CodeParam -> V2.StakeValidator
validator cp = V2.mkStakeValidatorScript $
  $$(PlutusTx.compile [|| mkUntypedStakeValidator . mkStakingValidator ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode cp
```

The rest is the same as before, the only difference being the function names, and instead of `unValidatorScript`, we use `unStakeValidatorScript` to get the `Script` type of the validator:

```
script :: CodeParam -> V2.Script
script = V2.unStakeValidatorScript . validator

codeParamShortBs :: CodeParam -> SBS.ShortByteString
codeParamShortBs cp = SBS.toShort . LBS.toStrict $ serialise $ script cp

codeParamScriptSerialised :: CodeParam -> PlutusScript PlutusScriptV2
codeParamScriptSerialised cp = PlutusScriptSerialised $ codeParamShortBs cp

writeSerialisedCodeParamScript :: CodeParam -> IO (Either (FileError ()) ())
writeSerialisedCodeParamScript cp = writeFileTextEnvelope "compiled/codeParam.plutus" Nothing $ codeParamScriptSerialised cp
```

Helper scripts:
```
create-script-address.sh
check-utxos.sh
send-funds-to-script.sh
register-and-delegate-script.sh
regelegate-invalid.sh
withdraw-rewards.sh (todo after some epochs)
```
