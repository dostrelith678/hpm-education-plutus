# A Pay-to-Store Policy

This script will be a parameterised policy script that allows the minting of tokens if a certain amount of ADA is paid to the store address as part of the minting transaction.

### Writing the validator

We can start by creating the `TokenSaleParams` that will consist of the store address, the token name, and the token price:

```haskell
data TokenSaleParams = TokenSaleParams
    {
        store :: PlutusV2.Address,
        tName :: PlutusV2.TokenName,
        tPrice :: Integer -- price in ADA per token
    } 
PlutusTx.unstableMakeIsData ''TokenSaleParams
PlutusTx.makeLift ''TokenSaleParams
```

The next part is writing the `mkPolicy` function that will represent our minting logic. We will need quite a bit of helper functions here. First, let's deconstruct `info` and `txOuts` from the transaction context.

```haskell
mkPolicy ::  TokenSaleParams -> BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy tsp _ ctx  =  traceIfFalse "Invalid mint" checkMint
    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

        txOuts :: [PlutusV2.TxOut]
        txOuts = PlutusV2.txInfoOutputs info
```

We want a `checkMint` function that will look at the minting field of the transaction (`PlutusV2.txInfoMint info`) and validate only if there is **only one particular token being minted**, that token **matches our `CurrencySymbol` and `TokenName`** and **the amount minted is valid** for the amount of ADA being paid to the store address.&#x20;

We will call this last variable `canMintAmount` and it will simply divide (using integer division) the amount of ADA paid to the store (`storeTxOutAdaValue`) by the price of the token. Getting the `storeTxOutAdaValue` will be slightly complicated. First, we will need to look at all the UTxOs of the transactions that go to the store address via `storeTxOuts :: [PlutusV2.TxOut]`. That will give us a list of UTxOs that we then have to filter for their values `storeTxOutValue :: PlutusV2.Value` and then filter that for only the ADA (Lovelace) values and also sum them all together via `storeTxOutLovelaceValue :: Integer`. We then simply need to turn that into the corresponding ADA value by dividing it by a million.

```haskell
...
        checkMint :: Bool
        checkMint = case PlutusV1.flattenValue (Plutus.txInfoMint info) of
          [(cs', tn', amt)] -> tn' == tn && cs' == cs && amt == canMintAmount
          _               -> False

        cs :: Plutus.CurrencySymbol
        cs = ownCurrencySymbol ctx

        tn :: Plutus.TokenName
        tn = tName tsp
        
        canMintAmount :: Integer
        canMintAmount = storeTxOutAdaValue `divide` tPrice tsp
        
        storeTxOuts :: [Plutus.TxOut]
        storeTxOuts = filter (\x -> Plutus.txOutAddress x == store tsp) txOuts

        storeTxOutValue :: Plutus.Value
        storeTxOutValue = mconcat (fmap Plutus.txOutValue storeTxOuts)

        storeTxOutLovelaceValue :: Integer
        storeTxOutLovelaceValue = getLovelaceQuantity (PlutusV1.flattenValue storeTxOutValue)

        storeTxOutAdaValue :: Integer
        -- note must use PLutusTx DIVIDE instead of DIV
        storeTxOutAdaValue = storeTxOutLovelaceValue `divide` 1_000_000

        getLovelaceQuantity :: [(Plutus.CurrencySymbol, Plutus.TokenName, Integer)] -> Integer
        getLovelaceQuantity = foldr (\(ccs, _, qt) -> if ccs == Plutus.adaSymbol then (qt +) else (0 + )) 0

```

To use the underscore format for large numbers to make them more readable such as `1_000_000`, we have to activate the extension `{-# LANGUAGE NumericUnderscores #-}`.  Here is the full list of extensions and imports used for this validator for reference:

```haskell
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NumericUnderscores  #-}

module MintingPolicy
  (
    scriptSerialised,
    writeSerialisedScript,
  )
where

import qualified PlutusTx
import PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as Plutus

import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2, writeFileTextEnvelope)
import Cardano.Api (FileError)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise

import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Script.Utils.V2.Contexts (ownCurrencySymbol)

import qualified Plutus.V1.Ledger.Value as PlutusV1

import Prelude (IO)
```

Once our logic is complete, we need to apply our parameter to the function and create a `MintingPolicy` via `PlutusV2.mkMintingPolicyScript`. Since we are creating a parameterised script, we need to lift and apply our `tsp` parameter to our `mkPolicy` function as we did with our [`StakingValidator`](../stake-validators/a-stake-validator-script.md):

```haskell
policy :: TokenSaleParams -> Scripts.MintingPolicy
policy tsp = Plutus.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode tsp
  where
    wrap tsp' = Scripts.mkUntypedMintingPolicy $ mkPolicy tsp'
```

To use this `MintingPolicy`, we need to serialise it as a script first. We can get the `Script` type via `unMintingPolicyScript`. We can create and apply the parameter here directly (using the `02.addr` as the store address):

```haskell
myTsp :: TokenSaleParams
myTsp = TokenSaleParams {
  store = Plutus.Address (Plutus.PubKeyCredential "1b1e5895b03302b248e8c459817bab49471c4013a0806ac52cb73f9b") Nothing,
  tName = "HPM",
  tPrice = 10
}

script :: Plutus.Script
script = Plutus.unMintingPolicyScript $ policy myTsp
```

And serialising the script as usual:

```haskell
scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

scriptSerialised :: PlutusScript PlutusScriptV2
scriptSerialised = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO (Either (FileError ()) ())
writeSerialisedScript = writeFileTextEnvelope "compiled/MintingPolicy.plutus" Nothing scriptSerialised
```

Finally, we can load the module in `cabal repl` from our `nix-shell` and compile the minting policy.

```haskell
Prelude> :l src/MintingPolicy.hs
[1 of 1] Compiling MintingPolicy    ( src/MintingPolicy.hs, /home/plutus/hpm-plutus/hpm-validators/dist-newstyle/build/x86_64-linux/ghc-8.10.7/hpm-validators-0.1.0.0/build/MintingPolicy.o )
Ok, one module loaded.
Prelude MintingPolicy> writeSerialisedScript
Right ()
```





### Testing the validator

To create the transactions for testing, we need to first get the policy ID from the policy script. From a new directory under `testnet/MintingPolicy/` the command looks like this:

```bash
cardano-cli transaction policyid --script-file ../../compiled/MintingPolicy.plutus
a18972b3b83c9ff2f048380048cfdd28752f5c7430b75678065e3098
```

Also, `cardano-cli` accepts only hex token names, so before we can use it as an argument, we need to hexlify our token name via:

```bash
echo -n "HPM" | xxd -ps
48504d
```

Finally, to test this minting policy we can write our usual testing scripts. The `check-utxos.sh` scripts lists UTxOs available at `01.addr` (customer) and `02.addr` (store).

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

customer=$(cardano-cli query utxo \
--address $(cat ../normal_address/01.addr) \
--testnet-magic $NWMAGIC)

store=$(cardano-cli query utxo \
--address $(cat ../normal_address/02.addr) \
--testnet-magic $NWMAGIC)

echo "Customer UTxOs:"
echo "${customer}"
echo ""

echo "Store UTxOs"
echo "${store}"
echo ""
```

```bash
./check-utxos.sh 
Customer UTxOs:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5dc5111e257f8e68b0978c9619e57bbb12d365c0ec45d879115866bb674156ae     0        1826915 lovelace + TxOutDatumNone
d20a1e9707e9fbd9adf606e9e9e168cfd9969431defc5869c4424f38673dddc5     0        10000000000 lovelace + TxOutDatumNone
ede24e9e40ca82830c75d827b5c3b090132c1afaebd3a4256655fb5d2382474a     0        9649776 lovelace + TxOutDatumNone
ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1     0        1829006 lovelace + TxOutDatumNone

Store UTxOs
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1bbb408f6cc96fd12de602539aa81989c3778d712132fa7a95de9f48ebf2e4ed     0        19684501 lovelace + TxOutDatumNone
59590fab00fb430d205151c59ca7e00af38e9945d778abdae6897f368aa39591     0        19682109 lovelace + TxOutDatumNone
667f81c9a89946d83f5975d9d97534df42be85a5a5aa1161b7af0ecb3d6592d0     0        19673177 lovelace + TxOutDatumNone
```

We can first test the minting policy by trying to mint an invalid number of tokens for the price. Let's say we want 10 tokens (price 100 ADA), but we only pay 90 ADA in our `mint-tokens-invalid.sh` script. To create minting transactions, we use the `--mint` argument which has the following syntax:

```bash
--mint VALUE             Mint multi-asset value(s) with the multi-asset cli
                           syntax. You must specify a script witness.

--mint "<TOKEN_QUANTITY> <TOKEN_POLICY_ID>.<TOKEN_NAME>"
```

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/01.addr) \
    --tx-in 0ecbdedf2f535c9931e4b36aa2b13fac93e9afa84d9b4797cc93ee24d42922fc#2 \
    --tx-in-collateral ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1#0 \
    --tx-out $(cat ../address/02.addr)+90000000 \
    --mint "10 a18972b3b83c9ff2f048380048cfdd28752f5c7430b75678065e3098.48504d" \
    --mint-script-file ../../compiled/MintingPolicy.plutus \
    --mint-redeemer-value [] \
    --out-file tx.body
```

If we try running it, we get an `Invalid mint` error message that we defined in our validator:

```bash
./mint-tokens-invalid.sh 
Command failed: transaction build  Error: The following scripts have execution failures:
the script for policyId 0 (in ascending order of the PolicyIds) failed with: 
The Plutus script evaluation failed: An error has occurred:  User error:
The machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.
Script debugging logs: Invalid mint
```

Now, let's try paying the right amount for the 10 tokens in `mint-tokens-valid.sh`.

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/01.addr) \
    --tx-in d20a1e9707e9fbd9adf606e9e9e168cfd9969431defc5869c4424f38673dddc5#0 \
    --tx-in-collateral ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1#0 \
    --tx-out $(cat ../address/02.addr)+100000000 \
    --tx-out $(cat ../address/01.addr)+2000000+"10 a18972b3b83c9ff2f048380048cfdd28752f5c7430b75678065e3098.48504d" \
    --mint "10 a18972b3b83c9ff2f048380048cfdd28752f5c7430b75678065e3098.48504d" \
    --mint-script-file ../../compiled/MintingPolicy.plutus \
    --mint-redeemer-value [] \
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

```bash
./mint-tokens-valid.sh 
Estimated transaction fee: Lovelace 387496
Transaction successfully submitted.
```

Checking the UTxO state after this transaction shows that the `Store` received the 100 ADA payment and the `Customer` received 10 minted tokens!

```bash
./check-utxos.sh 
Customer UTxOs:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
5dc5111e257f8e68b0978c9619e57bbb12d365c0ec45d879115866bb674156ae     0        1826915 lovelace + TxOutDatumNone
ede24e9e40ca82830c75d827b5c3b090132c1afaebd3a4256655fb5d2382474a     0        9649776 lovelace + TxOutDatumNone
ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1     0        1829006 lovelace + TxOutDatumNone
fbbf7a532ff30176087966c129f8fe44aa9e3462e4224e4fbfe3e162b1569ded     1        2000000 lovelace + 10 a18972b3b83c9ff2f048380048cfdd28752f5c7430b75678065e3098.48504d + TxOutDatumNone
fbbf7a532ff30176087966c129f8fe44aa9e3462e4224e4fbfe3e162b1569ded     2        9897612504 lovelace + TxOutDatumNone

Store UTxOs
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
1bbb408f6cc96fd12de602539aa81989c3778d712132fa7a95de9f48ebf2e4ed     0        19684501 lovelace + TxOutDatumNone
59590fab00fb430d205151c59ca7e00af38e9945d778abdae6897f368aa39591     0        19682109 lovelace + TxOutDatumNone
667f81c9a89946d83f5975d9d97534df42be85a5a5aa1161b7af0ecb3d6592d0     0        19673177 lovelace + TxOutDatumNone
fbbf7a532ff30176087966c129f8fe44aa9e3462e4224e4fbfe3e162b1569ded     0        100000000 lovelace + TxOutDatumNone
```
