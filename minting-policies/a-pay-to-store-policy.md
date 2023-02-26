# A Pay-to-Store Policy

This script will be a parameterised policy script that allows the minting of tokens if a certain amount of ADA is paid to the store address as part of the minting transaction. We can start by creating the `TokenSaleParams` that will consist of the store address, the token name, and the token price:

```haskell
data TokenSaleParams = TokenSaleParams
    {
        store :: PlutusV2.Address,
        tName :: PlutusV2.TokenName,
        price :: Integer -- price in ADA per token
    } 
PlutusTx.unstableMakeIsData ''TokenSaleParams
PlutusTx.makeLift ''TokenSaleParams
```

The next part is writing the `mkPolicy` function that will represent our minting logic. We will need quite a bit of helper functions here. First, we deconstruct `info` and `txOuts` of the transaction.&#x20;

The `checkMint` will look at the minting field of the transaction (`PlutusV2.txInfoMint info`) and validate only if there is only one particular token being minted, that token matches our `CurrencySymbol` and `TokenName` and the amount minted is valid for the amount of ADA being paid to the store address.&#x20;

We will call this last variable `canMintAmount` and it will simply divide (using integer division) the amount of ADA paid to the store (`storeTxOutAdaValue`) by the price of the token. Getting the `storeTxOutAdaValue` will be slightly complicated. First, we will need to look at all the UTxOs of the transactions that go to the store address via `storeTxOuts :: [PlutusV2.TxOut]`. That will give us a list of UTxOs that we then have to filter for their values `storeTxOutValue :: PlutusV2.Value` and then filter that for only the ADA (lovelace) values and also sum them all together via `storeTxOutLovelaceValue :: Integer`.

```haskell
mkPolicy ::  TokenSaleParams -> BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy tsp _ ctx  =  traceIfFalse "Invalid mint" checkMint
    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

        txOuts :: [PlutusV2.TxOut]
        txOuts = PlutusV2.txInfoOutputs info

        checkMint :: Bool
        checkMint = case PlutusV1.flattenValue (PlutusV2.txInfoMint info) of
          [(cs', tn', amt)] -> tn' P.== tn && cs' P.== cs && amt P.== canMintAmount
          _               -> False

        cs :: PlutusV2.CurrencySymbol
        cs = PlutusV2.ownCurrencySymbol ctx

        tn :: PlutusV2.TokenName
        tn = tName tsp
        
        canMintAmount :: Integer
        canMintAmount = storeTxOutAdaValue `divide` price tsp
        
        storeTxOuts :: [PlutusV2.TxOut]
        storeTxOuts = filter (\x -> PlutusV2.txOutAddress x P.== store tsp) txOuts

        storeTxOutValue :: PlutusV2.Value
        storeTxOutValue = mconcat (fmap PlutusV2.txOutValue storeTxOuts)

        storeTxOutLovelaceValue :: Integer
        storeTxOutLovelaceValue = getLovelaceQuantity (PlutusV1.flattenValue storeTxOutValue)

        storeTxOutAdaValue :: Integer
        storeTxOutAdaValue = storeTxOutLovelaceValue `divide` 1_000_000 -- note must use PLutusTx DIVIDE instead of DIV

        getLovelaceQuantity :: [(PlutusV2.CurrencySymbol, PlutusV2.TokenName, Integer)] -> Integer
        getLovelaceQuantity = foldr (\(ccs, _, qt) -> if ccs P.== PlutusV2.adaSymbol then (qt +) else (0 + )) 0
```

Once our logic is complete, we need to apply our parameter to the function and create a `MintingPolicy` via `PlutusV2.mkMintingPolicyScript`. Since we are creating a parameterised script, we need to lift and apply our `tsp` parameter to our `mkPolicy` function:

```haskell
policy :: TokenSaleParams -> Scripts.MintingPolicy
policy tsp = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode tsp
  where
    wrap tsp' = Scripts.mkUntypedMintingPolicy $ mkPolicy tsp'
```

We get the `Script` type via `unMintingPolicyScript`. We will create and apply the parameter here directly (using the `02.addr` as the store address):

```haskell
myTsp :: TokenSaleParams
myTsp = TokenSaleParams {
  store = Address (PlutusV2.PubKeyCredential "1dbbab8486140e253674dd2af159c322c5c48232b1a358670b1ef5a7") Nothing,
  tName = "HPM",
  price = 10
}

script :: PlutusV2.Script
script = PlutusV2.unMintingPolicyScript $ policy myTsp
```

And serialising the script as usual:

```haskell
scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "compiled/token-sale.plutus" Nothing serialisedScript
```

To create the transactions for testing, we need to first get the policy ID from the policy script:

```bash
cardano-cli transaction policyid --script-file token-sale.plutus 
a8b5a7e8b2dd90a209bc92a49f2b5f5b4ea9e6a0cdca73dbabb8f2db
```

Also, `cardano-cli` accepts only hex token names, so we need to hexlify our token name via:

```bash
echo -n "HPM" | xxd -ps
48504d
```

Finally, to test this minting policy we have some helper scripts:

1\) `check-utxos.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

funds_normal1=$(cardano-cli query utxo \
--address $(cat ../normal_address/01.addr) \
--testnet-magic $NWMAGIC)

funds_normal2=$(cardano-cli query utxo \
--address $(cat ../normal_address/02.addr) \
--testnet-magic $NWMAGIC)

echo "Normal address1:"
echo "${funds_normal1}"
echo ""

echo "Normal address2:"
echo "${funds_normal2}"
echo ""
```

2\) `mint-tokens-invalid.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../normal_address/01.addr) \
    --tx-in 0ecbdedf2f535c9931e4b36aa2b13fac93e9afa84d9b4797cc93ee24d42922fc#2 \
    --tx-in-collateral 313ba91e11efac7827d057613251273e193b71d995592ff0108252b19c2d0e26#0 \
    --tx-out $(cat ../normal_address/02.addr)+35000000 \
    --tx-out $(cat ../normal_address/01.addr)+2000000+"2 a8b5a7e8b2dd90a209bc92a49f2b5f5b4ea9e6a0cdca73dbabb8f2db.48504d" \
    --mint "2 a8b5a7e8b2dd90a209bc92a49f2b5f5b4ea9e6a0cdca73dbabb8f2db.48504d" \
    --mint-script-file token-sale.plutus \
    --mint-redeemer-value [] \
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

3\) `mint-tokens-valid.sh`

```bash
#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../normal_address/01.addr) \
    --tx-in 0a47f8a0afc05dc1197c736974cee7a93e4e3f5d996ac79ce0283f902dbafac7#1 \
    --tx-in-collateral 313ba91e11efac7827d057613251273e193b71d995592ff0108252b19c2d0e26#0 \
    --tx-out $(cat ../normal_address/02.addr)+20000000 \
    --tx-out $(cat ../normal_address/01.addr)+2000000+"2 a8b5a7e8b2dd90a209bc92a49f2b5f5b4ea9e6a0cdca73dbabb8f2db.48504d" \
    --mint "2 a8b5a7e8b2dd90a209bc92a49f2b5f5b4ea9e6a0cdca73dbabb8f2db.48504d" \
    --mint-script-file token-sale.plutus \
    --mint-redeemer-value [] \
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
