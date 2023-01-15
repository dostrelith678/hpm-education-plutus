# Minting Policies

Moving on to minting policies... These are very similar to `StakeValidator`s in the sense that they also do not receive `datum`, only the `redeemer` and `context`. This type of policy is used for the `Minting CurrencySymbol` script purpose. It returns a `()` representing a valid mint or throws an error if validation fails. But when using typed minting policy scripts, the function should return a bool instead as before.

Tokens on Cardano are made up of the `CurrencySymbol` and the token name. One `CurrencySymbol` can hold and infinite number of `TokenName`s. Token names are generally hexlyfied bytestrings, but have ASCII representation as well. The `CurrencySymbol` is the hash of the minting policy that controls token minting / burning. Before Plutus, in the Mary era, these policies were introduced and simple timelocking scripts etc. were already possible. Now with Plutus, we can control the minting policy with arbitrary logic instead of just key witnesses and timelocks. So the `CurrencySymbol` is the hash of the minting policy, whether it's a Plutus policy script or a Mary-era style policy (this is also referred to as the `policy ID`).

The `PlutusV2.mkMintingPolicyScript` accepts a function of `BuiltinData -> BuiltinData -> ()` of `CompiledCode` and returns a `MintingPolicy` - a wrapper around the `Script` type:

`mkMintingPolicyScript :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> MintingPolicy`

We will write two scripts in this section, first a pay-to-store-to-mint kind of script, and an NFT minting script.

## Pay-to-store policy

This script will be a parameterised policy script that allows minting of tokens if a certain amount of ADA is paid to the store address as part of the minting transaction. We can start by creating the `TokenSaleParams` that will consist of the store address, the token name and the token price:

```
data TokenSaleParams = TokenSaleParams
    {
        store :: PlutusV2.Address,
        tName :: PlutusV2.TokenName,
        price :: Integer -- price of ada per token
    } 
PlutusTx.unstableMakeIsData ''TokenSaleParams
PlutusTx.makeLift ''TokenSaleParams
```

The next part is writing the `mkPolicy` function that will represent our minting logic. We will need quite a bit of helper functions here. first we deconstruct `info` and `txOuts` of the transaction. The `checkMint` will look at the minting field of the transaction (`PlutusV2.txInfoMint info`) and validated only if there is only one token being minted, that token matches our `CurrencySymbol` and `TokenName` and the amount minted is valid for the amount of ADA being paid to the store address. We will call this last variable `canMintAmount` and it will simply divide (integer division) the amount of ADA paid to the store (`storeTxOutAdaValue`) by the price of the token. To get the `storeTxOutAdaValue` will be slightly complicated. First we will need to look at all the UTxOs of the transactions that go to the store address via `storeTxOuts :: [PlutusV2.TxOut]`. That will give us a list of UTxOs that we then have to filter its values `storeTxOutValue :: PlutusV2.Value` and then filter that for only the ADA (lovelace) values and also sum them all together via `storeTxOutLovelaceValue :: Integer`.


```
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

Once our logic is complete, we need to apply our parameter to the function and create a `MintingPolicy` via `PlutusV2.mkMintingPolicyScript`: I need to explain this better

```
policy :: TokenSaleParams -> Scripts.MintingPolicy
policy tsp = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode tsp
  where
    wrap tsp' = Scripts.mkUntypedMintingPolicy $ mkPolicy tsp'
```

We get the `Script` type via `unMintingPolicyScript`. We will create and apply the parameter here directly (using store as `02.addr`):
```
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
```
scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "compiled/token-sale.plutus" Nothing serialisedScript
```

To test this minting policy we have some helper scripts:

```
check-utxos.sh
mint-tokens-valid.sh
mint-tokens-invalid.sh
```

To create the transaction in the above scripts, we need to first get the policy ID from the policy script:

```
cardano-cli transaction policyid --script-file token-sale.plutus 
a8b5a7e8b2dd90a209bc92a49f2b5f5b4ea9e6a0cdca73dbabb8f2db
```

Also, `cardano-cli` accepts only hex token names, so we need to hexlify our token name via:
```
echo -n "HPM" | xxd -ps
48504d
```

## NFT policy

