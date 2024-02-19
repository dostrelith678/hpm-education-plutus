# The Simplest Script

As mentioned in the [EUTxO overview](../the-eutxo-model/introduction-to-the-eutxo-model/the-eutxo-model-extended-utxo-model.md), the validator script receives three arguments:

1. `Datum`
2. `Redeemer`
3. `Context`

The Haddock documentation for Plutus specifies the main modules ([https://intersectMBO.github.io/plutus/master/](https://intersectMBO.github.io/plutus/master/)):

```
PlutusTx: Compiling Haskell to PLC (Plutus Core; on-chain code).

PlutusTx.Prelude: Haskell prelude replacement compatible with PLC.

PlutusCore: Programming language in which scripts on the Cardano blockchain are written.

UntypedPlutusCore: On-chain Plutus code.
```

The two modules that we will be importing into our Haskell files are `PlutusTx` and `PlutusTx.Prelude`. We start with a new `SimplestSuccess.hs` file. We will **write the simplest contract that successfully validates every attempt to spend its funds**.

### Writing the validator

First, we will need to add some GHC extensions at the start of the file:

```haskell
{-# LANGUAGE DataKinds         #-} -- make any type constructor into a type
{-# LANGUAGE NoImplicitPrelude #-} -- do not import Prelude by default
{-# LANGUAGE TemplateHaskell   #-} -- allows embedding domain-specific language into the Haskell host language
```

The `DataKinds` extension is needed for some Template Haskell features or the `PlutusTx` compilation will fail. `NoImplicitPrelude` states not to import the Haskell Prelude. We always need to specify this as `PlutusTx` has its own Prelude that we have to use. When writing the validator to a file, we still need to use the `IO` monad from the original Prelude, which we can import explicitly. The `TemplateHaskell` extension is simply to allow us to write Template Haskell expressions to be able to properly use `PlutusTx.compile` inside our module.

Next, we define our module name (same as the filename):

<pre class="language-haskell"><code class="lang-haskell"><strong>module SimplestSuccess
</strong>  (
    successScriptSerialised,
    writeSerialisedSuccessScript
  ) 
where
</code></pre>

Next, we need to import the packages required for the compilation of our script. Note that these must be defined in our `.cabal` file in order to be imported here. For now, these will be:

```haskell
import qualified PlutusTx                           -- main on-chain code module
import qualified PlutusTx.Prelude     as Prelude    -- Prelude replacement for Plutus
import qualified Plutus.V2.Ledger.Api as Plutus     -- functions for working with scripts on the ledger

import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV2, writeFileTextEnvelope)
import Cardano.Api (FileError)

-- Hackage packages
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise

import Prelude (IO)
```

In general, we want to **write three major parts** of our Haskell file:

1. The `mkValidator` function that contains our validation logic.
2. Compilation of that function to a Plutus Core script (the on-chain language). This is done by using [template Haskell](http://wiki.haskell.org/Template\_Haskell).
3. Serialise and write the script to a `.plutus` file.

When we write a validator we define a function that receives the three aforementioned arguments and **returns a unit `()` if successful**. Not returning a `()` means that the validation failed and the transaction will be invalidated. With that in mind, we can start to think about the type signature of the validator function, something like:

`mkValidator :: Datum -> Redeemer -> Context -> ()`.

But what are the types of `Datum`, `Redeemer` and `Context`? It turns out that in Plutus, all three of the validation arguments need to come in a type of `Data`. We can explore the Haddock pages to learn more about it: [https://intersectMBO.github.io/plutus/master/plutus-core/html/PlutusCore-Data.html#t:Data](https://intersectMBO.github.io/plutus/master/plutus-core/html/PlutusCore-Data.html#t:Data).

We see that the `Data` type comes with several constructors, but the main takeaway is that it is a generic data type that can represent various things such as integers, byte strings, lists, and maps. Plutus also features a `BuiltinData` type ([https://intersectMBO.github.io/plutus/master/plutus-tx/html/PlutusTx-Builtins.html#g:4](https://intersectMBO.github.io/plutus/master/plutus-tx/html/PlutusTx-Builtins.html#g:4)) that can be used directly in the on-chain code.

So we can now write the type signature of our validator function using the `BuiltinData` type for its arguments and returning `()`:&#x20;

`mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()`

```
{-# INLINABLE mkValidator #-}
mkValidator :: Prelude.BuiltinData -> Prelude.BuiltinData -> Prelude.BuiltinData -> ()
mkValidator _ _ _ = ()
```

Since this function always returns `()` regardless of its arguments, any UTxO belonging to the script will be spendable by any transaction. We add the inlinable pragma just above the function to be able to later use it with the PlutusTx compiler directly in our Haskell code.

We now need to do part 2 of our three steps, compiling this validator function to Plutus Core:

```
validator :: Plutus.Validator
validator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

The unusual syntax above is template Haskell: `$$([|| ||])`. The function [`Plutus.mkValidatorScript`](https://intersectMBO.github.io/plutus-apps/main/plutus-ledger/html/Ledger.html#v:mkValidatorScript) requires a Plutus Core argument so the `mkValidator` is first compiled to Plutus Core. In order for this to work, the compiled function `mkValidator` must be made inlinable with `{-# INLINABLE mkValidator #-}` that we specified earlier.

Part 3 of our three steps is arguably the simplest. We need to unwrap the validator to get the script. This is just a necessary step to conform with the expected types. Since [`Plutus.Validator`](https://intersectMBO.github.io/plutus-apps/main/plutus-ledger/html/Ledger.html#t:Validator) is a wrapper around [`Plutus.Script`](https://intersectMBO.github.io/plutus-apps/main/plutus-ledger/html/Ledger.html#t:Script) which is used as the actual validator in the ledger, we need to unwrap it.

```haskell
script :: Plutus.Script
script = Plutus.unValidatorScript validator
```

We can now serialise the script to a `ShortByteString`:

```haskell
successScriptShortBs :: SBS.ShortByteString
successScriptShortBs = SBS.toShort Prelude.. LBS.toStrict Prelude.$ serialise script
```

The next step is just a type conversion again:

```haskell
successScriptSerialised :: PlutusScript PlutusScriptV2
successScriptSerialised = PlutusScriptSerialised successScriptShortBs
```

Finally, we expose a function that writes the `Plutus` script to a file that we will use with the actual blockchain:

<pre class="language-haskell"><code class="lang-haskell"><strong>writeSerialisedSuccessScript :: IO (Prelude.Either (FileError ()) ())
</strong>writeSerialisedSuccessScript = writeFileTextEnvelope "compiled/SimplestSuccess.plutus" Prelude.Nothing successScriptSerialised
</code></pre>

We can load up a `cabal repl`, and compile the script. Make sure you create the `compiled/` directory first.

```haskell
Prelude SimplestSuccess> SimplestSuccess.writeSerialisedSuccessScript 
Right ()
```

### Serialising a datum object

We now have the compiled script in `compiled/simplestSuccess.plutus`. Another thing we need is to serialise a `datum`. We need to use datums on script outputs as **any UTxO without a datum hash attached will be unspendable** as we mentioned before. We need to write a utility function for converting Plutus data to JSON because `cardano-cli` expects JSON values. Create a new file under `src/Helpers/Utils.hs`:

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Helpers.Utils
  (
    plutusDataToJSON,
    writeJSONData
  ) 
where

import qualified PlutusTx
import PlutusTx.Prelude
import Data.Aeson (encode)

import Cardano.Api.Shelley (fromPlutusData, scriptDataToJson, ScriptDataJsonSchema (ScriptDataJsonDetailedSchema))

import qualified Data.ByteString.Lazy as LBS

import Prelude (IO, String)

plutusDataToJSON :: PlutusTx.ToData a => a ->  LBS.ByteString
plutusDataToJSON = encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData

writeJSONData :: PlutusTx.ToData a => String -> a -> IO ()
writeJSONData filePath pData = LBS.writeFile filePath $ plutusDataToJSON pData
```

This is mostly boilerplate code that we don't need to think too much about. It simply takes some data of the [`ToData`](https://intersectMBO.github.io/plutus-apps/main/plutus-ledger-api/html/Plutus-V2-Ledger-Api.html#t:ToData) class and serialises it to a JSON that `cardano-cli` expects. We can now load and use this function anytime we need to write a datum file. Here, we just want to write a unit `()` datum file. Make sure you create the `compiled/assets/` directory before running the code below first.

<pre class="language-haskell"><code class="lang-haskell"><strong>Prelude> :l src/Helpers/Utils.hs
</strong><strong>Ok, one module loaded.
</strong><strong>Prelude Utils> writeJSONData "compiled/assets/unit.json" ()
</strong></code></pre>

{% hint style="info" %}
Every time we want to automatically load a module we write when launching a `cabal repl`, we can add them to our `.cabal` file in the `exposed-modules` field.

```haskell
-- hpm-validators.cabal

...
library
    hs-source-dirs:       src
    exposed-modules:      SimplestSuccess
                        , Helpers.Utils
...
```
{% endhint %}

### Testing the validator

To start testing our validators, we will need to create some regular Cardano addresses on the testnet and use the faucet to get some tADA. We will use these to pay the fees for the transactions we create as well as the collateral inputs. We will build two addresses now and use them throughout the course with different validators. Let's place all our testing files in the `testnet/` directory of the project root. Below is a `bash` script that creates the addresses for us (you can also use `cardano-cli` directly in the terminal). Make sure you create the `testnet/addresses/` directory beforehand.

{% hint style="warning" %}
_**We always test our validators from OUTSIDE the**** ****`nix-shell`****, i.e. with our local node that is synced. The**** ****`nix-shell`**** ****provides ONLY a development environment for writing and serialising Plutus validators.**_
{% endhint %}

<pre class="language-bash"><code class="lang-bash"><strong># testnet/create-addresses.sh
</strong><strong>
</strong><strong>#!/usr/bin/env bash
</strong>
NWMAGIC=2 # preview testnet

# Build normal address 1
cardano-cli address key-gen \
--verification-key-file ./address/01.vkey \
--signing-key-file ./address/01.skey

cardano-cli address build \
--payment-verification-key-file ./address/01.vkey \
--testnet-magic $NWMAGIC \
--out-file ./address/01.addr

# Build normal address 2
cardano-cli address key-gen \
--verification-key-file ./address/02.vkey \
--signing-key-file ./address/02.skey

cardano-cli address build \
--payment-verification-key-file ./address/02.vkey \
--testnet-magic $NWMAGIC \
--out-file ./address/02.addr

echo "Before continuing, request faucet funds to address: $(cat address/01.addr)!"

</code></pre>

To run the script, we first have to make it an executable:

```
chmod +x create-addressses.sh
./create-addressses.sh
```

{% hint style="info" %}
We will always need to make any `bash` scripts we intend to run executable first with the above command `chmod +x <script-name>.sh`.
{% endhint %}

Once done, we need to request funds to our new address from the faucet: [https://docs.cardano.org/cardano-testnet/tools/faucet/](https://docs.cardano.org/cardano-testnet/tools/faucet/). Make sure you select the right network for the transaction, we are using `preview` in this course.

The two addresses we created will be shared among all the validators we test. Now we need to create a _script address_ for our `SimplestSuccess` validator. For each validator we test, we will place the testing resources under a new directory specific to that validator. For `SimplestSuccess`, that will be `testnet/SimplestSuccess/`. After creating the directory, let's build the script address. Note that here we do not specify a key pair for the address, but instead a script file that acts as the validator for that address.

```bash
# testnet/SimplestSuccess/build-script-address.sh

#!/usr/bin/env bash

NWMAGIC=2 # preview testnet

# Build script address
cardano-cli address build \
--payment-script-file ../../compiled/SimplestSuccess.plutus \
--testnet-magic $NWMAGIC \
--out-file SimplestSuccess.addr
```

We can also build a convenience script to check the UTxOs at our addresses.

```bash
# testnet/SimplestSuccess/check-utxos.sh

#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

funds_normal=$(cardano-cli query utxo \
--address $(cat ../address/01.addr) \
--testnet-magic $NWMAGIC)

funds_script=$(cardano-cli query utxo \
--address $(cat SimplestSuccess.addr) \
--testnet-magic $NWMAGIC)

echo "Normal address:"
echo "${funds_normal}"

echo "Script address:"
echo "${funds_script}"
```

We will see something interesting when we check the UTxOs. Our normal address has just a single UTxO on it, which is the transaction from the faucet, and that is expected. But the script address has a lot of UTxOs on it. You might have expected it to have no UTxOs as we just created it. But it turns out, this is a common script whose address was already created and used on this testnet. Of course, if two or more people write scripts with the same compilation result (the same Plutus Core code that is executed on-chain), then the address created from that script will also be the same. This is because a script address is simply the hash of the script code.

Since we know there are already UTxOs sitting at the script address, we do not really need to send any funds to it in order to test that we can spend them back. We can just use any of the existing UTxOs since the script allows any UTxO sitting on it to be spent. We will still create a script for sending funds to the script for completeness. Note that as we mentioned before, any script UTxO without a datum attached is _**UNSPENDABLE**_ (go ahead and try spending one), so never forget to attach a datum when sending funds to a script. The `--tx-in` argument will be the UTxO from our normal address so you need to change it accordingly. For datum, we will simply embed the `unit.json` that we created earlier. Finally, we need to sign the transaction with the private key of the `01.addr`.

{% hint style="warning" %}
_**Note that for all the bash scripts in this course, you will need to change the --tx-in arguments to match your own UTxOs. If you have not maintained the same directory structure as outlined in the course, you will need to change those paths accordingly as well.**_
{% endhint %}

```bash
# testnet/SimplestSuccess/send-funds-to-script.sh

#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/01.addr) \
    --tx-in 2326577336a90f71738aab4803b3f1ae9107d0ddeb107fc4bf926b24e95930ad#0 \
    --tx-out $(cat SimplestSuccess.addr)+2000000 \
    --tx-out-datum-embed-file ../../compiled/assets/unit.json \
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

Now we want to see if we can spend a UTxO from the script. Good luck finding the one you just sent to it. You can just pick a random one that has a datum from the UTxO list instead. This time, we must include a collateral UTxO, which must be from a regular address as we mentioned before. Change the `--tx-in` and `--tx-in-collateral` accordingly.

```bash
# testnet/SimplestSuccess/spend-script-utxo.sh

#!/usr/bin/env bash

NWMAGIC=2 # preview testnet
export CARDANO_NODE_SOCKET_PATH=$CNODE_HOME/sockets/node0.socket

cardano-cli transaction build \
    --testnet-magic $NWMAGIC \
    --change-address $(cat ../address/01.addr) \
    --tx-in ea340a31a9ad4dd059e6743274607e2cc7bdb7b12b5345be8bc81988d9a6ea86#0 \
    --tx-in-script-file ../../compiled/SimplestSuccess.plutus \
    --tx-in-datum-file ../../compiled/assets/unit.json \
    --tx-in-redeemer-file ../../compiled/assets/unit.json \
    --tx-in-collateral ea340a31a9ad4dd059e6743274607e2cc7bdb7b12b5345be8bc81988d9a6ea86#1 \
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

After the transaction is successfully submitted and processed, we can confirm that our `01.addr` received the funds from the script and our collateral was not spent.

```bash
./check-utxos.sh
```

```bash
Normal address:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
ea340a31a9ad4dd059e6743274607e2cc7bdb7b12b5345be8bc81988d9a6ea86     1        9997830891 lovelace + TxOutDatumNone
ee346be463426509daec07aba24a8905c5f55965daebb39f842a49191d83f9e1     0        1829006 lovelace + TxOutDatumNone
```

### Practice!

It is important to practice on your own. Try using the materials above to write and test a Plutus script that always fails (a sort of token-burning script) from scratch.
