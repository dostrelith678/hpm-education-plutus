# Emulating the blockchain

So far, we have been using the `cardano-node` to submit transactions to the testnet in order to interact with our validators. While this is a valid way of investigating and testing behaviour, it can become tedious. Fortunately, we have another way of testing validators through _simulation_ using the `Plutus.Trace.Emulator` module ([https://playground.plutus.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Trace-Emulator.html](https://playground.plutus.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Trace-Emulator.html)). This module can be used to create a temporary emulated blockchain for testing validators without the need for an actual live Cardano network (such as preview, preprod or mainnet).

The two main components for emulation are the `Contract` monad and the `EmulatorTrace` monad. The `Contract` monad represents the off-chain code, i.e. the code that builds and submits transactions for `cardano-node` to validate. The `EmulatorTrace` monad is a contract trace that can be run in the Plutus emulator, and prints information about the emulated blockchain, its transactions and wallet/script balances.

We will first take a closer look at `EmulatorTrace`. We can run it without actually testing any validators just to see it create an emulated blockchain. We can open a `cabal repl` from the `nix-shell` and import the `Plutus.Trace.Emulator` module:

```
ghci> import Plutus.Trace.Emulator
```

The function we generally want to use for emulation is `runEmulatorTraceIO` which gives us the most meaningful information printed to `stdout`. We can check its signature on Haddock ([https://playground.plutus.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Trace-Emulator.html#v:runEmulatorTraceIO](https://playground.plutus.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Trace-Emulator.html#v:runEmulatorTraceIO)):

`runEmulatorTraceIO :: EmulatorTrace () -> IO ()`.

It accepts an `EmulatorTrace` and returns an `IO ()`. So what is an `EmulatorTrace`? Well, it has a somewhat complex definition that we do not really need to understand in detail:

```haskell
type EmulatorTrace =
        Eff
            '[ StartContract
            , RunContract
            , Assert
            , Waiting
            , EmulatorControl
            , EmulatedWalletAPI
            , LogMsg String
            , Error EmulatorRuntimeError
            ]
```

It is enough to know that it is a monad containing everything required to emulate the blockchain.

Back to the `runEmulatorTraceIO` function, let's run it with the simple example from the docs `runEmulatorTraceIO (void $ waitNSlots 1)`. We will need to import the `void` function from `Data.Functor`:

```
ghci> import Data.Functor (void)
ghci> runEmulatorTraceIO (void $ waitNSlots 1)
Slot 00000: TxnValidate 43ba666cc8a22a04b63a3b605ce14146dfa5ed999986625ad90c1bc16dabdd84 []
Slot 00000: SlotAdd Slot 1
Slot 00001: W[7]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[8]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[6]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[4]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[2]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[1]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[10]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[9]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[3]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: W[5]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.
Slot 00001: SlotAdd Slot 2
Slot 00002: W[7]: InsertionSuccess: New tip is Tip(Slot 2, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[8]: InsertionSuccess: New tip is Tip(Slot 2, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[6]: InsertionSuccess: New tip is Tip(Slot 2, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[4]: InsertionSuccess: New tip is Tip(Slot 2, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[2]: InsertionSuccess: New tip is Tip(Slot 2, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[1]: InsertionSuccess: New tip is Tip(Slot 2, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[10]: InsertionSuccess: New tip is Tip(Slot 2, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[9]: InsertionSuccess: New tip is Tip(Slot 2, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[3]: InsertionSuccess: New tip is Tip(Slot 2, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 1). UTxO state was added to the end.
Slot 00002: W[5]: InsertionSuccess: New tip is Tip(Slot 2, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 1). UTxO state was added to the end.
Slot 00002: SlotAdd Slot 3
Slot 00003: W[7]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[8]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[6]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[4]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[2]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[1]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[10]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[9]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[3]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 2). UTxO state was added to the end.
Slot 00003: W[5]: InsertionSuccess: New tip is Tip(Slot 3, BlockId 76be8b528d0075f7aae98d6fa57a6d3c83ae480a8469e668d7b0af968995ac71, BlockNumber 2). UTxO state was added to the end.
Final balances
Wallet 7: 
    {, ""}: 100000000
Wallet 8: 
    {, ""}: 100000000
Wallet 6: 
    {, ""}: 100000000
Wallet 4: 
    {, ""}: 100000000
Wallet 2: 
    {, ""}: 100000000
Wallet 1: 
    {, ""}: 100000000
Wallet 10: 
    {, ""}: 100000000
Wallet 9: 
    {, ""}: 100000000
Wallet 3: 
    {, ""}: 100000000
Wallet 5: 
    {, ""}: 100000000
```

Okay, we got an emulated blockchain! The only transaction we can see is the initial one`Slot 00000: TxnValidate 43ba666cc8a22a04b63a3b605ce14146dfa5ed999986625ad90c1bc16dabdd84 []`. This transaction created the initial wallet distribution. By default, this is 10 wallets with 100 ADA each as we can see in the logs. The balances at the end of the simulation are unchanged since we did not do any transactions after the initial one.

We can also see that the log messages regarding new blocks are duplicated for each of the wallets: `Slot 00001: W[1]: InsertionSuccess: New tip is Tip(Slot 1, BlockId 9e944371f5292bcd66e4e498bbc313b92ae884154f0eca1ddf75cd0ec69ddc47, BlockNumber 0). UTxO state was added to the end.`.

An advanced question would be how to configure the emulation differently, for example, with a different initial ADA distribution or with a trace format. The defaults will do just fine for examples in this course, but for those interested, there is another function `runEmulatorTraceIO'` which accepts additional configurations that can be customised:`runEmulatorTraceIO' :: TraceConfig -> EmulatorConfig -> EmulatorTrace () -> IO ()`

Here are the definitions of `TraceConfig` and `EmulatorConfig` along with their default instances:

```haskell
-- | Options for how to set up and print the trace.
data TraceConfig = TraceConfig
  { showEvent    :: EmulatorEvent' -> Maybe String
  -- ^ Function to decide how to print the particular events.
  , outputHandle :: Handle
  -- ^ Where to print the outputs to. Default: 'System.IO.stdout'
  }

instance Default TraceConfig where
  def = TraceConfig
            { showEvent     = defaultShowEvent
            , outputHandle  = stdout
            }

defaultShowEvent :: EmulatorEvent' -> Maybe String
defaultShowEvent = \case
  UserThreadEvent (UserLog msg)                                        -> Just $ "*** USER LOG: " <> msg
  InstanceEvent (ContractInstanceLog (ContractLog (A.String msg)) _ _) -> Just $ "*** CONTRACT LOG: " <> show msg
  InstanceEvent (ContractInstanceLog (StoppedWithError err)       _ _) -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> show err
  InstanceEvent (ContractInstanceLog NoRequestsHandled            _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (HandledRequest _)           _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (CurrentRequests _)          _ _) -> Nothing
  SchedulerEvent _                                                     -> Nothing
  WalletEvent _ _                                                      -> Nothing
  ev                                                                   -> Just . renderString . layoutPretty defaultLayoutOptions . pretty $ ev
```

```haskell
data EmulatorConfig =
    EmulatorConfig
        { _initialChainState :: InitialChainState -- ^ State of the blockchain at the beginning of the simulation. Can be given as a map of funds to wallets, or as a block of transactions.
        , _slotConfig        :: SlotConfig -- ^ Set the start time of slot 0 and the length of one slot
        } deriving (Eq, Show)

type InitialChainState = Either InitialDistribution [Tx]

instance Default EmulatorConfig where
  def = EmulatorConfig
          { _initialChainState = Left defaultDist
          , _slotConfig = def
          }

defaultDist :: InitialDistribution
defaultDist = defaultDistFor EM.knownWallets

data SlotConfig =
    SlotConfig
        { scSlotLength   :: Integer -- ^ Length (number of milliseconds) of one slot
        , scSlotZeroTime :: POSIXTime -- ^ Beginning of slot 0 (in milliseconds)
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise, NFData, OpenApi.ToSchema)

instance Default SlotConfig where
    {-# INLINABLE def #-}
    def = SlotConfig{ scSlotLength = 1000, scSlotZeroTime = POSIXTime beginningOfTime }
```
