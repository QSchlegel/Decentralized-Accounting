{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import qualified PlutusTx.Prelude                             as Plutus
import Plutus.Trace
import           Plutus.Trace.Emulator                        as Emulator
import qualified Data.Map                                     as Map
import           Data.Default                                 (Default (..))
import Wallet.Emulator.Wallet
import Ledger
import Ledger.Value
import Ledger.Ada as Ada

import Account

test :: IO ()
test = runEmulatorTraceIO' def emCfg accountTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet w, v) | w <- [1 .. 3]]) def def
  where
    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue token 1000

currency :: CurrencySymbol
currency = "aa"

tname :: TokenName
tname = "A"

token :: AssetClass
token = AssetClass (currency, tname)

accountTrace :: EmulatorTrace ()
accountTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    --Bank active credit
    callEndpoint @"init" h1 $ InitParams {
        ipName = 1,
        ipPattern = 1,
        ipAmount = 20000000
    }
    --liability passive debit
    void $ waitNSlots 2
    callEndpoint @"init" h2 $ InitParams {
        ipName = 1,
        ipPattern = 2,
        ipAmount = 15000000
    }
    

    void $ waitNSlots 2
    callEndpoint @"deposit" h1 $ DepositParams {
        dpName = 1,
        dpPattern = 1,
        dpCurSymbol = "aa",
        dpTName = "A",
        dpAmount = 100
    }

    void $ waitNSlots 2
    callEndpoint @"withdraw" h1 $ WithdParams {
        wpName = 1,
        wpPattern = 1,
        wpCurSymbol = "aa",
        wpTName = "A",
        wpAmount = 30
    }

    void $ waitNSlots 2
    callEndpoint @"transfer" h1 $ TransParams {
        tpNameSen = 1,
        tpPatternSen = 1,
        tpRecPkh = (mockWalletPaymentPubKeyHash $ knownWallet 2),
        tpNameRec = 1,
        tpPatternRec = 2,
        tpCurSymbol = "aa",
        tpTName = "A",
        tpAmount = 40
    }
    void $ waitNSlots 2
    callEndpoint @"transfer" h2 $ TransParams {
        tpNameSen = 1,
        tpPatternSen = 2,
        tpRecPkh = (mockWalletPaymentPubKeyHash $ knownWallet 1),
        tpNameRec = 1,
        tpPatternRec = 1,
        tpCurSymbol = "aa",
        tpTName = "A",
        tpAmount = 30
    }


    void $ waitNSlots 2
    callEndpoint @"view" h1 $ ViewParams {
        vpName = 1
    }
   
    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s
    