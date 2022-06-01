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
import Plutus.Trace
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
emCfg = EmulatorConfig (Left $ Map.fromList [if w ==1 then (knownWallet w, v1) else (knownWallet w, v2) | w <- [1 .. 2]]) def def
  where
    v1 :: Value
    v1 = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue token 1000
    v2 :: Value
    v2 = Ada.lovelaceValueOf 1_000_000_000 


currency :: CurrencySymbol
currency = "aa"

tname :: TokenName
tname = "Machine"

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
    void $ waitNSlots 2
    callEndpoint @"init" h1 $ InitParams {
        ipName = 2,
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
        dpTName = "Machine",
        dpAmount = 100
    }

    void $ waitNSlots 2
    callEndpoint @"withdraw" h1 $ WithdParams {
        wpName = 1,
        wpPattern = 1,
        wpCurSymbol = "aa",
        wpTName = "Machine",
        wpAmount = 10
    }

    void $ waitNSlots 2
    callEndpoint @"transfer" h1 $ TransParams {
        tpNameSen = 1,
        tpPatternSen = 1,
        tpRecPkh = (mockWalletPaymentPubKeyHash $ knownWallet 2),
        tpNameRec = 1,
        tpPatternRec = 2,
        tpCurSymbol = "aa",
        tpTName = "Machine",
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
        tpTName = "Machine",
        tpAmount = 30
    }


    void $ waitNSlots 2
    callEndpoint @"view" h1 $ ViewParams {
        vpName = 1
    }

    void $ waitNSlots 2
    callEndpoint @"close" h2 $ CloseParams {
        cpName = 1,
        cpPattern = 2
    }

    void $ waitNSlots 2
    callEndpoint @"mint" h1 $ MintParams {
        mpName = 2,
        mpPattern = 1,
        mpToken = "Car",
        mpAmount = 10
    }

    void $ waitNSlots 2
    callEndpoint @"close" h1 $ CloseParams {
        cpName = 1,
        cpPattern = 1
    }
    void $ waitNSlots 2
    callEndpoint @"close" h1 $ CloseParams {
        cpName = 2,
        cpPattern = 1
    }


   
    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s
    