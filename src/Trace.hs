{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Plutus.Trace
import Wallet.Emulator.Wallet

import Account



test :: IO ()
test = runEmulatorTraceIO accountTrace

accountTrace :: EmulatorTrace ()
accountTrace = do
    h1 <- activateContractWallet (knownWallet 1)  endpoints
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

    -- void $ waitNSlots 2
    -- callEndpoint @"view" h1 $ ViewParams {
    --     vpName = 1,
    --     vpPattern = 1
    -- }

    -- void $ waitNSlots 2
    -- callEndpoint @"fund" h1 $ FundParams {
    --     fpName = 1,
    --     fpPattern = 1,
    --     fpCurSymbol = "",
    --     fpTName = "",
    --     fpAmount = 10000000
    -- }

    -- void $ waitNSlots 2
    -- callEndpoint @"withdraw" h1 $ WithdParams {
    --     wpName = 1,
    --     wpPattern = 1,
    --     wpAmount = 10000000
    -- }

    -- void $ waitNSlots 2
    -- callEndpoint @"transfer" h1 $ TransParams {
    --     tpNameSen = 2,
    --     tpPatternSen = 1,
    --     tpNameRec = 1,
    --     tpPatternRec = 1,
    --     tpAmount = 20000000
    -- }

    void $ waitNSlots 2
    callEndpoint @"close" h1 $ CloseParams {
        cpName = 1,
        cpPattern = 1
    }
    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s
    