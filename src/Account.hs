{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Account where


import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (mint, singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value         as Value
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)
import           Wallet.Emulator.Wallet



data AccountParam = AccountParam 
    {   owner :: PaymentPubKeyHash
    } deriving Show
PlutusTx.makeLift ''AccountParam

data AccountDatum = AccountDatum
    {   name :: Integer,
        --Accountpatterns: {(0: balancesheet), (1:active Account), (2:pasive Account), ... PNL, etc...}
        pattern :: Integer
    } deriving Show
PlutusTx.unstableMakeIsData ''AccountDatum

--Validator
{-# INLINABLE mkAccountValidator #-}
mkAccountValidator :: AccountParam -> AccountDatum -> () -> ScriptContext -> Bool
mkAccountValidator p dat () ctx = traceIfFalse "owner's signature missing" (signedByOwner || True)
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        signedByOwner :: Bool
        signedByOwner = txSignedBy info $ unPaymentPubKeyHash $ owner p

data Account
instance Scripts.ValidatorTypes Account where
    type instance DatumType Account = AccountDatum
    type instance RedeemerType Account = ()

typedValidator :: AccountParam -> Scripts.TypedValidator Account
typedValidator p = Scripts.mkTypedValidator @Account
    ($$(PlutusTx.compile [|| mkAccountValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @AccountDatum @()

validator :: AccountParam -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: AccountParam -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

scrAddress :: AccountParam -> Ledger.Address
scrAddress = scriptAddress . validator

{-# INLINABLE mkPolicy #-}
mkPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) $ unPaymentPubKeyHash pkh

policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh


--Endpoint Params
--Pattern       Account |   active  |   passive |   profit  |   loss    |
--Balance sheet ________|___________|___________|___________|___________|___
--              credit  |   1       |   3       |   5       |   7       |
--              debit   |   2       |   4       |   6       |   8       |


data InitParams = InitParams {
    ipName :: !Integer,
    ipPattern :: !Integer,
    ipAmount :: !Integer
} deriving(Generic, ToJSON, FromJSON, ToSchema)

data CloseParams = CloseParams {
    cpName :: !Integer,
    cpPattern :: !Integer
} deriving(Generic, ToJSON, FromJSON, ToSchema)

data ViewParams = ViewParams {
    vpName :: !Integer
} deriving(Generic, ToJSON, FromJSON, ToSchema)

data DepositParams = DepositParams {
    dpName :: !Integer,
    dpPattern :: !Integer,
    dpCurSymbol :: !CurrencySymbol,
    dpTName :: !TokenName,
    dpAmount :: !Integer
} deriving(Generic, ToJSON, FromJSON, ToSchema)

data WithdParams = WithdParams {
    wpName :: !Integer,
    wpPattern :: !Integer,
    wpCurSymbol :: !CurrencySymbol,
    wpTName :: !TokenName,
    wpAmount :: !Integer
} deriving(Generic, ToJSON, FromJSON, ToSchema)

data TransParams = TransParams {
    --Sender
    tpNameSen :: !Integer,
    tpPatternSen :: !Integer,
    --Reciever
    tpRecPkh :: !PaymentPubKeyHash,
    tpNameRec :: !Integer,
    tpPatternRec :: !Integer,
    --General
    tpCurSymbol :: !CurrencySymbol,
    tpTName :: !TokenName,
    tpAmount :: !Integer
} deriving(Generic, ToJSON, FromJSON, ToSchema)

data MintParams = MintParams {
    mpName :: !Integer,
    mpPattern :: !Integer,
    mpToken     :: !TokenName,
    mpAmount    :: !Integer
} deriving(Generic, ToJSON, FromJSON, ToSchema)

type AccountSchema =
            Endpoint "init"     InitParams
        .\/ Endpoint "close"    CloseParams
        .\/ Endpoint "view"     ViewParams
        .\/ Endpoint "deposit"  DepositParams
        .\/ Endpoint "withdraw" WithdParams
        .\/ Endpoint "transfer" TransParams
        .\/ Endpoint "mint"     MintParams

        

--Contract Endpoints
init :: AsContractError e => InitParams -> Contract w s e ()
init ip = do
    pkh <- ownPaymentPubKeyHash
    let p = AccountParam { owner = pkh }
        dat = AccountDatum {
            name = ipName ip,
            pattern = ipPattern ip  }
        tx = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ ipAmount ip
    ledgerTx <- submitTxConstraints (typedValidator p) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "created an Account called %d of type %d"  (ipName ip) (ipPattern ip)

view:: AsContractError e => ViewParams -> Contract w s e ()
view vp = do 
    pkh <- ownPaymentPubKeyHash
    let p = AccountParam { owner = pkh }
    utxos <- Map.filter (isSuitable' (vpName vp)) <$> utxosAt (scrAddress p)
    if Map.null utxos
        then logInfo @String $ "no suitable Account was found at this Adress."
        else do
            let list        = Map.toList utxos
                ciList      = snd <$> list
                valueList   = _ciTxOutValue <$> ciList
                listVal     = flattenValue $ mconcat valueList
            logInfo @String $ printf "The Account %d holds %s" 
                (vpName vp) (show [ (show am) ++ " of " ++ (show tn) ++ "   " | (cs,tn,am) <-listVal])

deposit :: AsContractError e => DepositParams -> Contract w s e ()
deposit dp = do 
    pkh <- ownPaymentPubKeyHash
    let p = AccountParam { owner = pkh}
    utxos <- Map.filter (isSuitable (dpName dp) (dpPattern dp)) <$> utxosAt (scrAddress p)
    if Map.null utxos
        then logInfo @String $ "no suitable Account was found at this Adress."
        else do
            let 
                list = Map.toList utxos
                orefs = fst <$> list
                ciList = snd <$> list
                valueList = _ciTxOutValue <$> ciList
                lookups  = Constraints.unspentOutputs utxos <>
                           Constraints.otherScript (validator p)
            case getDatum' (head ciList) of
                Nothing -> logInfo @String $ "no Datum in Source Script"
                Just d -> do
                    let tval = (Value.singleton (dpCurSymbol dp) (dpTName dp) (dpAmount dp)) <> 
                               (mconcat valueList)
                        tx = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <> 
                                     (Constraints.mustPayToOtherScript (valHash p) d $ tval)
                    ledgerTx <- submitTxConstraintsWith @Account lookups tx
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                    logInfo @String $ printf "%d was deposited on Account %d." (dpAmount dp) (dpName dp)

withdraw :: AsContractError e => WithdParams -> Contract w s e ()
withdraw wp = do
    pkh <- ownPaymentPubKeyHash
    let p = AccountParam { owner = pkh }
    utxos <- Map.filter (isSuitable (wpName wp) (wpPattern wp)) <$> utxosAt (scrAddress p)
    if Map.null utxos
        then logInfo @String $ "No suitable Account was found."
        else do 
            let list = Map.toList utxos
                orefs       = fst <$> list
                ciList      = snd <$> list
                valueList   = _ciTxOutValue <$> ciList
                lookups     = Constraints.mintingPolicy (policy pkh) <>
                              Constraints.unspentOutputs utxos <>
                              Constraints.otherScript (validator p) 
            case getDatum' (head ciList) of
                Nothing -> logInfo @String $ "no Datum in Source Script"
                Just d -> do
                    let val1 = Value.singleton (wpCurSymbol wp) (wpTName wp) (-(wpAmount wp))
                        val2 = Value.singleton (curSymbol' pkh) "Balance Token" ((wpAmount wp))
                        tx = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <> 
                                      Constraints.mustPayToOtherScript (valHash p) d (
                                        (mconcat valueList) <> val1 <> val2 ) <>
                                      Constraints.mustMintValue val2
                    if valueOf (mconcat valueList) (wpCurSymbol wp) (wpTName wp) - (wpAmount wp) < 0
                        then logInfo @String $ printf "Insuffitient Funding in Account %d of type %d" (wpName wp) (wpPattern wp)
                        else do
                            ledgerTx <- submitTxConstraintsWith @Account lookups tx
                            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                            logInfo @String $ printf "%d was withdrawn form Account %d of type %d."
                                (wpAmount wp)   (wpName wp) (wpPattern wp)                       

transfer :: AsContractError e => TransParams -> Contract w s e ()
transfer tp = do
    pkh <- ownPaymentPubKeyHash
    let pSen = AccountParam { owner = pkh }
        pRec = AccountParam { owner = (tpRecPkh tp) }
    utxosSen <- Map.filter (isSuitable (tpNameSen tp) (tpPatternSen tp)) <$> utxosAt (scrAddress pSen)
    utxosRec <- Map.filter (isSuitable (tpNameRec tp) (tpPatternRec tp)) <$> utxosAt (scrAddress pRec)
    if Map.null utxosSen || Map.null utxosRec
        then logInfo @String $ "No suitable Accounts were found."
        else do 
            let listSen      = Map.toList utxosSen
                orefSen      = fst <$> listSen
                ciListSen    = snd <$> listSen
                valueListSen = _ciTxOutValue <$> ciListSen
                listRec      = Map.toList utxosRec
                orefRec      = fst <$> listRec
                ciListRec    = snd <$> listRec
                valueListRec = _ciTxOutValue <$> ciListRec
                lookups      =  Constraints.mintingPolicy (policy pkh) <>
                                Constraints.unspentOutputs utxosSen <>
                                Constraints.unspentOutputs utxosRec <>
                                Constraints.otherScript (validator pSen) <>
                                Constraints.otherScript (validator pRec)
            case getDatum' (head ciListSen) of
                Nothing -> logInfo @String $ "no Datum in Source Script"
                Just dSen -> do
                    case getDatum' (head ciListRec) of
                        Nothing -> logInfo @String $ "no Datum in Source Script"
                        Just dRec -> do
                            let tValSen = (mconcat valueListSen) <> (Value.singleton (tpCurSymbol tp) (tpTName tp) (-(tpAmount tp)))
                                tValRec = (mconcat valueListRec) <> (Value.singleton (tpCurSymbol tp) (tpTName tp) (tpAmount tp))  
                                mVal    = Value.singleton (curSymbol' pkh) "Balance Token" ((tpAmount tp))

                                tx =  mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefSen] <> 
                                      mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefRec] <> 
                                              (Constraints.mustPayToOtherScript (valHash pSen) dSen (tValSen <> mVal)) <>
                                              (Constraints.mustPayToOtherScript (valHash pRec) dRec tValRec) <>
                                               Constraints.mustMintValue mVal
                            if valueOf (mconcat valueListSen) (tpCurSymbol tp) (tpTName tp) - (tpAmount tp) < 0
                                then logInfo @String $ printf "Insuffitient Funding in Account %d of type %d" (tpNameSen tp) (tpPatternSen tp)
                                else do
                                    ledgerTx <- submitTxConstraintsWith @Account lookups tx
                                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                    logInfo @String $ logger tp
        where
            logger :: TransParams -> String
            logger (TransParams nameSen patSen _ nameRec patRec curSym tName amount)
                | patSen `modulo` 2 == 0 && patRec `modulo` 2 == 1 = printf "%d  was transfered from active Account %d to passive Account %d" amount nameRec nameSen
                | patSen `modulo` 2 == 1 && patRec `modulo` 2 == 0 = printf "%d  was transfered from active Account %d to passive Account %d" amount nameSen nameRec
                | otherwise = "Accounting Error"

close :: AsContractError e => CloseParams -> Contract w s e ()
close cp = do
    pkh <- ownPaymentPubKeyHash
    let p = AccountParam { owner = pkh }
    utxos <- Map.filter (isSuitable (cpName cp) (cpPattern cp)) <$> utxosAt (scrAddress p)
    if Map.null utxos
        then logInfo @String $ "no suitable Account was found at this Adress."
        else do
            let orefs = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos <>
                          Constraints.otherScript (validator p)
                tx :: TxConstraints Void Void
                tx = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs]

            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId $ ledgerTx
            logInfo @String $ "Account closed"

mint :: AsContractError e => MintParams -> Contract w s e ()
mint mp = do
    pkh <- ownPaymentPubKeyHash
    let p = AccountParam { owner = pkh }
    utxos <- Map.filter (isSuitable (mpName mp) (mpPattern mp)) <$> utxosAt (scrAddress p)
    if Map.null utxos
        then logInfo @String $ "No suitable Account was found."
        else do 
            let list = Map.toList utxos
                orefs    = fst <$> list
                ciList       = snd <$> list
                valueList = _ciTxOutValue <$> ciList
                lookups  = Constraints.unspentOutputs utxos <>
                           Constraints.otherScript (validator p)
            case getDatum' (head ciList) of
                Nothing -> logInfo @String $ "no Datum in Source Script"
                Just d -> do
                    let val     = Value.singleton (curSymbol' pkh)  (mpToken mp) (mpAmount mp)
                        lookups = Constraints.mintingPolicy $ policy pkh
                        tx      = Constraints.mustMintValue val <>
                                  Constraints.mustPayToOtherScript (valHash p) d ( val <> (mconcat valueList))
                    ledgerTx <- submitTxConstraintsWith @Void lookups tx
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                    logInfo @String $ printf "forged %s" (show val)

--helper Functions
isSuitable :: Integer -> Integer -> ChainIndexTxOut -> Bool
isSuitable n pat o = case _ciTxOutDatum o of
    Left _ -> False
    Right (Datum e) -> case PlutusTx.fromBuiltinData e of 
        Nothing -> False
        Just d -> name d == n && pattern d == pat

isSuitable' :: Integer -> ChainIndexTxOut -> Bool
isSuitable' n o = case _ciTxOutDatum o of
    Left _ -> False
    Right (Datum e) -> case PlutusTx.fromBuiltinData e of 
        Nothing -> False
        Just d -> name d == n

lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

getDatum' :: ChainIndexTxOut -> Maybe Datum
getDatum' o = case _ciTxOutDatum o of
            Left  i               -> Nothing 
            Right j               -> Just j

curSymbol' :: PaymentPubKeyHash -> CurrencySymbol
curSymbol' = scriptCurrencySymbol . policy
--Definitions
endpoints :: Contract () AccountSchema Text ()
endpoints = awaitPromise (init' `select` close' `select` view' `select` deposit' `select` withdraw' `select` transfer' `select` mint' ) >> endpoints
    where
        init'       = endpoint @"init" init
        close'      = endpoint @"close" close
        view'       = endpoint @"view" view
        deposit'    = endpoint @"deposit" deposit
        withdraw'   = endpoint @"withdraw" withdraw
        transfer'   = endpoint @"transfer" transfer
        mint'       = endpoint @"mint" mint


mkSchemaDefinitions ''AccountSchema
mkKnownCurrencies []