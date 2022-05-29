{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores    #-}
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
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
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
mkAccountValidator p dat () ctx = traceIfFalse "owner's signature missing" signedByOwner
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

--Endpoint Params
--Pattern       Account |   active  |   passive
--Balance sheet ________|___________|_____________
--              credit  |   1       |   3
--              debit   |   2       |   4

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
    vpName :: !Integer,
    vpPattern :: !Integer,
    vpCurSymbol :: !CurrencySymbol,
    vpTName :: !TokenName
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
    tpNameRec :: !Integer,
    tpPatternRec :: !Integer,
    --General
    tpCurSymbol :: !CurrencySymbol,
    tpTName :: !TokenName,
    tpAmount :: !Integer
} deriving(Generic, ToJSON, FromJSON, ToSchema)

type AccountSchema =
            Endpoint "init"     InitParams
        .\/ Endpoint "close"    CloseParams
        .\/ Endpoint "view"     ViewParams
        .\/ Endpoint "deposit"  DepositParams
        .\/ Endpoint "withdraw" WithdParams
        .\/ Endpoint "transfer" TransParams

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
    utxos <- Map.filter (isSuitable (vpName vp) (vpPattern vp)) <$> utxosAt (scrAddress p)
    if Map.null utxos
        then logInfo @String $ "no suitable Account was found at this Adress."
        else do
            let list = Map.toList utxos
                ciList       = snd <$> list
                valueList = _ciTxOutValue <$> ciList
            logInfo @String $ printf "The Account %d holds %d" 
                (vpName vp) (valueOf (mconcat valueList) (vpCurSymbol vp) (vpTName vp) )

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
                orefs    = fst <$> list
                ciList       = snd <$> list
                valueList = _ciTxOutValue <$> ciList
                lookups  = Constraints.unspentOutputs utxos <>
                           Constraints.otherScript (validator p)
            case getDatum' (head ciList) of
                Nothing -> logInfo @String $ "no Datum in Source Script"
                Just d -> do
                    let tx = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs]
                             <> Constraints.mustPayToOtherScript (valHash p) d (
                                    (mconcat valueList) <> (Value.singleton (wpCurSymbol wp) (wpTName wp) (-(wpAmount wp))))
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
    let p = AccountParam { owner = pkh }
    utxosSen <- Map.filter (isSuitable (tpNameSen tp) (tpPatternSen tp)) <$> utxosAt (scrAddress p)
    utxosRec <- Map.filter (isSuitable (tpNameRec tp) (tpPatternRec tp)) <$> utxosAt (scrAddress p)
    if Map.null utxosSen && Map.null utxosRec
        then logInfo @String $ "No suitable Accounts were found."
        else do 
            let listSen     = Map.toList utxosSen
                orefSen     = fst <$> listSen
                ciListSen   = snd <$> listSen
                valueListSen = _ciTxOutValue <$> ciListSen
                listRec     = Map.toList utxosRec
                orefRec     = fst <$> listRec
                ciListRec   = snd <$> listRec
                valueListRec = _ciTxOutValue <$> ciListRec
                lookups  =  Constraints.unspentOutputs utxosSen <>
                            Constraints.unspentOutputs utxosRec <>
                            Constraints.otherScript (validator p)
            case getDatum' (head ciListSen) of
                Nothing -> logInfo @String $ "no Datum in Source Script"
                Just dSen -> do
                    case getDatum' (head ciListRec) of
                        Nothing -> logInfo @String $ "no Datum in Source Script"
                        Just dRec -> do
                            let tValSen = (mconcat valueListSen) <> (Value.singleton (tpCurSymbol tp) (tpTName tp) (-(tpAmount tp)))
                                          
                                tValRec = (mconcat valueListRec) <> (Value.singleton (tpCurSymbol tp) (tpTName tp) (tpAmount tp)) 
                                          
                                tx = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefSen] <> 
                                      mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefRec] <> 
                                      (Constraints.mustPayToOtherScript (valHash p) dSen tValSen) <>
                                      (Constraints.mustPayToOtherScript (valHash p) dRec tValRec)
                            if valueOf (mconcat valueListSen) (tpCurSymbol tp) (tpTName tp) - (tpAmount tp) < 0
                                then logInfo @String $ printf "Insuffitient Funding in Account %d of type %d" (tpNameSen tp) (tpPatternSen tp)
                                else do
                                    ledgerTx <- submitTxConstraintsWith @Account lookups tx
                                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                    logInfo @String $ logger tp
        where
            logger :: TransParams -> String
            logger (TransParams nameSen patSen nameRec patRec curSym tName amount)
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

--helper Functions
isSuitable :: Integer -> Integer -> ChainIndexTxOut -> Bool
isSuitable n pat o = case _ciTxOutDatum o of
    Left _ -> False
    Right (Datum e) -> case PlutusTx.fromBuiltinData e of 
        Nothing -> False
        Just d -> name d == n && pattern d == pat

lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

getDatum' :: ChainIndexTxOut -> Maybe Datum
getDatum' o = case _ciTxOutDatum o of
            Left  i               -> Nothing 
            Right j               -> Just j

--Definitions
endpoints :: Contract () AccountSchema Text ()
endpoints = awaitPromise (init' `select` close' `select` view' `select` deposit' `select` withdraw' `select` transfer' ) >> endpoints
    where
        init'     = endpoint @"init" init
        close'    = endpoint @"close" close
        view'     = endpoint @"view" view
        deposit'  = endpoint @"deposit" deposit
        withdraw' = endpoint @"withdraw" withdraw
        transfer' = endpoint @"transfer" transfer


mkSchemaDefinitions ''AccountSchema
mkKnownCurrencies []