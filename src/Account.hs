{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   #-}
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
    vpPattern :: !Integer
} deriving(Generic, ToJSON, FromJSON, ToSchema)

data FundParams = FundParams {
    fpName :: !Integer,
    fpPattern :: !Integer,
    fpAmount :: !Integer
} deriving(Generic, ToJSON, FromJSON, ToSchema)

data WithdParams = WithdParams {
    wpName :: !Integer,
    wpPattern :: !Integer,
    wpAmount :: !Integer
} deriving(Generic, ToJSON, FromJSON, ToSchema)

data TransParams = TransParams {
    --Sender
    tpNameSen :: !Integer,
    tpPatternSen :: !Integer,
    --Reciever
    tpNameRec :: !Integer,
    tpPatternRec :: !Integer,
    tpAmount :: !Integer
} deriving(Generic, ToJSON, FromJSON, ToSchema)

type AccountSchema =
            Endpoint "init"     InitParams
        .\/ Endpoint "close"    CloseParams
        .\/ Endpoint "view"     ViewParams
        .\/ Endpoint "fund"     FundParams
        .\/ Endpoint "withdraw" WithdParams
        .\/ Endpoint "transfer" TransParams

--Contract Endpoints

--ToDo: Multi Account, Multi Asset
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

--ToDo: Multi Account, Multi Asset
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
            

view:: AsContractError e => ViewParams -> Contract w s e ()
view vp = do 
    pkh <- ownPaymentPubKeyHash
    let p = AccountParam { owner = pkh }
    utxos <- Map.filter (isSuitable (vpName vp) (vpPattern vp)) <$> utxosAt (scrAddress p)
    if Map.null utxos
        then logInfo @String $ "no suitable Account was found at this Adress."
        else do
            logInfo @String $ printf "The Account %d holds %d" (vpName vp) (sum (lovelaces <$> (_ciTxOutValue <$> (snd <$> Map.toList utxos))))

--ToDo: Multi Account, Multi Asset
fund :: AsContractError e => FundParams -> Contract w s e ()
fund fp = do
    pkh <- ownPaymentPubKeyHash
    let p = AccountParam { owner = pkh }
    utxos <- Map.filter (isSuitable (fpName fp) (fpPattern fp)) <$> utxosAt (scrAddress p)
    if Map.null utxos
        then logInfo @String $ "no suitable Account was found at this Adress."
        else do 
            let orefs    = fst <$> Map.toList utxos
                os       = snd <$> Map.toList utxos
                residing = sum (lovelaces <$> (_ciTxOutValue <$> os))
                lookups  = Constraints.unspentOutputs utxos <>
                           Constraints.otherScript (validator p)
            case txOutDatumHash $ toTxOut $ head $ os of
                Nothing -> logInfo @String $ "no DatumHash in Source Script"
                Just dh -> do
                    d <- datumFromHash dh
                    case d of 
                        Nothing -> logInfo @String $ "no Datum in Source Script"
                        Just d -> do
                            let tx = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <> 
                                    (Constraints.mustPayToOtherScript (valHash p) d $ Ada.lovelaceValueOf $ ((fpAmount fp) + residing))
                            ledgerTx <- submitTxConstraintsWith @Account lookups tx
                            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                            logInfo @String $ printf "Account %d of type %d recieved funding of %s."    
                                (fpName fp) (fpPattern fp)  (show residing)
        
--ToDo: Multi Account, Multi Asset
withdraw :: AsContractError e => WithdParams -> Contract w s e ()
withdraw wp = do
    pkh <- ownPaymentPubKeyHash
    let p = AccountParam { owner = pkh }
    utxos <- Map.filter (isSuitable (wpName wp) (wpPattern wp)) <$> utxosAt (scrAddress p)
    if Map.null utxos
        then logInfo @String $ "No suitable Account was found."
        else do 
            let orefs    = fst <$> Map.toList utxos
                os       = snd <$> Map.toList utxos
                residing = sum (lovelaces <$> (_ciTxOutValue <$> os))
                lookups  = Constraints.unspentOutputs utxos <>
                           Constraints.otherScript (validator p)
            case txOutDatumHash $ toTxOut $ head $ os of
                Nothing -> logInfo @String $ "No DatumHash in Source Script"
                Just dh -> do
                    d <- datumFromHash dh
                    case d of 
                        Nothing -> logInfo @String $ "No Datum in Source Script"
                        Just d -> do
                            let ptx = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs]
                            case residing' (residing - (wpAmount wp)) ptx p d of 
                                Nothing -> logInfo @String $ printf "Insuffitient Funding in Account %d of type %d" (wpName wp) (wpPattern wp)
                                Just tx -> do
                                    ledgerTx <- submitTxConstraintsWith @Account lookups tx
                                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                    logInfo @String $ printf "%d was withdrawn form Account %d of type %d."
                                        (wpAmount wp)   (wpName wp) (wpPattern wp)

--Accountpatterns: {(0: balancesheet), (1:active Account), (2:pasive Account), ... PNL, etc...}
--ToDo: Multi Account, Multi Asset
transfer :: AsContractError e => TransParams -> Contract w s e ()
transfer tp = do
    pkh <- ownPaymentPubKeyHash
    let p = AccountParam { owner = pkh }
    utxosSen <- Map.filter (isSuitable (tpNameSen tp) (tpPatternSen tp)) <$> utxosAt (scrAddress p)
    utxosRec <- Map.filter (isSuitable (tpNameRec tp) (tpPatternRec tp)) <$> utxosAt (scrAddress p)
    if Map.null utxosSen && Map.null utxosRec
        then logInfo @String $ "No suitable Accounts were found."
        else do 
            let orefSen     = fst <$> Map.toList utxosSen
                osSen       = snd <$> Map.toList utxosSen
                residingSen = sum (lovelaces <$> (_ciTxOutValue <$> osSen))
                orefRec     = fst <$> Map.toList utxosRec
                osRec       = snd <$> Map.toList utxosRec
                residingRec = sum (lovelaces <$> (_ciTxOutValue <$> osRec))                
                lookups  =  Constraints.unspentOutputs utxosSen <>
                            Constraints.unspentOutputs utxosRec <>
                            Constraints.otherScript (validator p)
            case txOutDatumHash $ toTxOut $ head $ osSen of
                Nothing -> logInfo @String $ "No DatumHash in Sender Script"
                Just dhSen -> do
                    dSen <- datumFromHash dhSen
                    case dSen of 
                        Nothing -> logInfo @String $ "No Datum in Sender Script"
                        Just dSen -> do
                            case txOutDatumHash $ toTxOut $ head $ osRec of
                                Nothing -> logInfo @String $ "No DatumHash in Reciever Script"
                                Just dhRec -> do
                                    dRec <- datumFromHash dhRec
                                    case dRec of 
                                        Nothing -> logInfo @String $ "No Datum in Reciever Script"
                                        Just dRec -> do
                                            let ptx = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefSen] <> 
                                                      mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefRec] <> 
                                                              (Constraints.mustPayToOtherScript (valHash p) dRec $ Ada.lovelaceValueOf $ (residingRec + (tpAmount tp)))
                                            case residing' (residingSen - (tpAmount tp)) ptx p dSen of 
                                                Nothing -> logInfo @String $ printf "Insuffitient Funding in Account %d of type %d" (tpNameSen tp) (tpPatternSen tp)
                                                Just tx -> do
                                                    ledgerTx <- submitTxConstraintsWith @Account lookups tx
                                                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx 
                                                    logInfo @String $ (transferLog tp)
                                                    logInfo @String $ printf "%d was tranfered form Account %d of type %d to -> Account %d of type %d."
                                                        (tpAmount tp)
                                                        (tpNameSen tp)
                                                        (tpPatternSen tp)
                                                        (tpNameRec tp)
                                                        (tpPatternRec tp)
    where
        transferLog :: TransParams -> String
        transferLog (TransParams sName sPattern rName rPattern amount )
            | sPattern == 1 && rPattern == 1 = printf "Account %d to Account %d " rName sName
            | sPattern == 1 && rPattern == 2 = printf "Account %d to Account %d " rName sName
            | sPattern == 2 && rPattern == 2 = printf "Account %d to Account %d " sName rName
            | sPattern == 2 && rPattern == 1 = printf "Account %d to Account %d " sName rName


--helper Function
isSuitable :: Integer -> Integer -> ChainIndexTxOut -> Bool
isSuitable n pat o = case _ciTxOutDatum o of
    Left _ -> False
    Right (Datum e) -> case PlutusTx.fromBuiltinData e of 
        Nothing -> False
        Just d -> name d == n && pattern d == pat

lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

residing' :: Integer ->  TxConstraints () AccountDatum -> AccountParam  -> Datum -> Maybe (TxConstraints () AccountDatum)
residing' x tx p dSen
            | x > 0     = Just (tx <> (Constraints.mustPayToOtherScript (valHash p) dSen $ Ada.lovelaceValueOf $ x))
            | x == 0    = Just tx
            | otherwise = Nothing

--Definitions
endpoints :: Contract () AccountSchema Text ()
endpoints = awaitPromise (init' `select` close' `select` view' `select` fund' `select` withdraw' `select` transfer' ) >> endpoints
    where
        init'     = endpoint @"init" init
        close'    = endpoint @"close" close
        view'    = endpoint @"view" view
        fund'     = endpoint @"fund" fund
        withdraw' = endpoint @"withdraw" withdraw
        transfer' = endpoint @"transfer" transfer


mkSchemaDefinitions ''AccountSchema
mkKnownCurrencies []