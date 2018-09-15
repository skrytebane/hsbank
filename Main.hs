{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens        (key, _String)
import qualified Data.ByteString.Lazy   as LB
import           Data.Monoid            ((<>))
import           Data.String            (fromString)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Format
import qualified Data.Text.IO           as IO
import           Data.Text.Lazy         (toStrict)
import           Data.Text.Lazy.Builder (toLazyText, singleton)
import           Data.Time.Clock        (UTCTime)
import           GHC.Generics
import           Network.Wreq
import qualified Network.Wreq.Session   as S
import           System.Environment     (getEnv)
import           System.FilePath        ((</>))
import qualified Network.URI.Encode     as UE
import qualified Accounts               as AC
import qualified Transactions           as TR

data Config = Config
  { customer :: String
  , apiKey   :: String
  , secret   :: String
  , _expires  :: UTCTime
  }
  deriving (Show, Generic)

instance FromJSON Config

requestBearerToken :: S.Session -> Config -> IO String
requestBearerToken sess cfg = do
  let opts = defaults & auth ?~ basicAuth (fromString $ UE.encode $ apiKey cfg)
                                          (fromString $ UE.encode $ secret cfg)
  res <- S.postWith opts sess "https://api.sbanken.no/identityserver/connect/token"
    [ "grant_type" := ("client_credentials"::String) ]
  let type' = res ^? responseBody . key "token_type" . _String
  case type' of
    Just "Bearer" ->
      case res ^? responseBody . key "access_token" . _String of
        Just s  -> return $ T.unpack s
        Nothing -> fail "No bearer token!"
    _ -> fail $ "Expecting bearer token, got " ++ show type'

getAccounts :: S.Session -> String -> Config -> IO (Maybe AC.AccountResult)
getAccounts sess token cfg = do
  let opts = defaults & auth ?~ oauth2Bearer (fromString token)
             & header "customerId" .~ [fromString $ customer cfg]
  res <- S.getWith opts sess "https://api.sbanken.no/bank/api/v1/Accounts/"
  case eitherDecode <$> res ^? responseBody of
    Just (Right parsed) ->
      return parsed
    Just (Left e) ->
      fail e
    Nothing ->
      fail "No response body!"

getTransactions :: S.Session -> String -> Config -> Text -> IO (Maybe TR.TransactionResult)
getTransactions sess token cfg account = do
  let opts = defaults & auth ?~ oauth2Bearer (fromString token)
             & header "customerId" .~ [fromString $ customer cfg]
             & param "length" .~ ["50"]
  res <- S.getWith opts sess $ "https://api.sbanken.no/bank/api/v1/Transactions/" ++ T.unpack account
  case eitherDecode <$> res ^? responseBody of
    Just (Right parsed) -> return parsed
    Just (Left e) -> fail e
    Nothing -> fail "No response body!"

readConfig :: IO Config
readConfig = do
  home <- getEnv "HOME"
  let path = home </> "secrets" </> "sbanken.json"
  s <- LB.readFile path
  case eitherDecode s of
    Left e    -> fail e
    Right cfg -> return cfg

formatBalances :: [AC.Account] -> Text
formatBalances accts =
  let header' =
        right 12  ' ' ("Konto"::Text) <>
        right 22  ' ' ("Kontonamn"::Text) <>
        left 12   ' ' ("Disponibelt"::Text) <>
        left 12   ' ' ("Saldo"::Text) <>
        singleton ' ' <>
        singleton '\n'
      balance' acct =
        right 12  ' ' (AC.accountNumber acct) <>
        right 22  ' ' (AC.name acct) <>
        left 12   ' ' (fixed 2 $ AC.available acct) <>
        left 12   ' ' (fixed 2 $ AC.balance acct) <>
        singleton ' ' <>
        singleton '\n'
  in
    toStrict $ toLazyText $ header' <> foldMap balance' accts

formatTransactions :: [TR.Transaction] -> Text
formatTransactions trans =
  let
    stripDate = T.takeWhile (/= 'T')
    header' =
      right 10 ' ' ("Dato"::Text) <>
      left  9  ' ' ("Beløp"::Text) <>
      singleton ' ' <>
      right 60  ' ' ("Tekst"::Text) <>
      right 10  ' ' ("Type"::Text) <>
      right 10  ' ' ("Reservasjon"::Text) <>
      singleton '\n'
    line tr =
      right 10 ' ' (stripDate $ TR.accountingDate tr) <>
      left  9 ' ' (fixed 2 $ TR.amount tr) <>
      singleton ' ' <>
      right 60 ' ' (TR.text tr) <>
      right 10 ' ' (TR.transactionType tr) <>
      right 10 ' ' (case TR.reservationType tr of
                       Just ttype -> show ttype
                       _ -> "") <>
      singleton '\n'
  in
    toStrict $ toLazyText $ header' <> foldMap line trans

printTransactions :: (Maybe TR.TransactionResult, Text) -> IO ()
printTransactions (trans, acc) =
  case trans of
    Just (TR.TransactionResult _ (Just items') _ _ _ _) -> do
      IO.putStrLn $ "* Konto: " <> acc
      IO.putStrLn $ formatTransactions items'
    _ ->
      return ()

main :: IO ()
main = do
  cfg <- readConfig
  sess <- S.newAPISession
  token <- requestBearerToken sess cfg
  accs <- getAccounts sess token cfg

  let accounts = case AC.items <$> accs of
                   Just (Just accs') -> accs'
                   _ -> []

  case length accs of
    0 -> return ()
    _ -> IO.putStrLn $ formatBalances accounts

  transactions <- getTransactions sess token cfg `traverse` (AC.accountId <$> accounts)
  mapM_ printTransactions $ zip transactions (AC.accountNumber <$> accounts)
