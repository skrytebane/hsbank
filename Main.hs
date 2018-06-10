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
import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Time.Clock        (UTCTime)
import           GHC.Generics
import           Network.Wreq
import qualified Network.Wreq.Session   as S
import           System.Environment     (getEnv)
import           System.FilePath        ((</>))
import qualified Network.URI.Encode     as UE

data Config = Config
  { customer :: String
  , apiKey   :: String
  , secret   :: String
  , expires  :: UTCTime
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

data AccountResult = AccountResult
  {
    availableItems :: Maybe Int
  , items          :: Maybe [Account]
  , errorType      :: Maybe Int
  , isError        :: Bool
  , errorMessage   :: Maybe Text
  , traceId        :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON AccountResult

data Account = Account
  {
    accountId       :: Text
  , accountNumber   :: Text
  , ownerCustomerId :: Text
  , name            :: Text
  , accountType     :: Text
  , available       :: Double -- disponibelt
  , balance         :: Double -- saldo
  , creditLimit     :: Double
  } deriving (Show, Generic)

instance FromJSON Account

getAccounts :: S.Session -> String -> Config -> IO (Maybe AccountResult)
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

readConfig :: IO Config
readConfig = do
  home <- getEnv "HOME"
  let path = home </> "secrets" </> "sbanken.json"
  s <- LB.readFile path
  case eitherDecode s of
    Left e    -> fail e
    Right cfg -> return cfg

printBalances :: [Account] -> IO ()
printBalances accts = do
  printFormat header'
  mapM_ (printFormat . formatBalance) accts
  where
    printFormat = IO.putStrLn . toStrict . toLazyText
    header' =
      right 12 ' ' ("Konto"::Text) <>
      right 22 ' ' ("Kontonamn"::Text) <>
      left 12 ' ' ("Disponibelt"::Text) <>
      left 12 ' ' ("Saldo"::Text)
    formatBalance acct =
      right 12 ' ' (accountNumber acct) <>
      right 22 ' ' (name acct) <>
      left 12 ' ' (fixed 2 $ available acct) <>
      left 12 ' ' (fixed 2 $ balance acct)

main :: IO ()
main = do
  cfg <- readConfig
  sess <- S.newSession
  token <- requestBearerToken sess cfg
  accs <- getAccounts sess token cfg
  case items <$> accs of
    Just (Just accs') -> printBalances accs'
    _ -> putStrLn "No accounts!"
