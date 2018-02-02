{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens      (key, _String)
import           Data.Maybe           (maybe)
import           Data.String          (fromString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics
import           Network.Wreq
import qualified Network.Wreq.Session as S
import           System.Environment   (getEnv)
import           System.FilePath      ((</>))
import           System.IO

data Config = Config
  { customer :: Text
  , apiKey   :: Text
  , secret   :: Text
  }
  deriving (Show, Generic)

instance FromJSON Config

requestBearerToken :: S.Session -> Config -> IO String
requestBearerToken sess (Config _ u p) = do
  let opts = defaults & auth ?~ basicAuth (fromString $ T.unpack u)
                                          (fromString $ T.unpack p)
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
    availableItems :: Int
  , items          :: [Account]
  , errorType      :: Maybe Text
  , isError        :: Bool
  , errorMessage   :: Maybe Text
  , traceId        :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON AccountResult

data Account = Account
  {
    ownerCustomerId :: Text
  , balance         :: Double
  , customerId      :: Text
  , name            :: Text
  , defaultAccount  :: Bool
  , accountNumber   :: Text
  , creditLimit     :: Double
  , accountType     :: Text
  , available       :: Double
  } deriving (Show, Generic)

instance FromJSON Account

getAccounts :: S.Session -> String -> Config -> IO (Maybe AccountResult)
getAccounts sess token (Config uid _ _) = do
  let opts = defaults & auth ?~ oauth2Bearer (fromString token)
  res <- S.getWith opts sess $
    "https://api.sbanken.no/bank/api/v1/Accounts/" ++ T.unpack uid
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
  withFile path ReadMode $
    \handle ->
      do s <- hGetContents handle
         case eitherDecode $ fromString s of
           Left e    -> fail e
           Right cfg -> return cfg

printBalances :: [Account] -> IO ()
printBalances =
  mapM_ printBalance
  where printBalance (Account _ bal _ name' _ num _ _ avail) =
          putStrLn $ T.unpack num ++ ": " ++ T.unpack name' ++
          ": " ++ show bal ++ " (saldo: " ++ show avail ++ ")"

main :: IO ()
main = do
  cfg <- readConfig
  sess <- S.newSession
  token <- requestBearerToken sess cfg
  accs <- getAccounts sess token cfg
  printBalances $ maybe [] items accs
