{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Data.Aeson.Lens      (key, _String)
import           Data.String          (fromString)
import qualified Data.Text            as T
import           Data.Yaml
import           GHC.Generics
import           Network.Wreq
import qualified Network.Wreq.Session as S
import           System.Environment   (getEnv)
import           System.FilePath      ((</>))

tokenUrl :: String
tokenUrl = "https://api.sbanken.no/identityserver/connect/token"

data Config = Config
  {
    customerId :: String
  , apiKey     :: String
  , secret     :: String
  } deriving (Show, Generic)

instance FromJSON Config

requestBearerToken :: S.Session -> Config -> IO String
requestBearerToken sess (Config _ u p) = do
  let opts = defaults & auth ?~ basicAuth (fromString u) (fromString p)
                      & header "User-Agent" .~ ["hsbank"]
  res <- S.postWith opts sess tokenUrl
    [ "grant_type" := ("client_credentials"::String) ]
  let type' = res ^? responseBody . key "token_type" . _String
  case type' of
    Just "bearer" ->
      case res ^? responseBody . key "access_token" . _String of
        Just s  -> return $ T.unpack s
        Nothing -> fail "No bearer token!"
    _ -> fail "Expecting bearer token."

readConfig :: IO Config
readConfig = do
  home <- getEnv "HOME"
  res <- decodeFile $ home </> "secrets" </> "sbanken.yaml"
  case res of
    Nothing  -> fail "Invalid or non-existent secrets file."
    Just cfg -> return cfg

main :: IO ()
main = do
  cfg <- readConfig
  sess <- S.newSession
  token <- requestBearerToken sess cfg
  print token
