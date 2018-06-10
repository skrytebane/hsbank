{-# LANGUAGE DeriveGeneric #-}

module Accounts where

import           Data.Aeson
import           Data.Text              (Text)
import           GHC.Generics

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
