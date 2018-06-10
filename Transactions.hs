{-# LANGUAGE DeriveGeneric #-}

module Transactions where

import           Data.Aeson
import           Data.Text              (Text)
import           GHC.Generics

data TransactionResult = TransactionResult
  {
    availableItems :: Maybe Int
  , items          :: Maybe [Transaction]
  , errorType      :: Maybe Int
  , isError        :: Bool
  , errorMessage   :: Maybe Text
  , traceId        :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON TransactionResult

data Transaction = Transaction
  {
    accountingDate :: Text
  , interestDate :: Text
  , otherAccountNumber :: Maybe Text
  , otherAccountNumberSpecified :: Bool
  , amount :: Double
  , text :: Text
  , transactionType :: Text
  , transactionTypeCode :: Int
  , transactionTypeText :: Text
  , isReservation :: Bool
  , reservationType :: Maybe ReservationType
  , source :: SourceType
  , cardDetails :: Maybe CardDetails
  , cardDetailsSpecified :: Bool
  } deriving (Show, Generic)

instance FromJSON Transaction

data ReservationType =
  NotReservation
  | VisaReservation
  | PurchaseReservation
  | AtmReservation
  deriving (Show, Generic)

instance FromJSON ReservationType where
  parseJSON (Number n) =
    case n of
      0 -> pure NotReservation
      1 -> pure VisaReservation
      2 -> pure PurchaseReservation
      3 -> pure AtmReservation
      _ -> mempty
  parseJSON _ = mempty

data SourceType =
  AccountStatement
  | Archive
  deriving (Show, Generic)

instance FromJSON SourceType where
  parseJSON (Number n) =
    case n of
      0 -> pure AccountStatement
      1 -> pure Archive
      _ -> mempty
  parseJSON _ = mempty

data CardDetails = CardDetails
  {
    cardNumber :: Text
  , currencyAmount :: Maybe Double
  , currencyRate :: Maybe Double
  , merchantCategoryCode :: Maybe Text
  , merchantCategoryDescription :: Maybe Text
  , merchantCity :: Maybe Text
  , merchantName :: Maybe Text
  , originalCurrencyCode :: Maybe Text
  , purchaseDate :: Maybe Text
  , transactionId :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON CardDetails
