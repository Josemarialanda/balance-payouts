module Csv.Types ( IR (..) 
                 , User (..) 
                 , BalancePayout (..)
                 , RunParameters (..)
                 , module Data.Time
                 , module Data.Conduit
                 , FromNamedRecord
                 , ToNamedRecord ) where

import qualified Csv.Utils         as U
import Data.Time                   ( Day )
import Control.Monad               ( MonadPlus ( mzero ) )
import Data.Conduit                ( ConduitT
                                   , Void )  
import Data.CSV.Conduit.Conversion ( FromNamedRecord
                                   , ToNamedRecord
                                   , ToNamedRecordOrdered
                                   , NamedRecordOrdered
                                   , NamedRecord
                                   , Field
                                   , Parser
                                   , ToField (..)
                                   , FromField (..)
                                   , parseNamedRecord
                                   , toNamedRecord
                                   , namedRecord
                                   , namedRecordOrdered
                                   , toNamedRecordOrdered
                                   , (.:), (.=) )   
import GHC.Generics (Generic)

data RunParameters = RunParameters {
    rp'interestPath    :: FilePath
  , rp'inPath          :: FilePath
  , rp'outPath         :: FilePath
  , rp'payoutRate      :: Double
  , rp'maxAge          :: Integer
  , rp'payoutDay       :: Int
  , rp'contributionDay :: Int }
  deriving (Show)

data BalancePayout = BalancePayout {
    bpd'date    :: !Day
  , bpd'balance :: !Double
  , bpd'payout  :: !Double }
  deriving (Show)

instance FromNamedRecord BalancePayout where
  parseNamedRecord :: NamedRecord -> Parser BalancePayout
  parseNamedRecord m = BalancePayout <$> m .: "date" <*> m .: "balance" <*> m .: "payout"

instance ToNamedRecord BalancePayout where
  toNamedRecord :: BalancePayout -> NamedRecord
  toNamedRecord BalancePayout{..} = namedRecord [ "date" .= bpd'date, "balance" .= bpd'balance, "payout" .= bpd'payout ]

instance ToNamedRecordOrdered BalancePayout where
  toNamedRecordOrdered :: BalancePayout -> NamedRecordOrdered
  toNamedRecordOrdered BalancePayout{..} = namedRecordOrdered [ "date" .= bpd'date, "balance" .= bpd'balance, "payout" .= bpd'payout ]

data IR = IR {
    ird'date :: !Day
  , ird'rate :: !Double }
  deriving (Show)

instance FromNamedRecord IR where
  parseNamedRecord :: NamedRecord -> Parser IR
  parseNamedRecord m = IR <$> m .: "date" <*> m .: "rate"

instance ToNamedRecord IR where
  toNamedRecord :: IR -> NamedRecord
  toNamedRecord IR{..} = namedRecord [ "date" .= ird'date, "rate" .= ird'rate ]

data User = User {
    ud'userID              :: !Int
  , ud'dob                 :: !Day 
  , ud'joinDob             :: !Day
  , ud'joinContribution    :: !Double
  , ud'monthlyContribution :: !Double
  , ud'retirementDate      :: !Day } 
  deriving (Show, Generic)

instance FromNamedRecord User where
  parseNamedRecord :: NamedRecord -> Parser User
  parseNamedRecord m = User
    <$> m .: "userID" 
    <*> m .: "dob" 
    <*> m .: "joinDob"
    <*> m .: "joinContribution"
    <*> m .: "monthlyContribution"
    <*> m .: "retirementDate"

instance ToNamedRecord User where
  toNamedRecord :: User -> NamedRecord
  toNamedRecord User{..} = namedRecord 
    [ "userID" .= ud'userID
    , "dob" .= ud'dob
    , "joinDob" .= ud'joinDob
    , "joinContribution" .= ud'joinContribution
    , "monthlyContribution" .= ud'monthlyContribution
    , "retirementDate" .= ud'retirementDate
    ]

instance FromField Day where
    parseField :: Field -> Parser Day
    parseField d = either (const mzero) pure $ U.parseDate $ U.strictByteStringToStrictText d

instance ToField Day where
    toField :: Day -> Field
    toField d = U.stringToStrictByteString $ show d