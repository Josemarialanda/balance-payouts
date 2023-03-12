module Csv.Stream.StreamCsv ( mkDailyAccountBalanceAndMonthlyPayout ) where

import qualified Csv.Utils as U
import qualified Csv.Types as T
import Data.Conduit ( ConduitT, (.|), await, runConduit, yield )
import Data.Conduit.Binary ( sourceFile, sinkFile )
import Data.CSV.Conduit
    ( readCSVFile,
      writeHeaders,
      runResourceT,
      CSV(fromCSV, intoCSV),
      CSVSettings,
      MapRow )
import qualified Data.Vector           as V
import Data.ByteString (ByteString)
import Data.CSV.Conduit.Conversion (FromNamedRecord(parseNamedRecord), ToNamedRecord (toNamedRecord), runParser, Named (Named))
import Control.Monad (forM_)

-- TODO: CHECK MAX AGE parameter
mkDailyAccountBalanceAndMonthlyPayout :: T.RunParameters -> IO ()
mkDailyAccountBalanceAndMonthlyPayout T.RunParameters{..} = do
  users <- readCsv @T.User U.csvFormat rp'inPath
  forM_ users $ \(Named user@T.User{..}) -> runResourceT $ runConduit $
      sourceFile rp'interestPath 
   .| intoCSV U.csvFormat        
   .| balancePayoutPipe user 0 (balancePayoutHandler user)  
   .| (writeHeaders U.csvFormat >> fromCSV U.csvFormat) 
   .| sinkFile (rp'outPath <> "_" <> show ud'userID)
  where
  balancePayoutHandler :: T.User -> Double -> T.IR -> T.BalancePayout
  balancePayoutHandler user@T.User{..} balance irData@T.IR{..}
    | ird'date < ud'joinDob  = mkBalancePayout ird'date 0.0 0.0
    | ird'date == ud'joinDob = mkBalancePayout ird'date ud'joinContribution 0.0
    | hasReachedRetirement ud'retirementDate ird'date
                             = doPayout irData balance
    | otherwise              = doContribution user irData balance ud'monthlyContribution
  
  doContribution :: T.User -> T.IR -> Double -> Double -> T.BalancePayout
  doContribution T.User{..} T.IR{..} balance contribution
    | isContributionDay ird'date &&
      (not . isJoinDay ud'joinDob) ird'date = mkBalancePayout ird'date (accrueInterest balance ird'rate + contribution) 0.0
    | otherwise                             = mkBalancePayout ird'date (accrueInterest balance ird'rate) 0.0
  
  doPayout :: T.IR -> Double -> T.BalancePayout
  doPayout T.IR{..} balance
    | isPayoutDay ird'date = let payout = balance * U.percentToDecimal rp'payoutRate
                                 balance' = accrueInterest (balance - payout) ird'rate
                              in mkBalancePayout ird'date balance' payout
    | otherwise            = mkBalancePayout ird'date (accrueInterest balance ird'rate) 0.0
  
  isPayoutDay ::  T.Day -> Bool
  isPayoutDay date = U.day date == rp'payoutDay
  
  hasReachedRetirement ::  T.Day -> T.Day -> Bool
  hasReachedRetirement retirementDate date = date >= retirementDate
  
  isJoinDay :: T.Day -> T.Day -> Bool
  isJoinDay joinDob date = joinDob == date
  
  isContributionDay :: T.Day -> Bool
  isContributionDay date = U.day date == rp'contributionDay


  mkBalancePayout :: T.Day -> Double -> Double -> T.BalancePayout
  mkBalancePayout = T.BalancePayout
  
  accrueInterest :: Double -> Double -> Double
  accrueInterest balance rate = balance + (U.percentToDecimal balance * rate)

  balancePayoutPipe :: (Monad m) => T.User
                                      -> Double 
                                      -> (Double -> T.IR -> T.BalancePayout) 
                                      -> ConduitT (MapRow ByteString) (MapRow ByteString) m ()
  balancePayoutPipe user@T.User{..} balance handler = await >>= \case
    Nothing -> return ()
    Just m -> do 
      let irDataParser = parseNamedRecord @T.IR m
          balancePayoutData@T.BalancePayout{..} = case runParser irDataParser of
            Left e  -> error $ "Malformed CSV:\n" <> e
            Right r -> handler balance r
          balancePayoutDataRecord = toNamedRecord balancePayoutData
      if bpd'date < ud'dob
      then balancePayoutPipe user bpd'balance handler
      else do
        yield balancePayoutDataRecord
        balancePayoutPipe user bpd'balance handler
  
  readCsv :: (FromNamedRecord a, ToNamedRecord a) => CSVSettings -> FilePath -> IO [Named a]
  readCsv format filepath = V.toList <$> readCSVFile format filepath