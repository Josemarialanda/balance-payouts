module DB.Stream.StreamDB ( uploadCsv ) where

import qualified Csv.Utils         as U
import qualified Csv.Types         as CSVT
import qualified DB.Types          as DBT
import qualified DB.DB             as DB
import Data.Conduit                ( ConduitT
                                   , (.|)
                                   , await
                                   , runConduit )
import Data.Conduit.Binary         ( sourceFile )
import Data.CSV.Conduit            ( runResourceT
                                   , CSV ( intoCSV )
                                   , MapRow ) 
import Data.ByteString             ( ByteString )
import Data.Conduit.Combinators    ( sinkNull )
import Conduit                     ( ResourceT
                                   , MonadIO ( liftIO )
                                   , Identity )
import Data.CSV.Conduit.Conversion ( runParser )

uploadCsv :: CSVT.User -> FilePath -> IO ()
uploadCsv user outPath = do
  userRef <- DB.insertUser user
  runResourceT $ runConduit $
    sourceFile outPath
      .| intoCSV U.csvFormat
      .| dbPipe userRef
      .| sinkNull

dbPipe :: DBT.UserT Identity -> ConduitT (MapRow ByteString) (MapRow ByteString) (ResourceT IO) ()
dbPipe user = await >>= \case
  Nothing -> return ()
  Just m -> do 
    let balancePayoutParser = U.parseNamedRecord @CSVT.BalancePayout m
    case runParser balancePayoutParser of
      Left e  -> error $ "Malformed CSV:\n" <> e
      Right r -> liftIO $ DB.insertBalancePayout user r
    dbPipe user