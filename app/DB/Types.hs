module DB.Types where

import Data.Int      ( Int32 )
import Data.Text     ( Text )
import Database.Beam ( Generic
                     , C
                     , Columnar
                     , Identity
                     , Table(..)
                     , Beamable
                     , TableEntity
                     , Database )

data BalancePayoutDB f = BalancePayoutDB { 
    bpdb'users         :: f (TableEntity UserT)
  , bpdb'balancePayout :: f (TableEntity BalancePayoutT) }
  deriving (Generic, Database be)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

data UserT f = User { 
    ut'userID               :: C f Int32
  , ut'dob                  :: C f Text
  , ut'joinDob              :: C f Text
  , ut'joinContribution     :: C f Double
  , ut'monthlyContribution  :: C f Double
  , ut'retirementDate       :: C f Text }
  deriving (Generic, Beamable)

instance Table UserT where
   data PrimaryKey UserT f = UserId (Columnar f Int32) deriving (Generic, Beamable)
   primaryKey :: UserT column -> PrimaryKey UserT column
   primaryKey = UserId . ut'userID

deriving instance Show User
deriving instance Eq User

data BalancePayoutT f = BalancePayout { 
    bpt'date    :: C f Text
  , bpt'balance :: C f Double
  , bpt'payout  :: C f Double
  , bpt'userID  :: PrimaryKey UserT f }
  deriving (Generic, Beamable)

type BalancePayout = BalancePayoutT Identity
type BalancePayoutID = PrimaryKey BalancePayoutT Identity

deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show BalancePayout

instance Table BalancePayoutT where
    data PrimaryKey BalancePayoutT f = BalancePayoutID (PrimaryKey UserT f) deriving (Generic, Beamable)
    primaryKey :: BalancePayoutT column -> PrimaryKey BalancePayoutT column
    primaryKey = BalancePayoutID . bpt'userID

data RunParameters = RunParameters {
    rp'inPath          :: FilePath
  , rp'outPath         :: FilePath }
  deriving (Show)