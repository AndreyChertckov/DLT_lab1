{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Part2 (part2) where

import           Control.Applicative
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField

newtype Amount = Amount {unAmount :: Float} deriving (Num, Show)

feesAmount = Amount 30

instance FromField Amount where
  fromField f = Amount <$> (fromField f)

instance ToField Amount where
  toField = toField . unAmount


data Account = Account { id_     :: Int
                       , name    :: [Char] 
                       , credits :: Amount
                       , bank    :: [Char]
                       } deriving (Show)

instance FromRow Account where
  fromRow = Account <$> field <*> field <*> field <*> field

instance ToRow Account where
  toRow Account{..} = toRow (id_, name, unAmount credits, bank)

makeSimpleTransaction 
  :: Amount
  -> Account -- ^ From account
  -> Account -- ^ To account
  -> Account -- ^ Fees account
  -> (Account, Account, Account) -- ^ (From account, To account)
makeSimpleTransaction amount fromAccount toAccount feesAccount = (resultFromAccount, resultToAccount, resultFeesAccount)
  where
    resultFromAccount = fromAccount {credits = credits fromAccount - amount}
    newAmount = if bank fromAccount == bank toAccount
                then amount
                else amount - feesAmount
    resultToAccount = toAccount {credits = credits toAccount + newAmount}
    resultFeesAccount = feesAccount {credits = credits feesAccount + (amount - newAmount)}

makeTransactionInDataBase
  :: Connection
  -> Amount
  -> Account
  -> Account
  -> Account
  -> IO ()
makeTransactionInDataBase conn amount (Account fromId _ _ _) (Account toId _ _ _) (Account feesId _ _ _) = do
    (fromAccount : _) <- query conn "SELECT * FROM accounts WHERE id = ?" (Only fromId) :: IO [Account]
    (toAccount : _) <- query conn "SELECT * FROM accounts WHERE id = ?" (Only toId) :: IO [Account]
    (feesAccount : _) <- query conn "SELECT * FROM accounts WHERE id = ?" (Only feesId) :: IO [Account]
    let (resultFromAccount, resultToAccount, resultFeesAccount) = makeSimpleTransaction amount fromAccount toAccount feesAccount
    execute conn "UPDATE accounts SET credits = (?) WHERE id = ?" (credits resultFromAccount, id_ resultFromAccount) 
    execute conn "UPDATE accounts SET credits = (?) WHERE id = ?" (credits resultToAccount, id_ resultToAccount) 
    execute conn "UPDATE accounts SET credits = (?) WHERE id = ?" (credits resultFeesAccount, id_ resultFeesAccount) 


-- Add BankName field to the table
-- Account 1 & 3 is SpearBank, Account 2 is Tinkoff.
-- Internal fees is 0.
-- External is 30 Rub.
-- Fees should be saved in new Record(Account 4).
-- Generate and insert 3 accounts into the table, each account has 1000 Rub.
-- Create Transactions:
-- T1. Account 1 send 500 RUB to Account 3
-- T2. Account 2 send 700 RUB to Account 1
-- T3. Account 2 send 100 RUB to Account 3
-- Return the amount Credit for all Account.
part2 :: IO ()
part2 = do
  conn <- open "part2.db"
  let acc1 = Account 1 "Account 1" 1000 "SpearBank"
  let acc2 = Account 2 "Account 2" 1000 "Tinkoff"
  let acc3 = Account 3 "Account 3" 1000 "SpearBank"
  let acc4 = Account 4 "Fees" 0 "Central"
  execute_ conn "CREATE TABLE IF NOT EXISTS accounts (id INTEGER PRIMARY KEY, name TEXT, credits FLOAT, bank TEXT)"
  execute conn "INSERT INTO accounts (id, name, credits, bank) VALUES (?,?,?,?)" (acc1)
  execute conn "INSERT INTO accounts (id, name, credits, bank) VALUES (?,?,?,?)" (acc2)
  execute conn "INSERT INTO accounts (id, name, credits, bank) VALUES (?,?,?,?)" (acc3)
  execute conn "INSERT INTO accounts (id, name, credits, bank) VALUES (?,?,?,?)" (acc4)
  makeTransactionInDataBase conn 500 acc1 acc3 acc4
  makeTransactionInDataBase conn 700 acc2 acc1 acc4
  makeTransactionInDataBase conn 100 acc2 acc3 acc4
  accounts <- query_ conn "SELECT * FROM accounts" :: IO [Account]
  (putStrLn . show) accounts
