{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Part3 (part3) where

import           Control.Applicative
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Data.DateTime

newtype Amount = Amount {unAmount :: Float} deriving (Num, Show)

feeAmount = Amount 30

instance FromField Amount where
  fromField f = Amount <$> (fromField f)

instance ToField Amount where
  toField = toField . unAmount


data Account = Account { accountId     :: Int
                       , name          :: [Char] 
                       , credits       :: Amount
                       , bank          :: [Char]
                       } deriving (Show)

instance FromRow Account where
  fromRow = Account <$> field <*> field <*> field <*> field

instance ToRow Account where
  toRow Account{..} = toRow (accountId, name, unAmount credits, bank)


data Transaction = Transaction { transactionID       :: Maybe Int
                               , from                :: Int
                               , to                  :: Int
                               , fee                 :: Amount
                               , amount              :: Amount
                               , transactionDateTime :: Maybe DateTime
                               } deriving (Show)

instance FromRow Transaction where
  fromRow = Transaction <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Transaction where
  toRow Transaction{..} = toRow (transactionID, from, to, fee, amount, transactionDateTime)

applyTransaction 
  :: Transaction
  -> Account -- ^ From account
  -> Account -- ^ To account
  -> Account -- ^ Fees account
  -> (Account, Account, Account) -- ^ (From account, To account)
applyTransaction Transaction{..} fromAccount toAccount feesAccount = (resultFromAccount, resultToAccount, resultFeesAccount)
  where
    resultFromAccount = fromAccount {credits = credits fromAccount - amount}
    newAmount = amount - fee 
    resultToAccount = toAccount {credits = credits toAccount + newAmount}
    resultFeesAccount = feesAccount {credits = credits feesAccount + fee}

makeSimpleTransaction
  :: Amount
  -> Account
  -> Account
  -> Transaction
makeSimpleTransaction amount fromAccount toAccount = Transaction {transactionID = Nothing, from = accountId fromAccount, to = accountId toAccount, fee = transactionFee, amount = amount, transactionDateTime = Nothing}
  where
    transactionFee = if bank fromAccount == bank toAccount then 0 else feeAmount

makeTransactionInDataBase
  :: Connection
  -> Amount
  -> Account
  -> Account
  -> Account
  -> IO ()
makeTransactionInDataBase conn amount' (Account fromId _ _ _) (Account toId _ _ _) (Account feesId _ _ _) = do
    (fromAccount : _) <- query conn "SELECT * FROM accounts WHERE id = ?" (Only fromId) :: IO [Account]
    (toAccount : _) <- query conn "SELECT * FROM accounts WHERE id = ?" (Only toId) :: IO [Account]
    (feesAccount : _) <- query conn "SELECT * FROM accounts WHERE id = ?" (Only feesId) :: IO [Account]
    let transaction = makeSimpleTransaction amount' fromAccount toAccount
    let (resultFromAccount, resultToAccount, resultFeesAccount) = applyTransaction transaction fromAccount toAccount feesAccount
    execute conn "UPDATE accounts SET credits = (?) WHERE id = ?" (credits resultFromAccount, accountId resultFromAccount) 
    execute conn "UPDATE accounts SET credits = (?) WHERE id = ?" (credits resultToAccount, accountId resultToAccount) 
    execute conn "UPDATE accounts SET credits = (?) WHERE id = ?" (credits resultFeesAccount, accountId resultFeesAccount) 
    execute conn "INSERT INTO transactions (from_, to_, fee, amount) VALUES (?,?,?,?)" (from transaction, to transaction, fee transaction, amount transaction)


-- Create Table Called Ledger to show all transactions:
--     1. ID (unique)
--     2. From (ID)
--     3. To (ID)
--     4. Fee (Rub)
--     5. Amount (Rub)
--     6. TransactionDateTime (DateTime)
-- Modify Exercise 1 & 2 To save all transaction inside this table
part3 :: IO ()
part3 = do
  conn <- open "part3.db"
  let acc1 = Account 1 "Account 1" 1000 "SpearBank"
  let acc2 = Account 2 "Account 2" 1000 "Tinkoff"
  let acc3 = Account 3 "Account 3" 1000 "SpearBank"
  let acc4 = Account 4 "Fees" 0 "Central"
  execute_ conn "CREATE TABLE IF NOT EXISTS accounts (id INTEGER PRIMARY KEY, name TEXT, credits FLOAT, bank TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS transactions (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, from_ INTEGER, to_ INTEGER, fee FLOAT, amount FLOAT, DATETIME DEFAULT CURRENT_TIMESTAMP)"
  execute conn "INSERT INTO accounts (id, name, credits, bank) VALUES (?,?,?,?)" (acc1)
  execute conn "INSERT INTO accounts (id, name, credits, bank) VALUES (?,?,?,?)" (acc2)
  execute conn "INSERT INTO accounts (id, name, credits, bank) VALUES (?,?,?,?)" (acc3)
  execute conn "INSERT INTO accounts (id, name, credits, bank) VALUES (?,?,?,?)" (acc4)
  makeTransactionInDataBase conn 500 acc1 acc3 acc4
  makeTransactionInDataBase conn 700 acc2 acc1 acc4
  makeTransactionInDataBase conn 100 acc2 acc3 acc4
  accounts <- query_ conn "SELECT * FROM accounts" :: IO [Account]
  transactions <- query_ conn "SELECT * FROM transactions" :: IO [Transaction]
  (putStrLn . show) accounts
  (putStrLn . show) transactions
