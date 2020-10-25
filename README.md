# DLT lab1 Exercise 1

## Results

### Part 1
```
[Account {id_ = 1, name = "Account 1", credits = Amount {unAmount = 1200.0}},Account {id_ = 2, name = "Account 2", credits = Amount {unAmount = 200.0}},Account {id_ = 3, name = "Account 3", credits = Amount {unAmount = 1600.0}}]
```

### Part 2
```
[Account {id_ = 1, name = "Account 1", credits = Amount {unAmount = 1170.0}, bank = "SpearBank"},Account {id_ = 2, name = "Account 2", credits = Amount {unAmount = 200.0}, bank = "Tinkoff"},Account {id_ = 3, name = "Account 3", credits = Amount {unAmount = 1570.0}, bank = "SpearBank"},Account {id_ = 4, name = "Fees", credits = Amount {unAmount = 60.0}, bank = "Central"}]
```

### Part 3
```
[Account {accountId = 1, name = "Account 1", credits = Amount {unAmount = 1170.0}, bank = "SpearBank"},Account {accountId = 2, name = "Account 2", credits = Amount {unAmount = 200.0}, bank = "Tinkoff"},Account {accountId = 3, name = "Account 3", credits = Amount {unAmount = 1570.0}, bank = "SpearBank"},Account {accountId = 4, name = "Fees", credits = Amount {unAmount = 60.0}, bank = "Central"}]
[Transaction {transactionID = Just 1, from = 1, to = 3, fee = Amount {unAmount = 0.0}, amount = Amount {unAmount = 500.0}, transactionDateTime = Just 2020-10-25 20:59:54 UTC},Transaction {transactionID = Just 2, from = 2, to = 1, fee = Amount {unAmount = 30.0}, amount = Amount {unAmount = 700.0}, transactionDateTime = Just 2020-10-25 20:59:54 UTC},Transaction {transactionID = Just 3, from = 2, to = 3, fee = Amount {unAmount = 30.0}, amount = Amount {unAmount = 100.0}, transactionDateTime = Just 2020-10-25 20:59:54 UTC}]
```
