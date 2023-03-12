# TODO: 

* Upload CSV to DB in chunks rather than doing single IO operations... lol

* Make command line interface

* Check max age parameter

# Interview: Daily Balance & Payout

Your task is to write a program that can calculate a daily account balance and
monthly payout for each user based on a given formula, until each user has
reached some maximum age. Input parameters to the problem are given via the
command line, and some input data is then read from a CSV file. Your program
should write the results to a database using the Haskell `beam-*` packages.

## Assessment

We are looking for:
- clean maintainable code
- low memory usage
- leverage available cores
- type-safety
- leverage Haskell libraries

## Input

### Command Line Parameters

Your program should read the following parameters from the command line:
- `-payout-rate`:   percentage of balance to payout (1.2% represented as 1.2)
- `-max-age`:       last age in years for which to calculate balance & payout
- `-interest-path`: absolute path to a CSV file containing daily interest rates
- `-in-path`:       absolute path to a CSV file containing the user data
- `-out-dir`:       absolute path to a directory of where to write the results to

Example (doesn't need to be exactly the same):

```
cabal run interview -- -payout-rate 2 -max-age 120 \
  -interest-path /foo/interest.csv -in-file /foo/bar.csv -out-dir /foo/out
```

### Interest Rate Data

A simple CSV with two columns. The interest rate is given as a percentage
meaning that 1.2% will be represented as 1.2. Example format:

```
date, rate
1950-01-01, 0.1
1950-01-02, 0.12
1950-01-03, 0.1
```

You can find the interest rates file here:
https://github.com/TontineTrust/balance-payouts-interview/blob/main/rates.csv

### User Data

The CSV file given by `in-path` contains data on each user. This file could
potentially be massive (millions of users). The format of the CSV file:

```
userID, dob, joinDob, joinContribution, monthlyContribution, retirementDate
1234, 21/12/1970, 16/08/2022, 100000, 100, 21/12/2035
```

Here is a description of each field:
- userID: unique identifier for each user
- dob: user's date of birth given in yyyy-mm-dd
- joinDob: date the user made their first contribution in yyyy-mm-dd
- joinContribution: amount of money the user contributed on `joinDob`
- monthlyContribution: amount of money the user deposits on the 1st of each
  month, but not on the day they joined if they joined on the 1st
- retirementDate: the user will receive a payout on the 25th of each month, on
  or after their retirment date, given in dd/mm/yyy

### Example Calculation

`cabal run interview -- -payout-rate 2 -max-age 1200 ...`

Interest rate CSV contains the fixed rate of `0.1` for each day.

User data CSV contains one user:
`1234, 21/12/1970, 31/08/2022, 100000, 100, 21/12/2035`

```
21/12/1970: balance = 0
31/08/2022: balance = 100000
01/09/2022: balance = (prevDayBalance * 1.001) + 100
02/09/2022: balance = prevDayBalance * 1.001
...
25/12/2035: payout  = prevDayBalance * 0.02
            balance = (preDayBalance - payout) * 1.001
```

A user's balance cannot be negative.

### Output Data

The command line parameters give an output directory `-out-dir` e.g. `/foo/out`:

For each user with user ID X the output data should be written to `/foo/out/X.csv`

For example, user 1234's output file `/foo/out/1234.csv` would start like:

```
date, balance, payout
21/12/1970, 0, 0
22/12/1970, 0, 0
```
