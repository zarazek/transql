{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Sql.Tokenizer
import Language.Sql.Parser
import Language.Sql.PrettyPrinter

example1 = "CREATE TABLE IF NOT EXISTS Employees (\
\              login     VARCHAR(8) PRIMARY KEY,\
\              password  VARCHAR(15) NOT NULL,\
\              name      VARCHAR(100) NOT NULL,\
\              active    BOOL NOT NULL DEFAULT TRUE)"

example2 = "CREATE TABLE IF NOT EXISTS Tasks (\
\              id          INTEGER PRIMARY KEY AUTOINCREMENT,\
\              title       VARCHAR(100) NOT NULL UNIQUE,\
\              description VARCHAR(1000) NOT NULL,\
\              status      INTEGER NOT NULL DEFAULT 0)"

example3 = "CREATE TABLE IF NOT EXISTS EmployeesTasks (\
\              employee          REFERENCES Employees(login),\
\              task              REFERENCES Tasks(id),\
\              assignment_acitve BOOL NOT NULL DEFAULT TRUE,\
\              finished          BOOL NOT NULL DEFAULT FALSE,\
\              time_spent        INTEGER NOT NULL DEFAULT 0,\
\              PRIMARY KEY (employee, task))"

example4 = "INSERT OR IGNORE INTO Employees(login, password, name) VALUES\
\             ('ybarodzi', 'pass1', 'Yauheni Barodzich'   ),\
\             ('mlukashe', 'pass2', 'Mikhail Lukashevich' ),\
\             ('tlukashe', 'pass3', 'Tatsiana Lukashevich'),\
\             ('wwisniew', 'pass4', 'Wojciech Wiśniewski' )"

example5 = "INSERT OR IGNORE INTO Tasks(title, description) VALUES\
\              ('Pompowanie przedniego koła', 'Zadanie polega na napompowaniu przedniego koła roweru.\nSzybciutko!'),\
\              ('Pompowanie tylnego koła', 'Zadanie polega na napompowaniu tylnego koła roweru.\nPrędziutko!'),\
\              ('Smarowanie łańcucha', 'Zadanie polega na nasmarowaniu łańcucha rowerowego.\nMigiem!')"

example6 = "INSERT OR IGNORE INTO EmployeesTasks(employee, task)\
\                     SELECT E.login, T.id\
\                     FROM Employees AS E JOIN Tasks AS T\
\                     WHERE E.name = 'Yauheni Barodzich' AND T.title = 'Pompowanie przedniego koła'\
\           UNION ALL SELECT E.login, T.id\
\                     FROM Employees AS E JOIN Tasks as T\
\                     WHERE E.name = 'Mikhail Lukashevich' AND T.title = 'Pompowanie tylnego koła'\
\           UNION ALL SELECT E.login, T.id\
\                     FROM Employees AS E JOIN Tasks as T\
\                     WHERE E.name = 'Tatsiana Lukashevich' AND T.title = 'Smarowanie łańcucha'\
\           UNION ALL SELECT E.login as employee, T.id AS task\
\                     FROM Employees AS E JOIN Tasks as T\
\                     WHERE E.name = 'Wojciech Wiśniewski' AND T.title = 'Smarowanie łańcucha'"

example7 = "()()lekarz;;;"

printParsingResult str = case tokenize str of
                           Left err -> putStrLn err
                           Right tokens -> mapM_ (putStrLn . printStatement) $ fst $ parse $ filter significant tokens

main :: IO ()
main = mapM_ pp [ example1, example2, example3, example4, example5, example6, example7 ] 
  where pp str = printParsingResult str >> putStrLn ""
