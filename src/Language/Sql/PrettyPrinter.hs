{-# LANGUAGE LambdaCase #-}

module Language.Sql.PrettyPrinter where

import Language.Sql.Syntax
import Data.Text (unpack)
import Data.List (intersperse)
import Data.Bool (bool)
import Data.Ratio ((%))

printStatement = \case
  CreateTable { ctTemporary = temporary
              , ctIfNotExists = ifNotExists
              , ctName = name
              , ctColumns = columns
              , ctTableConstraints = constraints } -> printCreateTable temporary ifNotExists name columns constraints
  _ -> "something"

flag b txt = bool "" txt b
together = concat . intersperse " " . filter (not . null)

printCreateTable temporary ifNotExists name columns constraints = together lst
  where lst = [ "CREATE"
              , flag temporary "TEMPORARY"
              , "TABLE"
              , flag ifNotExists "IF NOT EXISTS"
              , printTableName name
              , "("
              , concat $ intersperse ", " $ map printColumnDefinition columns
              , together $ map ((", " ++) . printTableConstraint) constraints
              , ")" ]

printTableName (TableName schema table) = maybe "" ((++ ".") . unpack) schema ++ unpack table

printColumnDefinition (ColumnDefinition name type_ constraints) = together lst
  where lst = [ unpack name
              , maybe "" printTypeName type_
              , together $ map printColumnConstraint constraints ]

printTypeName (TypeName names precision) = together (map unpack names) ++ printPrecision precision
         
printPrecision = \case
                    PrecisionNone -> ""
                    PrecisionLength l -> "(" ++ show l ++ ")"
                    PrecisionLengthAndPrecision l p -> "(" ++ show l ++ ", " ++ show p ++ ")"

printColumnConstraint (ColumnConstraint name type_) = together lst
  where lst = [ maybe "" (("CONSTRAINT " ++) . unpack) name
              , printColumnConstraintType type_ ]

printColumnConstraintType = \case
  CCTPrimaryKey sortingType conflictClause autoincrement -> together [ "PRIMARY KEY"
                                                                     , printSortingType sortingType
                                                                     , printConflictClause conflictClause
                                                                     , flag autoincrement "AUTOINCREMENT" ]
  CCTNotNull conflictClause -> together [ "NOT NULL"
                                        , printConflictClause conflictClause ]
  CCTUnique conflictClause -> together [ "UNIQUE"
                                       , printConflictClause conflictClause ]
  CCTCheck expr -> "CHECK (" ++ printExpression expr ++ ")"
  CCTDefault expr -> "DEFAULT " ++ printExpression expr
  CCTCollate name -> "COLLATE " ++ unpack name
  CCTForeignKey clause -> "REFERENCES " ++ printForeignKeyClause clause

printSortingType = \case
  SortingTypeNone -> ""
  SortingTypeAscending -> "ASC"
  SortingTypeDescending -> "DESC"

printConflictClause = \case
  ConflictClauseNone -> ""
  ConflictClauseRollback -> "ON CONFLICT ROLLBACK"
  ConflictClauseAbort -> "ON CONFLICT ABORT"
  ConflictClauseFail -> "ON CONFLICT FAIL"
  ConflictClauseIgnore -> "ON CONFLICT IGNORE"
  ConflictClauseReplace -> "ON CONFLICT REPLACE"

printExpression = \case
  LiteralExpr lit -> printLiteral lit
  ColumnExpr name -> printColumnName name
  NegateExpr e -> unary "-" e
  BitwiseNotExpr e -> unary "~" e
  LogicalNotExpr e -> unary "NOT " e
  ConcatenateExpr e1 e2 -> binary "||" e1 e2
  MultiplyExpr e1 e2 -> binary "*" e1 e2
  DivideExpr e1 e2 -> binary "/" e1 e2
  ModuloExpr e1 e2 -> binary "%" e1 e2
  AddExpr e1 e2 -> binary "+" e1 e2
  SubtractExpr e1 e2 -> binary "-" e1 e2
  ShiftLeftExpr e1 e2 -> binary "<<" e1 e2
  ShiftRightExpr e1 e2 -> binary ">>" e1 e2
  BitwiseAndExpr e1 e2 -> binary "&" e1 e2
  BitwiseOrExpr e1 e2 -> binary "|" e1 e2
  LessExpr e1 e2 -> binary "<" e1 e2
  LessOrEqualExpr e1 e2 -> binary "<=" e1 e2
  GreaterExpr e1 e2 -> binary ">" e1 e2
  GreaterOrEqualExpr e1 e2 -> binary ">=" e1 e2
  EqualExpr e1 e2 -> binary "=" e1 e2
  IsExpr e1 e2 -> binary "IS" e1 e2
  InExpr e1 e2 -> binary "IN" e1 e2
  LikeExpr e1 e2 -> binary "LIKE" e1 e2
  GlobExpr e1 e2 -> binary "GLOB" e1 e2
  MatchExpr e1 e2 -> binary "MATCH" e1 e2
  RegexpExpr e1 e2 -> binary "REGEXP" e1 e2
  LogicalAndExpr e1 e2 -> binary "AND" e1 e2
  LogicalOrExpr e1 e2 -> binary "OR" e1 e2
  CollateExpr e name -> printExpression e ++ " COLLATE " ++ unpack name
  where unary str e = str ++ inParens e
        binary str e1 e2 = inParens e1 ++ " " ++ str ++ " " ++ inParens e2
        inParens e = "(" ++ printExpression e ++ ")"

printLiteral = \case
  Null -> "NULL"
  FalseL -> "FALSE"
  TrueL -> "TRUE"
  CurrentTime -> "CURRENT_TIME"
  CurrentDate -> "CURRENT_DATE"
  CurrentTimestamp -> "CURRENT_TIMESTAMP"
  IntegerLiteral n -> show n
  FloatingPointLiteral sign int fraction exponent -> show (fromRational num :: Double)
    where num = (rationalSign * rationalInt + fraction) * rationalPow
          rationalSign = bool (1 % 1) (-1 % 1) sign
          rationalInt = int % 1
          rationalPow = iterateN (* powerOfTen) (abs exponent) (1 % 1)
          powerOfTen = bool (10 % 1) (1 % 10) (exponent < 0)
          iterateN f n = go n
            where go 0 x = x
                  go n x = go (n-1) (f x)
          
  StringLiteral txt -> "'" ++ concatMap escape (unpack txt) ++ "'"
    where escape = \case
            '\'' -> "''"
            c    -> [c]

printColumnName (ColumnName table name) = maybe "" ((++ ".") . printTableName) table ++ unpack name

printForeignKeyClause = const "foreign key clause"

printTableConstraint = const "table constraint"
