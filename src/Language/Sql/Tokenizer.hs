{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleContexts #-}

module Language.Sql.Tokenizer (Token(..), significant, tokenize) where

import Prelude hiding (null)
import Data.Attoparsec.Text (Parser, char, inClass, satisfy, digit,
                             decimal, hexadecimal, signed, 
                             string, takeWhile1,
                             parse, IResult(..))
import Control.Applicative (Alternative, (<|>), many, some)
import Data.Foldable (asum)
import Data.List (foldl', intersperse)
import Data.Char (toUpper, isAlpha, isAlphaNum, isSpace, digitToInt)
import Data.Text (Text, pack, unpack, empty, null)
import Data.Bool (bool)
import Data.Ratio ((%))
import qualified Data.ListTrie.Patricia.Map as ST
import Data.ListTrie.Base.Map (WrappedIntMap)

data Token = DoublePipe
           | ShiftLeft
           | ShiftRight
           | DoubleEqual
           | NotEqual
           | Different
           | LessOrEqual
           | GreaterOrEqual
           | LeftParen
           | RightParen
           | Comma
           | Dot
           | Equal
           | Tilda
           | Minus
           | Plus
           | Star
           | Slash
           | Percent
           | Ampersand
           | Pipe
           | Less
           | Greater
           | AbortKeyword
           | ActionKeyword
           | AllKeyword
           | AndKeyword
           | AsKeyword
           | AscKeyword
           | AutoincrementKeyword
           | ByKeyword
           | CascadeKeyword
           | CheckKeyword
           | CollateKeyword
           | ConflictKeyword
           | ConstraintKeyword
           | CreateKeyword
           | CrossKeyword
           | CurrentDateKeyword
           | CurrentTimeKeyword
           | CurrentTimestampKeyword
           | DefaultKeyword
           | DeferrableKeyword
           | DeferredKeyword
           | DeleteKeyword
           | DescKeyword
           | DistinctKeyword
           | ExceptKeyword
           | ExistsKeyword
           | FailKeyword
           | FalseKeyword
           | ForeignKeyword
           | FromKeyword
           | GlobKeyword
           | GroupKeyword
           | HavingKeyword
           | IfKeyword
           | IgnoreKeyword
           | ImmediateKeyword
           | InKeyword
           | InitiallyKeyword
           | InnerKeyword
           | InsertKeyword
           | IntersectKeyword
           | IntoKeyword
           | IsKeyword
           | JoinKeyword
           | KeyKeyword
           | LeftKeyword
           | LikeKeyword
           | MatchKeyword
           | NaturalKeyword
           | NoKeyword
           | NotKeyword
           | NullKeyword
           | OnKeyword
           | OrKeyword
           | OuterKeyword
           | PrimaryKeyword
           | ReferencesKeyword
           | RegexpKeyword
           | ReplaceKeyword
           | RestrictKeyword
           | RollbackKeyword
           | SelectKeyword
           | SetKeyword
           | TableKeyword
           | TempKeyword
           | TemporaryKeyword
           | TrueKeyword
           | UnionKeyword
           | UniqueKeyword
           | UpdateKeyword
           | UsingKeyword
           | ValuesKeyword
           | WhereKeyword
           | Identifier Text
           | SqlString Text
           | UnsignedDecimal Integer
           | SignedDecimal Integer
           | FloatingPoint Bool Integer Rational Integer
           | UnsignedHex Integer
           | Whitespace Text
           | LineComment Text
           | BlockComment Text
  deriving (Eq, Show)

significant :: Token -> Bool
significant t = case t of
                  Whitespace _   -> False
                  LineComment _  -> False
                  BlockComment _ -> False
                  _              -> True

sqlSymbol :: Parser Token
sqlSymbol = asum (map parseSymbol symbolList)
  where parseSymbol (str, token) = string str *> pure token
        symbolList = [ ("||", DoublePipe    ),
                       ("<<", ShiftLeft     ),
                       (">>", ShiftRight    ),
                       ("==", DoubleEqual   ),
                       ("!=", NotEqual      ),
                       ("<>", Different     ),
                       ("<=", LessOrEqual   ),
                       (">=", GreaterOrEqual),
                       ("(",  LeftParen     ),
                       (")",  RightParen    ),
                       (",",  Comma         ),
                       (".",  Dot           ),
                       ("=",  Equal         ),
                       ("~",  Tilda         ),
                       ("-",  Minus         ),
                       ("+",  Plus          ),
                       ("*",  Star          ),
                       ("/",  Slash         ),
                       ("%",  Percent       ),
                       ("&",  Ampersand     ),
                       ("|",  Pipe          ),
                       ("<",  Less          ),
                       (">",  Greater       ) ]

sqlIdentifier :: Parser Token
sqlIdentifier = checkForKeyword <$> ((:) <$> identifierStarter <*> many identifierFollower)
  where identifierStarter = satisfy isAlpha <|> underscore
        identifierFollower = satisfy isAlphaNum <|> underscore
        underscore = char '_'
        checkForKeyword str = case ST.lookup (map toUpper str) keywordMap of
                                Just keyword -> keyword
                                Nothing      -> Identifier (pack str)
        keywordMap :: ST.TrieMap WrappedIntMap Char Token
        keywordMap = foldl' insertKeyword ST.empty keywordList
        insertKeyword m (keyword, token) = ST.insert keyword token m
        keywordList = [ ("ABORT",             AbortKeyword            ),
                        ("ACTION",            ActionKeyword           ),
                        ("ALL",               AllKeyword              ),
                        ("AND",               AndKeyword              ),
                        ("AS",                AsKeyword               ),
                        ("ASC",               AscKeyword              ),
                        ("AUTOINCREMENT",     AutoincrementKeyword    ),
                        ("BY",                ByKeyword               ),
                        ("CASCADE",           CascadeKeyword          ),
                        ("CHECK",             CheckKeyword            ),
                        ("COLLATE",           CollateKeyword          ),
                        ("CONFLICT",          ConflictKeyword         ),
                        ("CONSTRAINT",        ConstraintKeyword       ),
                        ("CREATE",            CreateKeyword           ),
                        ("CROSS",             CrossKeyword            ),
                        ("CURRENT_DATE",      CurrentDateKeyword      ),
                        ("CURRENT_TIME",      CurrentTimeKeyword      ),
                        ("CURRENT_TIMESTAMP", CurrentTimestampKeyword ),
                        ("DEFAULT",           DefaultKeyword          ),
                        ("DEFERRABLE",        DeferrableKeyword       ),
                        ("DEFERRED",          DeferredKeyword         ),
                        ("DELETE",            DeleteKeyword           ),
                        ("DESC",              DescKeyword             ),
                        ("DISTINCT",          DistinctKeyword         ),
                        ("EXCEPT",            ExceptKeyword           ),
                        ("EXISTS",            ExistsKeyword           ),
                        ("FAIL",              FailKeyword             ),
                        ("FALSE",             FalseKeyword            ),
                        ("FOREIGN",           ForeignKeyword          ),
                        ("FROM",              FromKeyword             ),
                        ("GLOB",              GlobKeyword             ),
                        ("GROUP",             GroupKeyword            ),
                        ("HAVING",            HavingKeyword           ),
                        ("IF",                IfKeyword               ),
                        ("IGNORE",            IgnoreKeyword           ),
                        ("IMMEDIATE",         ImmediateKeyword        ),
                        ("IN",                InKeyword               ),
                        ("INITIALLY",         InitiallyKeyword        ),
                        ("INNER",             InnerKeyword            ),
                        ("INSERT",            InsertKeyword           ),
                        ("INTERSECT",         IntersectKeyword        ),
                        ("INTO",              IntoKeyword             ),
                        ("IS",                IsKeyword               ),
                        ("JOIN",              JoinKeyword             ),
                        ("KEY",               KeyKeyword              ),
                        ("LEFT",              LeftKeyword             ),
                        ("LIKE",              LikeKeyword             ),
                        ("MATCH",             MatchKeyword            ),
                        ("NATURAL",           NaturalKeyword          ),
                        ("NO",                NoKeyword               ),
                        ("NOT",               NotKeyword              ),
                        ("NULL",              NullKeyword             ),
                        ("ON",                OnKeyword               ),
                        ("OR",                OrKeyword               ),
                        ("OUTER",             OuterKeyword            ),
                        ("PRIMARY",           PrimaryKeyword          ),
                        ("REFERENCES",        ReferencesKeyword       ),
                        ("REGEXP",            RegexpKeyword           ),
                        ("REPLACE",           ReplaceKeyword          ),
                        ("RESTRICT",          RestrictKeyword         ),
                        ("ROLLBACK",          RollbackKeyword         ),
                        ("SELECT",            SelectKeyword           ),
                        ("SET",               SetKeyword              ),
                        ("TABLE",             TableKeyword            ),
                        ("TEMP",              TempKeyword             ),
                        ("TEMPORARY",         TemporaryKeyword        ),
                        ("TRUE",              TrueKeyword             ),
                        ("UNION",             UnionKeyword            ),
                        ("UNIQUE",            UniqueKeyword           ),
                        ("UPDATE",            UpdateKeyword           ),
                        ("USING",             UsingKeyword            ),
                        ("VALUES",            ValuesKeyword           ),
                        ("WHERE",             WhereKeyword            ) ]

sqlString :: Parser Token
sqlString = (SqlString . pack) <$> (char '\'' *> many sqlStringChar <* char '\'')
  where sqlStringChar = normalChar <|> doubleQuotes
        normalChar = satisfy (/= '\'')
        doubleQuotes = string "''" *> pure '\''

withDefault :: Alternative f => a -> f a -> f a
withDefault x a = a <|> pure x

sqlUnsignedDecimal :: Parser Token
sqlUnsignedDecimal = UnsignedDecimal <$> decimal

sqlSignedDecimal :: Parser Token
sqlSignedDecimal = SignedDecimal <$> signed decimal

sqlFloatingPoint :: Parser Token
sqlFloatingPoint = mkFloatingPoint <$> sign <*> floatingPointPart <*> exponentPart
  where mkFloatingPoint s (i, f) e = FloatingPoint s i f e
        sign = withDefault False (isNegative <$> satisfy (inClass "+-"))
        isNegative c = case c of
                        '+' -> False
                        '-' -> True
        floatingPointPart = startsWithDigit <|> startsWithDot
        startsWithDigit = (,) <$> decimal <*> withDefault 0 (dot *> manyFractionalDigits)
        startsWithDot = (0,) <$> (dot *> someFractionalDigits)
        exponentPart = withDefault 0 (satisfy (inClass "Ee") *> signed decimal)
        manyFractionalDigits = stringToFraction <$> many digit
        someFractionalDigits = stringToFraction <$> some digit
        dot = char '.'
        stringToFraction = uncurry (%) . foldl' accum (0,1)
        accum (num, denom) c = (10*num + toInteger (digitToInt c), 10*denom)

sqlHexadecimal :: Parser Token
sqlHexadecimal = UnsignedHex <$> (char '0' *> satisfy (inClass "xX") *> hexadecimal)

sqlNumber :: Parser Token
sqlNumber = sqlUnsignedDecimal <|> sqlSignedDecimal <|> sqlFloatingPoint <|> sqlHexadecimal

sqlWhitespace :: Parser Token
sqlWhitespace = Whitespace <$> takeWhile1 isSpace

sqlTokens :: Parser [Token]
sqlTokens = many (sqlSymbol <|> sqlIdentifier <|> sqlString <|> sqlNumber <|> sqlWhitespace)

checkedParse :: Parser a -> Text -> Either String a
checkedParse p str = categorize (parse p str) where
  categorize res = case res of
                     Fail _ ctxs err -> Left (concat (intersperse ", " ctxs) ++ ": " ++ err)
                     Partial f -> categorize (f empty)
                     Done leftover r -> case null leftover of
                                          True -> Right r
                                          False -> Left ("garbage at end: '" ++ unpack leftover ++ "'")

tokenize :: Text -> Either String [Token]
tokenize txt = checkedParse sqlTokens txt
