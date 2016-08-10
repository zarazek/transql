{-# LANGUAGE RecursiveDo #-}

module Language.Sql.Parser where

import Language.Sql.Tokenizer
import Language.Sql.Syntax
import Text.Earley
import Control.Applicative ((<|>), many, some, optional)
import Data.Maybe (isJust, fromJust)

leftParenT = token LeftParen
rightParenT = token RightParen
commaT = token Comma
dotT = token Dot
tildaT = token Tilda
minusT = token Minus
plusT = token Plus
doublePipeT = token DoublePipe
starT = token Star
slashT = token Slash
percentT = token Percent
shiftLeftT = token ShiftLeft
shiftRightT = token ShiftRight
ampersandT = token Ampersand
pipeT = token Pipe
lessT = token Less
lessOrEqualT = token LessOrEqual
greaterT = token Greater
greaterOrEqualT = token GreaterOrEqual
equalT = token Equal
doubleEqualT = token DoubleEqual
notEqualT = token NotEqual
differentT = token Different

abortT = token AbortKeyword
actionT = token ActionKeyword
allT = token AllKeyword
andT = token AndKeyword
asT = token AsKeyword
ascT = token AscKeyword
autoincrementT = token AutoincrementKeyword
byT = token ByKeyword
cascadeT = token CascadeKeyword
checkT = token CheckKeyword
collateT = token CollateKeyword
conflictT = token ConflictKeyword
constraintT = token ConstraintKeyword
createT = token CreateKeyword
crossT = token CrossKeyword
currentDateT = token CurrentDateKeyword
currentTimeT = token CurrentTimeKeyword
currentTimestampT = token CurrentTimestampKeyword
defaultT = token DefaultKeyword
deferrableT = token DeferrableKeyword
deferredT = token DeferredKeyword
deleteT = token DeleteKeyword
descT = token DescKeyword
distinctT = token DistinctKeyword
exceptT = token ExceptKeyword
existsT = token ExistsKeyword
failT = token FailKeyword
falseT = token FalseKeyword
foreignT = token ForeignKeyword
fromT = token FromKeyword
globT = token GlobKeyword
groupT = token GroupKeyword
havingT = token HavingKeyword
ifT = token IfKeyword
immediateT = token ImmediateKeyword
inT = token InKeyword
initiallyT = token InitiallyKeyword
insertT = token InsertKeyword
intersectT = token IntersectKeyword
intoT = token IntoKeyword
isT = token IsKeyword
ignoreT = token IgnoreKeyword
innerT = token InnerKeyword
joinT = token JoinKeyword
keyT = token KeyKeyword
leftT = token LeftKeyword
likeT = token LikeKeyword
matchT = token MatchKeyword
naturalT = token NaturalKeyword
noT = token NoKeyword
notT = token NotKeyword
nullT = token NullKeyword
onT = token OnKeyword
orT = token OrKeyword
outerT = token OuterKeyword
primaryT = token PrimaryKeyword
referencesT = token ReferencesKeyword
regexpT = token RegexpKeyword
replaceT = token ReplaceKeyword
restrictT = token RestrictKeyword
rollbackT = token RollbackKeyword
selectT = token SelectKeyword
setT = token SetKeyword
tableT = token TableKeyword
tempT = token TempKeyword
temporaryT = token TemporaryKeyword
trueT = token TrueKeyword
unionT = token UnionKeyword
uniqueT = token UniqueKeyword
updateT = token UpdateKeyword
usingT = token UsingKeyword
valuesT = token ValuesKeyword
whereT = token WhereKeyword

identifierT = terminal extractIdentifier
  where extractIdentifier t = case t of
                                Identifier x -> Just x
                                _            -> Nothing

stringT = terminal extractString
  where extractString t = case t of
                            SqlString s -> Just s
                            _           -> Nothing

unsignedT = terminal extractNumber
  where extractNumber t = case t of
                            UnsignedDecimal x -> Just x
                            _                 -> Nothing

integerT = terminal extractNumber
  where extractNumber t = case t of
                            UnsignedDecimal x -> Just x
                            SignedDecimal x   -> Just x
                            UnsignedHex x     -> Just x
                            _                 -> Nothing


withDefault a p = p <|> pure a
optionalFlag flagP = withDefault False (flagP *> pure True)
entity p a = p *> pure a
nonEmptyListWith separator element = (:) <$> element <*> many (separator *> element)
nonEmptyList = nonEmptyListWith commaT
parenthesizedList element = leftParenT *> nonEmptyList element <* rightParenT  
after f g x y = f (g x y)

tableName = TableName <$> optional (identifierT <* dotT) <*> identifierT

columnName = ColumnName <$> optional (tableName <* dotT) <*> identifierT 

literal = entity nullT Null <|>
          entity trueT TrueL <|>
          entity falseT FalseL <|>
          entity currentTimeT CurrentTime <|>
          entity currentDateT CurrentDate <|>
          entity currentTimestampT CurrentTimestamp <|>
          IntegerLiteral <$> integerT <|>
          floatingPointLiteral <|>
          StringLiteral <$> stringT
  where floatingPointLiteral = terminal extractFloatingPoint
        extractFloatingPoint t = case t of
                                   FloatingPoint s i f e -> Just (FloatingPointLiteral s i f e)
                                   _                     -> Nothing

expression = mdo
  l0 <- rule $ LiteralExpr <$> literal <|>
               ColumnExpr <$> columnName <|>
               leftParenT *> lFinal <* rightParenT
  l1 <- rule $ tildaT *> (BitwiseNotExpr <$> l0) <|>
               l0
  l2 <- rule $ CollateExpr <$> l1 <* collateT <*> identifierT <|>
               l1
  l3 <- rule $ minusT *> (NegateExpr <$> l2) <|>
               plusT *> l2 <|>
               l2
  l4 <- rule $ ConcatenateExpr <$> l3 <* doublePipeT <*> l3 <|>
               l3
  l5 <- rule $ MultiplyExpr <$> l4 <* starT <*> l4 <|>
               DivideExpr <$> l4 <* slashT <*> l4 <|>
               ModuloExpr <$> l4 <* percentT <*> l4 <|>
               l4
  l6 <- rule $ AddExpr <$> l5 <* plusT <*> l5 <|>
               SubtractExpr <$> l5 <* minusT <*> l5 <|>
               l5
  l7 <- rule $ ShiftLeftExpr <$> l6 <* shiftLeftT <*> l6 <|>
               ShiftRightExpr <$> l6 <* shiftRightT <*> l6 <|>
               BitwiseAndExpr <$> l6 <* ampersandT <*> l6 <|>
               BitwiseOrExpr <$> l6 <* pipeT <*> l6 <|>
               l6
  l8 <- rule $ LessExpr <$> l7 <* lessT <*> l7 <|>
               LessOrEqualExpr <$> l7 <* lessOrEqualT <*> l7 <|>
               GreaterExpr <$> l7 <* greaterT <*> l7 <|>
               GreaterOrEqualExpr <$> l7 <* greaterOrEqualT <*> l7 <|>
               l7
  l9 <- rule $ EqualExpr <$> l8 <* equalT <*> l8 <|>
               EqualExpr <$> l8 <* doubleEqualT <*> l8 <|>
               (LogicalNotExpr `after` EqualExpr) <$> l8 <* notEqualT <*> l8 <|>
               (LogicalNotExpr `after` EqualExpr) <$> l8 <* differentT <*> l8 <|>
               IsExpr <$> l8 <* isT <*> l8 <|>
               (LogicalNotExpr `after` IsExpr) <$> l8 <* isT <* notT <*> l8 <|>
               InExpr <$> l8 <* inT <*> l8 <|>
               (LogicalNotExpr `after` IsExpr) <$> l8 <* notT <* inT <*> l8 <|>
               LikeExpr <$> l8 <* likeT <*> l8 <|>
               GlobExpr <$> l8 <* globT <*> l8 <|>
               MatchExpr <$> l8 <* matchT <*> l8 <|>
               RegexpExpr <$> l8 <* regexpT <*> l8 <|>
               l8
  l10 <- rule $ notT *> (LogicalNotExpr <$> l9) <|>
                l9
  l11 <- rule $ LogicalAndExpr <$> l10 <* andT <*> l10 <|>
                l10
  l12 <- rule $ LogicalOrExpr <$> l11 <* orT <*> l11 <|>
                l11
  let lFinal = l11
  return lFinal

precision = leftParenT *> actualPrecision <* rightParenT <|> pure PrecisionNone
  where actualPrecision = precisionAndLength <|> length
        precisionAndLength = PrecisionLengthAndPrecision <$> unsignedT <* commaT <*> unsignedT
        length = PrecisionLength <$> unsignedT

typeName = TypeName <$> some identifierT <*> precision

sortingType = entity ascT SortingTypeAscending <|>
              entity descT SortingTypeDescending <|>
              pure SortingTypeNone

conflictClause = onT *> conflictT *> (entity rollbackT ConflictClauseRollback <|>
                                      entity abortT ConflictClauseAbort <|>
                                      entity failT ConflictClauseFail <|>
                                      entity ignoreT ConflictClauseIgnore <|>
                                      entity replaceT ConflictClauseReplace) <|>
                 pure ConflictClauseNone

autoincrement = optionalFlag autoincrementT

foreignKeyClause = referencesT *> (ForeignKeyClause <$> identifierT <*> columnList <*> actionList <*> deferrable)
  where columnList = parenthesizedList identifierT <|>
                     pure []
        actionList = many (onT *> action)
        action = ForeignKeyAction <$> actionTrigger <*> actionType
        actionTrigger = entity deleteT ActionTriggerDelete <|>
                        entity updateT ActionTriggerUpdate
        actionType = entity (sequenceA [setT, nullT]) ActionSetNull <|>
                     entity (sequenceA [setT, defaultT]) ActionSetDefault <|>
                     entity cascadeT ActionCascade <|>
                     entity restrictT ActionRestrict <|>
                     entity (sequenceA [noT, actionT]) ActionNoAction
        deferrable = optional (Deferrable <$> currentlyDeferrable <*> optional initiallyDeferrable)
        currentlyDeferrable = withDefault True (notT *> pure False) <* deferrableT
        initiallyDeferrable = initiallyT *> (entity deferredT True <|> entity immediateT False)

columnConstraintType = do
  expr <- expression
  let primaryKeyC = primaryT *> keyT *> (CCTPrimaryKey <$> sortingType <*> conflictClause <*> autoincrement)
  let notNullC = notT *> nullT *> (CCTNotNull <$> conflictClause)
  let uniqueC = uniqueT *> (CCTUnique <$> conflictClause)
  let checkC = checkT *> leftParenT *> (CCTCheck <$> expr) <* rightParenT
  let literalOrExpr = LiteralExpr <$> literal <|> leftParenT *> expr <* rightParenT
  let defaultC = defaultT *> (CCTDefault <$> literalOrExpr)
  let collateC = collateT *> (CCTCollate <$> identifierT)
  let foreignKeyC = CCTForeignKey <$> foreignKeyClause
  return (primaryKeyC <|> notNullC <|> uniqueC <|> checkC <|> defaultC <|> collateC <|> foreignKeyC)

columnConstraint = do
  cct <- columnConstraintType
  return (ColumnConstraint <$> optional (constraintT *> identifierT) <*> cct)

columnDefinition = do
  cc <- columnConstraint
  return (ColumnDefinition <$> identifierT <*> optional typeName <*> many cc)

tableConstraintType = do
  expr <- expression
  let columnList = parenthesizedList identifierT
  let primaryKeyC = primaryT *> keyT *> (TCTPrimaryKey <$> columnList <*> conflictClause)
  let uniqueC = uniqueT *> (TCTUnique <$> columnList <*> conflictClause)
  let checkC = checkT *> leftParenT *> (TCTCheck <$> expr) <* rightParenT
  let foreignKeyC = foreignT *> keyT *> (TCTForeignKey <$> columnList <*> foreignKeyClause)
  return (primaryKeyC <|> uniqueC <|> checkC <|> foreignKeyC)

tableConstraint = do
  tct <- tableConstraintType
  return (TableConstraint <$> optional (constraintT *> identifierT) <*> tct)

-- TODO: create from select
createTable = do
 cd <- columnDefinition
 tc <- tableConstraint
 return (createT *> 
         (CreateTable <$>
          optionalFlag (tempT <|> temporaryT) <*
          tableT <*>
          optionalFlag (sequenceA [ifT, notT, existsT]) <*>
          tableName <*
          leftParenT <*>
          nonEmptyList cd <*>
          many (commaT *> tc) <*
          rightParenT))

-- TODO: with clause
insert = do
  expr <- expression 
  let insertType = entity insertT InsertTypeInsert <|>
                   entity replaceT InsertTypeReplace <|>
                   entity (sequenceA [insertT, orT, replaceT]) InsertTypeInsertOrReplace <|>
                   entity (sequenceA [insertT, orT, rollbackT]) InsertTypeInsertOrRollback <|>
                   entity (sequenceA [insertT, orT, abortT]) InsertTypeInsertOrAbort <|>
                   entity (sequenceA [insertT, orT, failT]) InsertTypeInsertOrFail <|>
                   entity (sequenceA [insertT, orT, ignoreT]) InsertTypeInsertOrIgnore
  let columnList = parenthesizedList identifierT <|> pure []
  let row = parenthesizedList expr
  let rowList = nonEmptyList row
  selectStatement <- select
  let values = valuesT *> (VRowList <$> rowList) <|>
               VSelect <$> selectStatement <|>
               entity (sequenceA [defaultT, valuesT]) VDefaults
                
  return (Insert <$> insertType <* intoT <*> tableName <*> columnList <*> values)

-- TODO: better JOIN support
-- TODO: values
-- TODO: order by
-- TODO: limit
-- TODO: with clause
select = mdo
  selectStatement <- rule $ SSUnionAll <$> simpleSelect <* unionT <* allT <*> selectStatement <|>
                            SSUnion <$> simpleSelect <* unionT <*> selectStatement <|>
                            SSIntersect <$> simpleSelect <* intersectT <*> selectStatement <|>
                            SSExcept <$> simpleSelect <* exceptT <*> selectStatement <|>
                            simpleSelect
  simpleSelect <- rule $ selectT *> (SSSelect <$> selectFlag <*> columnList <*> fromClause <*> whereClause <*> groupByClause)
  selectFlag <- rule $ entity distinctT SFDistinct <|>
                       entity allT SFAll <|>
                       pure SFNone
  columnList <- rule $ nonEmptyList resultColumn
  resultColumn <- rule $ RCExpr <$> expr <*> optional (asT *> identifierT) <|>
                         RCAll <$> optional (tableName <* dotT) <* starT
  expr <- expression 
  fromClause <- rule $ optional (fromT *> joinClause)
  joinClause <- rule $ JCJoin <$> simpleTable <*> joinOperator <*> joinClause <*> joinConstraint <|>
                       JCSimple <$> simpleTable
  simpleTable <- rule $ AliasedJoinClauseElement <$> simpleTableSpec <*> optional (optional asT *> identifierT)
  simpleTableSpec <- rule $ JCETable <$> tableName <|>
                            leftParenT *> (JCESubquery <$> selectStatement)  <* rightParenT
  joinOperator <- rule $ entity commaT JOComma <|>
                         JOExplicitJoin <$> explicitJoin
  explicitJoin <- rule $ PrefixedJoinType <$> optionalFlag naturalT <*> joinType <* joinT
  joinType <- rule $ entity (sequenceA [leftT, outerT]) JTLeftOuter <|>
                     entity leftT JTLeft <|>
                     entity innerT JTInner <|>
                     entity crossT JTCross <|>
                     pure JTDefault
  joinConstraint <- rule $ optional (onT *> (JCOn <$> expr) <|>
                                     usingT *> (JCUsing <$> parenthesizedList identifierT))
  whereClause <- rule $ optional (whereT *> expr)
  groupByClause <- rule $ optional (groupT *> byT *> (GroupBy <$> nonEmptyList expr <*> optional (havingT *> expr)))
  return selectStatement
 
sqlStatement = do
  ct <- createTable
  i <- insert
  s <- select
  return (ct <|> i <|> Select <$> s)

parse :: [Token] -> ([SqlStatement], Report String [Token])
parse tokens = fullParses sqlStatementParser tokens
  where sqlStatementParser = parser sqlStatement
