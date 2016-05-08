module Language.Sql.Syntax where

import Data.Text

type Name = Text

data TableName = TableName { tnSchema :: Maybe Name
                           , tnTable :: Name }
  deriving (Show)

data ColumnName = ColumnName { cnTable :: Maybe TableName
                             , cnColumn :: Name }
  deriving (Show)

data Literal = Null
             | FalseL
             | TrueL
             | CurrentTime
             | CurrentDate
             | CurrentTimestamp
             | IntegerLiteral Integer
             | FloatingPointLiteral Bool Integer Rational Integer
             | StringLiteral Text
  deriving (Show)
           

data Precision = PrecisionNone
               | PrecisionLength Integer
               | PrecisionLengthAndPrecision Integer Integer
  deriving (Show)

data TypeName = TypeName { tnName :: [Name]
                         , tnPrecision :: Precision }
  deriving (Show)

data SortingType = SortingTypeNone
                 | SortingTypeAscending
                 | SortingTypeDescending
  deriving (Show)

data ConflictClause = ConflictClauseNone
                    | ConflictClauseRollback
                    | ConflictClauseAbort
                    | ConflictClauseFail
                    | ConflictClauseIgnore
                    | ConflictClauseReplace
  deriving (Show)

data ForeignKeyActionTrigger = ActionTriggerDelete
                             | ActionTriggerUpdate
  deriving (Show)

data ForeignKeyActionType = ActionSetNull
                          | ActionSetDefault
                          | ActionCascade
                          | ActionRestrict
                          | ActionNoAction
  deriving (Show)

-- TODO: dodać MATCH
data ForeignKeyAction = ForeignKeyAction ForeignKeyActionTrigger ForeignKeyActionType
  deriving (Show)

data Deferrable = Deferrable Bool (Maybe Bool)
  deriving (Show)

-- data PrimaryKeySpecification = PrimaryKeySpecification { pksColumns :: [Name]
--                                                        , pksConflictClause :: ConflictClause
--                                                        , pksSortingType :: SortingType
--                                                        , pksAutoIncrement :: Bool }
--   deriving (Show)

-- data UniqueSpecification = UniqueSpecification { usColumns :: [Name]
--                                                , usConflictClause :: ConflictClause }
--   deriving (Show)

data ForeignKeyClause = ForeignKeyClause { fkcForeignTable :: Name
                                         , fkcForeignColumns :: [Name]
                                         , fkcActions :: [ForeignKeyAction]
                                         , fkcDeferrable :: Maybe Deferrable }
  deriving (Show)

-- data ForeignKeySpecification = ForeignKeySpecification { fksLocalColumns :: [Name]
--                                                        , fksClause :: ForeignKeyClause }
--   deriving (Show)

data ColumnConstraintType = CCTPrimaryKey { cpkSortingType :: SortingType
                                          , cpkConflictClause :: ConflictClause
                                          , cpkAutoincrement :: Bool }
                          | CCTNotNull ConflictClause
                          | CCTUnique ConflictClause
                          | CCTCheck Expr
                          | CCTDefault Expr
                          | CCTCollate Name
                          | CCTForeignKey ForeignKeyClause
                                            
  deriving (Show)

data ColumnConstraint = ColumnConstraint { ccName :: Maybe Name
                                         , ccType :: ColumnConstraintType }
  deriving (Show)

data ColumnDefinition = ColumnDefinition { cdName :: Name
                                         , cdType :: Maybe TypeName
                                         , cdConstraints :: [ ColumnConstraint ] }
  deriving (Show)

data Expr = LiteralExpr Literal
          | ColumnExpr ColumnName
          | NegateExpr Expr
          | BitwiseNotExpr Expr
          | LogicalNotExpr Expr
          | ConcatenateExpr Expr Expr
          | MultiplyExpr Expr Expr
          | DivideExpr Expr Expr
          | ModuloExpr Expr Expr
          | AddExpr Expr Expr
          | SubtractExpr Expr Expr
          | ShiftLeftExpr Expr Expr
          | ShiftRightExpr Expr Expr
          | BitwiseAndExpr Expr Expr
          | BitwiseOrExpr Expr Expr
          | LessExpr Expr Expr
          | LessOrEqualExpr Expr Expr
          | GreaterExpr Expr Expr
          | GreaterOrEqualExpr Expr Expr
          | EqualExpr Expr Expr
          | IsExpr Expr Expr
          | InExpr Expr Expr
          | LikeExpr Expr Expr
          | GlobExpr Expr Expr
          | MatchExpr Expr Expr
          | RegexpExpr Expr Expr
          | LogicalAndExpr Expr Expr
          | LogicalOrExpr Expr Expr
          | CollateExpr Expr Name
  deriving (Show)

data TransactionType = TransactionTypeDefault
                     | TransactionTypeDeferred
                     | TransactionTypeExclusive

data TableConstraintType = TCTPrimaryKey { tpkColumns :: [Name]
                                         , tpkConflictClause :: ConflictClause }
                         | TCTUnique { tuColumns :: [Name]
                                     , tuConflictClause :: ConflictClause }
                         | TCTCheck Expr
                         | TCTForeignKey { tfkLocalColumns :: [Name]
                                         , tfkForeignKeyClause :: ForeignKeyClause }
  deriving (Show)

data TableConstraint = TableConstraint { tcName :: Maybe Name
                                       , tcConstraint :: TableConstraintType }
  deriving (Show)

data InsertType = InsertTypeInsert
                | InsertTypeReplace
                | InsertTypeInsertOrReplace
                | InsertTypeInsertOrRollback
                | InsertTypeInsertOrAbort
                | InsertTypeInsertOrFail
                | InsertTypeInsertOrIgnore
  deriving (Show)

data Values = VRowList [[Expr]]
            | VSelect SelectStatement
            | VDefaults
  deriving (Show)

data SelectFlag = SFDistinct
                | SFAll
                | SFNone
  deriving (Show)

data ResultColumn = RCExpr { rceExpression :: Expr
                           , rceAlias :: Maybe Name }
                  | RCAll (Maybe TableName)
  deriving (Show)

data JoinClauseElement = JCETable TableName
                       | JCESubquery SelectStatement
  deriving (Show)

data AliasedJoinClauseElement = AliasedJoinClauseElement { ajceElement :: JoinClauseElement
                                                          , ajceAlias :: Maybe Name }
  deriving (Show)

data JoinOperator = JOComma
                  | JOExplicitJoin PrefixedJoinType
  deriving (Show)

data PrefixedJoinType = PrefixedJoinType { pjtNatural :: Bool
                                         , pjtType :: JoinType }
  deriving (Show)

data JoinType = JTLeftOuter
              | JTLeft
              | JTInner
              | JTCross
              | JTDefault
  deriving (Show)

data JoinConstraint = JCOn Expr
                    | JCUsing [Name]
  deriving (Show)

data JoinClause = JCSimple AliasedJoinClauseElement
                | JCJoin { jcjHead :: AliasedJoinClauseElement
                         , jcjOperator :: JoinOperator
                         , jcjTail :: JoinClause
                         , jcjConstraint :: Maybe JoinConstraint }
  deriving (Show)

data GroupBy = GroupBy { gbGroupBy :: [Expr]
                       , gbHaving :: Maybe Expr }
  deriving (Show)

data SelectStatement = SSSelect { ssFlag :: SelectFlag
                                , ssResultColumns :: [ResultColumn]
                                , ssFrom :: Maybe JoinClause
                                , ssWhere :: Maybe Expr
                                , ssGroupBy :: Maybe GroupBy }
                     | SSUnionAll SelectStatement SelectStatement
                     | SSUnion SelectStatement SelectStatement
                     | SSIntersect SelectStatement SelectStatement
                     | SSExcept SelectStatement SelectStatement
  deriving (Show)

data SqlStatement = CreateTable { ctTemporary :: Bool
                                , ctIfNotExists :: Bool
                                , ctName :: TableName
                                , ctColumns :: [ColumnDefinition]
                                , ctTableConstraints :: [TableConstraint] }
                  | Insert { iType :: InsertType
                           , iTable :: TableName
                           , iCollumns :: [Name]
                           , iValues :: Values }
                  | Select SelectStatement
  deriving (Show)
