{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module GSQL where

import Control.Applicative
import Data.Functor

data AccumTyp = SumAccum Typ | MaxAccum Typ | MinAccum Typ
              | AvgAccum 
              | BitwiseOrAccum | OrAccum | AndAccum | BitwiseAndAccum 
              | ListAccum Typ | SetAccum Typ | BagAccum Typ
              | MapAccum Typ Typ 
              | HeapAccum String | GroupByAccum [(String, Typ)]
              | ArrayAccum String
              deriving (Eq, Show)

data CreateQuery = CreateQuery{
  queryName :: String,
  pList :: [(String, Typ)],
  associatedGraph :: String,
  returnTyp :: Maybe (Either BaseTyp AccumTyp),
  endPoint :: Maybe String,
  queryBody :: [String]
} deriving (Eq, Show)

data Expr = ExpGAccum String
          | ExpTyp String
          | ExpAttr String String
          | ExpAccum String Sring
          | 

data Exp = EInt Integer | EReal Double | EString String | EBool Bool | ESetBag [Exp] | EVertex String String
         deriving (Eq, Show)

data QuerySpecification = QSAll | QList [String] deriving (Eq, Show)
data InstallQuery = InstallQuery [String] QuerySpecification
                  deriving(Eq, Show)

data Typ = TBase BaseTyp | TTuple String | TAccum AccumTyp | TStringCompress deriving (Eq, Show)
data BaseTyp = TInt | TUInt | TFloat | TDouble | TString | TBool | TVertex String
             | TEdge | TJsonObject | TJsonArray | TDatetime
             deriving (Eq, Show)
data FilePath = FPVar String | FPConst String deriving (Eq, Show)

data TypeDef = TypeDef [(String, BaseTyp)] String deriving (Eq, Show)
data ParameterTyp = PBase BaseTyp | PSet BaseTyp | PBag BaseTyp | PFile deriving (Eq, Show)

-- accumulators

data AccumDeclStmt = AccumDeclStmt AccumTyp [String] 
                   | GAccumDeclStmt AccumTyp [String]
                   deriving (Eq, Show)

