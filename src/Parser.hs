{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Parser where

import Text.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token
import Control.Applicative
import Data.Functor
import GSQL

langDef = emptyDef {
  commentStart = "/*",
  commentEnd = "*/",
  commentLine = "//",
  nestedComments = False,
  identStart = letter <|> char '_',
  identLetter = alphaNum <|> char '_',
  opStart = oneOf "+-*/=<>!@",
  reservedNames = [
    "ACCUM","ALIGNAS","ALIGNOF","AND","AND_EQ","ANY","ASC","ASM","AUTO","AVG",
    "BAG","BETWEEN","BITAND","BITOR","BOOL","BREAK","BY","CASE","CATCH","CHAR",
    "CHAR16_T","CHAR32_T","CLASS","COALESCE","COMPL","COMPRESS","CONCEPT","CONST",
    "CONSTEXPR","CONST_CAST","CONTINUE","COUNT","CREATE","DATETIME","DATETIME_ADD",
    "DATETIME_SUB","DECLTYPE","DEFAULT","DELETE","DESC","DO","DONE","DOUBLE",
    "DYNAMIC_CAST","EDGE","ELSE","END","ENUM","ESCAPE","EXCEPTION","EXPLICIT",
    "EXPORT","EXTERN","FALSE",".FILTER","FLOAT","FOR","FOREACH","FRIEND","FROM",
    "GOTO","GRAPH","GSQL_INT_MAX","GSQL_INT_MIN","GSQL_UINT_MAX","HAVING","IF",
    "IN","INLINE","INSERT","INT","INTERSECT","INTERVAL","INTO","IS","ISEMPTY",
    "JSONARRAY","JSONOBJECT","LIKE","LIMIT","LIST","LOADACCUM","LOG","LONG","MAP",
    "MAX","MIN","MINUS","MUTABLE","NAMESPACE","NEW","NOEXCEPT","NOT","NOT_EQ",
    "NOW","NULL","NULLPTR","OFFSET","OPERATOR","OR","ORDER","OR_EQ","PINNED",
    "PRIMARY_ID","PRINT","PRIVATE","PRODUCT","PROTECTED","PUBLIC","QUERY","RAISE",
    "RANGE","REGISTER","REINTERPRET_CAST","REQUIRES","RETURN","RETURNS","SAMPLE",
    "SELECT","SELECTVERTEX","SET","SHORT","SIGNED","SIZEOF","STATIC","STATIC_ASSERT",
    "STATIC_CAST","STRING","STRUCT","SUM","SWITCH","TARGET","TEMPLATE","THEN","THIS",
    "THREAD_LOCAL","THROW","TO","TO_DATETIME","TRUE","TRY","TUPLE","TYPEDEF",
    "TYPEID","TYPENAME","TYPES","UINT","UNION","UNSIGNED","UPDATE","USER","USING",
    "VALUES","VERTEX","VIRTUAL","VOID","VOLATILE","WCHAR_T","WHEN","WHERE","WHILE",
    "XOR","XOR_EQ"],
  reservedOpNames = [
    "+", "-", "*", "/", "=", "+=", "<", "<=", ">", ">=", "->", "!=", ","
  ],
  caseSensitive = False
}

lexer@TokenParser{
  parens = mParens,
  brackets = mBrackets,
  braces = mBraces,
  reserved = mReserved,
  reservedOp = mReservedOp,
  comma = mComma,
  semi = mSemi,
  identifier = mIdentifier,
  stringLiteral = mStringLiteral
} = makeTokenParser langDef

createQuery = CreateQuery <$>
  (mReserved "create" *> mReserved "query" *> mIdentifier) <*>
  mParens parameterList <*>
  (mReserved "for" *> mReserved "graph" *> mIdentifier) <*>
  optionMaybe (mReserved "returns" *> mParens (Right <$> baseType <|> Left <$> accumType)) <*>
  optionMaybe (mReserved "api" *> mParens mStringLiteral) <*>
  mBraces (typedefs >> declStmts >> declExceptStmt >> queryBodyStmts)

parameterValueList = sepBy parameterValue mComma
parameterValue = parameterConstant 
              <|> ESetBag <$> mBrackets (sepBy parameterValue mComma)
              <|> mParens (EVertex <$> (mStringLiteral <* mComma) <*> mStringLiteral)

parameterConstant =  numeric
                 <|> EString <$> mStringLiteral
                 <|> mReserved "true" $> EBool True
                 <|> mReserved "false" $> EBool False

parameterList = sepBy (parameterType <*> mIdentifier <*> optionMaybe (mReservedOp "=" *> constant)) mComma

typedefs = endBy typedef mSemi
declStmts = endBy declStmt mSemi
declStmt =  baseDeclStmt 
        <|> accumDeclStmt
        <|> fileDeclStmt
declExceptStmt = endBy declExceptStmt mSemi
queryBodyStmts = endBy queryBodyStmt mSemi
queryBodyStmt =  assignStmt
             <|> vSetVarDeclStmt
             <|> gAccumAssignStmt
             <|> gAccumAccumStmt
             <|> funcCallStmt
             <|> selectStmt
             <|> queryBodyCaseStmt
             <|> queryBodyIfStmt
             <|> queryBodyWhileStmt
             <|> queryBodyForEachStmt
             <|> reserved "break"
             <|> reserved "continue"
             <|> updateStmt
             <|> insertStmt
             <|> queryBodyDeleteStmt
             <|> printStmt
             <|> printlnStmt
             <|> logStmt
             <|> returnStmt
             <|> raiseStmt
             <|> tryStmt

querySpecification = mReservedOp "*" $> QSAll
                  <|> mReserved "all" $> QSAll
                  <|> QList <$> sepBy1 mIdentifier mComma
installQuery = InstallQuery <$> (mReserved "install" *> mReserved "query" *> optionMaybe installOptions) <*> querySpecification

runQuery = mReserved "run" *> mReserved "query" *> optionMaybe runOptions <*> mIdentifier <*> mParens parameterValueList

showQuery = mReserved "show" *> mReserved "query" *> mIdentifier
dropQuery = mReserved "drop" *> mReserved "query" *> querySpecification

-- types and names

numeric = EInt <$> integer lexer <|> EReal <$> float lexer

typ =  TTuple <$> mIdentifier
   <|> TAccum <$> accumType
   <|> mReserved "string" *> mReserved "compress" $> TStringCompress
   <|> TBase <$> baseType

baseType =     mReserved "int" $> TInt
        <|> mReserved "uint" $> TUInt
        <|> mReserved "float" $> TFloat
        <|> mReserved "double" $> TDouble
        <|> mReserved "string" $> TString
        <|> mReserved "bool" $> TBool
        <|> mReserved "vertex" *> mReservedOp "<" *> (TVertex <$> mIdentifier) <* mReservedOp ">"

filePath = FPVar <$> mIdentifier <|> FPConst <$> mStringLiteral

typedef = TypeDef <$> (mReserved "typedef" *> mReserved "tuple" *> angles lexer tupleType) <*> mIdentifier
tupleType = commaSep lexer (flip (,) <$> baseType <*> mIdentifier <|> (,) <$> mIdentifier <*> baseType)

parameterType =  PBase <$> baseType
             <|> PSet <$> (mReserved "set" *> angles lexer baseType)
             <|> PBag <$> (mReserved "bag" *> angles lexer baseType)
             <|> PFile <$ mReserved "file"

-- accumulators


accumDecl = const <$> (char '@' *> mIdentifier) <*> optionMaybe (mReservedOp "=" *> constant)
gAccumDecl = const <$> (char '@' *> char '@' *> mIdentifier) <*> optionMaybe (mReservedOp "=" *> constant)
accumDeclStmt = AccumDeclStmt <$>  accumType <*> commaSep1 lexer accumDecl
             <|> flip AccumDeclStmt <$> commaSep1 lexer accumDecl <*> accumType
             <|> GAccumDeclStmt <$> (optionMaybe (mReserved "static") *> accumType) <*> commaSep1 lexer gAccumDecl
             <|> flip GAccumDeclStmt <$> (optionMaybe (mReserved "static") *> commaSep1 lexer gAccumDecl) <*> accumType

accumType =  SumAccum <$> (mReserved "SumAccum" *> angles lexer sumtyp)
        <|> MaxAccum <$> (mReserved "MaxAccum" *> angles lexer minmaxtyp)
        <|> MinAccum <$> (mReserved "MinAccum" *> angles lexer minmaxtyp)
        <|> AvgAccum <$ mReserved "AvgAccum"
        <|> OrAccum <$ mReserved "OrAccum"
        <|> AndAccum <$ mReserved "AndAccum"
        <|> BitwiseOrAccum <$ mReserved "BitwiseOrAccum"
        <|> BitwiseAndAccum <$ mReserved "BitwiseAndAccum"
        <|> ListAccum <$> (mReserved "ListAccum" *> angles lexer typ)
        <|> SetAccum <$> (mReserved "SetAccum" *> angles lexer elementType)
        <|> BagAccum <$> (mReserved "BagAccum" *> angles lexer elementType)
        <|> (mReserved "MapAccum" *> angles lexer (MapAccum <$> elementType <* mComma <*> typ ))

sumtyp = mReserved "int" $> TBase TInt
      <|> mReserved "float" $> TBase TFloat
      <|> mReserved "double" $> TBase TDouble
      <|> mReserved "string" *> mReserved "compress" $> TStringCompress
      <|> mReserved "string" $> TBase TString
minmaxtyp = mReserved "int" $> TBase TInt
      <|> mReserved "float" $> TBase TFloat
      <|> mReserved "double" $> TBase TDouble
elementType =  TTuple <$> mIdentifier
           <|> TStringCompress <$ (mReserved "string" *> mReserved "compress")
           <|> TBase <$> baseType

gAccumAccumStmt = (,) <$> (string "@@" *> mIdentifier <* mReservedOp "+=") <*> expr

-- operators, functions, and expressions

constant =  numeric 
        <|> mStringLiteral
        <|> mReserved "true"
        <|> mReserved "false"
        <|> mReserved "gsql_uint_max"
        <|> mReserved "gsql_int_max"
        <|> mReserved "gsql_int_min"
        <|> mReserved "to_datetime" *> mParens mStringLiteral

