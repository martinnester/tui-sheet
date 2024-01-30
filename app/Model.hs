{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Model where

import Control.Applicative (Alternative (empty), (<|>))
import Control.Monad (foldM, join, liftM2)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char (isDigit, isUpper, ord)
import Data.Fixed (mod')
import Data.Foldable (foldlM, foldrM)
import Data.Functor.Identity (Identity)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Parsec (ParseError, ParsecT, SourcePos, alphaNum, eof, getPosition, letter, lower, many, many1, oneOf, optionMaybe, optional, parse, satisfy, string, try)
import Text.Parsec.Expr
  ( Assoc (AssocLeft, AssocRight),
    Operator (Infix, Prefix),
    buildExpressionParser,
  )
import Text.Parsec.Language (emptyDef, javaStyle)
import Text.Parsec.Pos (newPos)
import Text.Parsec.Token
import Text.ParserCombinators.Parsec (SourcePos)

data LiteralValue = Float Float | Int Int | Bool Bool | List [LiteralValue] | Null | String String

type Address = (Int, Int)

addressAsString (col, row) = numToLetters col ++ show row

numToLetters num = reverse $ inReverse num
  where
    inReverse (-1) = []
    inReverse num =
      let (q, r) = num `divMod` 26
       in (['A' .. 'Z'] !! r) : inReverse (q - 1)

lettersToNum letters = sum (zipWith (\l i -> (26 ^ i) * (ord l - 64)) (reverse letters) [0 ..]) - 1

addressRange :: Address -> Address -> [Address]
addressRange (aCol, aRow) (bCol, bRow) = concatMap (\col -> map (col,) [(min aRow bRow) .. (max aRow bRow)]) [(min aCol bCol) .. (max aCol bCol)]

instance Show LiteralValue where
  show = \case
    Float x -> show x
    Int x -> show x
    Bool x -> if x then "true" else "false"
    List xs -> show xs
    Null -> "null"
    String s -> show s

data SourceRange = SourcePosRange {left :: SourcePos, right :: SourcePos}

instance Show SourceRange where
  show SourcePosRange {left, right} = show left ++ " to " ++ show right

data BinaryType
  = Add
  | Sub
  | Div
  | Mul
  | Mod
  | Pow
  | And
  | Or
  | BwAnd
  | BwOr
  | BwXor
  | LeftShift
  | RightShift
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Range

instance Show BinaryType where
  show = \case
    Add -> "+"
    Sub -> "-"
    Div -> "/"
    Mul -> "*"
    Mod -> "%"
    Pow -> "**"
    And -> "&&"
    Or -> "||"
    BwAnd -> "&"
    BwOr -> "|"
    BwXor -> "^"
    LeftShift -> "<<"
    RightShift -> ">>"
    Equal -> "=="
    NotEqual -> "!="
    LessThan -> "<"
    LessThanOrEqual -> "<="
    GreaterThan -> ">"
    GreaterThanOrEqual -> ">="
    Range -> ".."

data UnaryType = Neg | Not

instance Show UnaryType where
  show = \case
    Neg -> "-"
    Not -> "!"

data Expression
  = Binary {sourceRange :: SourceRange, binaryType :: BinaryType, lhs :: Expression, rhs :: Expression}
  | Unary {sourceRange :: SourceRange, unaryType :: UnaryType, rhs :: Expression}
  | Literal {sourceRange :: SourceRange, litValue :: LiteralValue}
  | Cell {sourceRange :: SourceRange, address :: Address}
  | CellRange {sourceRange :: SourceRange, startAddress :: Address, endAddress :: Address}
  | ReadVar {sourceRange :: SourceRange, name :: String}
  | SetVar {sourceRange :: SourceRange, name :: String, rhs :: Expression}
  | Function {sourceRange :: SourceRange, name :: String, argsExpressions :: [Expression]}
  | Conditional {sourceRange :: SourceRange, conditionExp :: Expression, trueExp :: Expression, maybeFalseExp :: Maybe Expression}
  | ForEach {sourceRange :: SourceRange, elementName :: String, listExp :: Expression, mapExp :: Expression}
  | ConstructList {sourceRange :: SourceRange, expressions :: [Expression]}
  | Block {sourceRange :: SourceRange, statements :: [Expression]}

instance Show Expression where
  show = \case
    Binary {binaryType, lhs, rhs} -> "(" ++ show lhs ++ show binaryType ++ show rhs ++ ")"
    Unary {unaryType, rhs} -> "(" ++ show unaryType ++ show rhs ++ ")"
    Literal {litValue} -> show litValue
    Block {statements} -> "{" ++ intercalate ";" (map show statements) ++ "}"
    Conditional {conditionExp, trueExp, maybeFalseExp} -> "if " ++ show conditionExp ++ " " ++ show trueExp ++ " else " ++ show maybeFalseExp
    ForEach {elementName, listExp, mapExp} -> "for " ++ elementName ++ " in " ++ show listExp ++ " " ++ show mapExp
    Function {name, argsExpressions} -> name ++ "(" ++ intercalate ", " (map show argsExpressions) ++ ")"
    ReadVar {name} -> name
    SetVar {name, rhs} -> name ++ " = " ++ show rhs
    ConstructList {expressions} -> show expressions

type EvalResult = Either Expression (Enviroment, LiteralValue)

data Enviroment = Enviroment
  { variables :: Map.Map String LiteralValue,
    functions :: Map.Map String ((SourceRange, [LiteralValue]) -> Maybe LiteralValue),
    cells :: Map.Map Address LiteralValue
  }

-- lookupCell address env@Enviroment {grid} = parse expressionParser "" (fromMaybe "0" (Map.lookup address grid))

functionList =
  [ ("range", \case (sourceRange, [Int a, Int b]) -> Just $ List $ map Int [a .. b]; _ -> Nothing),
    ( "mean",
      \case
        params@(sourceRange, [List xs]) ->
          ( case sumFunc params of
              Just (Float x) -> Just $ Float $ x / fromIntegral (length xs)
              Just (Int x) -> Just $ Float $ fromIntegral x / fromIntegral (length xs)
              _ -> Nothing
          )
        _ -> Nothing
    ),
    ("min", \case (sourceRange, [List xs]) -> foldM (chooseLeftWhen LessThan) Null xs; _ -> Nothing),
    ("max", \case (sourceRange, [List xs]) -> foldM (chooseLeftWhen GreaterThan) Null xs; _ -> Nothing),
    ( "sum",
      sumFunc
    )
  ]
  where
    chooseLeftWhen binaryType lhs rhs = case literalBinaryOp binaryType (lhs, rhs) of
      Just (Bool True) -> Just lhs
      Just (Bool False) -> Just rhs
      x -> x
    sumFunc = foldBinaryOp Add
    foldBinaryOp op = \case (sourceRange, [List xs]) -> foldM (curry (literalBinaryOp op)) Null (reverse xs); _ -> Nothing

variableList =
  [ ("pi", Float 3.14159265358979323846264)
  ]

emptyEnviroment :: Enviroment
emptyEnviroment =
  Enviroment
    { variables = Map.fromList variableList,
      functions = Map.fromList functionList,
      cells = mempty
    }

mockRange = SourcePosRange {left = newPos "" 0 0, right = newPos "" 0 0}

instance Show Enviroment where
  show Enviroment {variables, functions} = "{variables=" ++ show variables ++ "}"

literalBinaryOp binaryType literals =
  ( case binaryType of
      Add ->
        floatOp (+)
          <|> intOp (+)
          <|> ( do
                  (case literals of (String a, String b) -> Just $ String (a ++ b); _ -> Nothing)
              )
      Sub -> floatOp (-) <|> intOp (-)
      Div -> floatOp (/) <|> intOp div
      Mul -> floatOp (*) <|> intOp (*)
      Mod -> floatOp mod' <|> intOp mod
      Pow -> floatOp (+) <|> intOp (+)
      And -> boolOp (&&)
      Or -> boolOp (||)
      BwAnd -> intOp (.&.)
      BwOr -> intOp (.|.)
      BwXor -> intOp xor
      LeftShift -> intOp shiftL
      RightShift -> intOp shiftR
      Equal -> numBoolOp (==) <|> boolOp (==)
      NotEqual -> numBoolOp (/=) <|> boolOp (/=)
      LessThan -> numBoolOp (<)
      LessThanOrEqual -> numBoolOp (<=)
      GreaterThan -> numBoolOp (>)
      GreaterThanOrEqual -> numBoolOp (>=)
      Range -> (case literals of (Int a, Int b) -> Just $ List (map Int [a .. b]); _ -> Nothing)
  )
    <|> ignoreNull
  where
    ignoreNull = do
      case literals of
        (a, Null) -> Just a
        (Null, a) -> Just a
        _ -> Nothing
    intOp f =
      case literals of
        (Int a, Int b) -> Just $ Int $ f a b
        (Int a, Float b) -> Just $ Int $ f a (round b)
        (Float a, Int b) -> Just $ Int $ f (round a) b
        _ -> Nothing
    numOp f =
      case literals of
        (Int a, Int b) -> Just $ Float $ f (fromIntegral a) (fromIntegral b)
        (Float a, Float b) -> Just $ Float $ f a b
        (Int a, Float b) -> Just $ Float $ f (fromIntegral a) b
        (Float a, Int b) -> Just $ Float $ f a (fromIntegral b)
        _ -> Nothing
    floatOp f =
      case literals of
        (Float a, Float b) -> Just $ Float $ f a b
        (Int a, Float b) -> Just $ Float $ f (fromIntegral a) b
        (Float a, Int b) -> Just $ Float $ f a (fromIntegral b)
        _ -> Nothing
    numBoolOp f =
      case literals of
        (Int a, Int b) -> Just $ Bool $ f (fromIntegral a) (fromIntegral b)
        (Float a, Float b) -> Just $ Bool $ f a b
        (Int a, Float b) -> Just $ Bool $ f (fromIntegral a) b
        (Float a, Int b) -> Just $ Bool $ f a (fromIntegral b)
        _ -> Nothing
    boolOp f = case literals of
      (Bool a, Bool b) -> Just $ Bool $ f a b
      _ -> Nothing

litUnaryOp = \case
  (Not, Bool b) -> Just $ Bool $ not b
  (Neg, Int b) -> Just $ Int $ negate b
  (Neg, Float b) -> Just $ Float $ negate b
  _ -> Nothing

evaluate :: Enviroment -> Expression -> EvalResult
evaluate env exp@Binary {lhs, rhs, binaryType} = do
  (lhsEnv, lhsLitVal) <- evaluate env lhs
  (rhsEnv, rhsLitVal) <- evaluate lhsEnv rhs
  maybe
    (Left exp)
    (Right . (rhsEnv,))
    (literalBinaryOp binaryType (lhsLitVal, rhsLitVal))
evaluate env exp@Unary {rhs, unaryType} = do
  (rhsEnv, rhsLitVal) <- evaluate env rhs
  maybe
    (Left exp)
    (Right . (rhsEnv,))
    (litUnaryOp (unaryType, rhsLitVal))
evaluate env Literal {litValue} = Right (env, litValue)
evaluate env@Enviroment {cells} exp@Cell {address} = maybe (Right (env, Null)) (Right . (env,)) (Map.lookup address cells)
evaluate env exp@CellRange {sourceRange, startAddress, endAddress} =
  evaluate
    env
    ConstructList
      { sourceRange,
        expressions = map (\address -> Cell {sourceRange, address}) (addressRange startAddress endAddress)
      }
evaluate env@Enviroment {variables} exp@ReadVar {name} =
  maybe
    (Left exp)
    (Right . (env,))
    (Map.lookup name variables)
evaluate env@Enviroment {variables} exp@SetVar {name, rhs} = do
  (rhsEnv, rhsLitVal) <- evaluate env rhs
  Right (rhsEnv {variables = Map.insert name rhsLitVal variables}, Null)
evaluate env@Enviroment {functions} exp@Function {sourceRange, name, argsExpressions} = do
  (env2, x) <- evaluate env ConstructList {sourceRange, expressions = argsExpressions}
  case (Map.lookup name functions, x) of
    (Just f, List xs) -> case f (sourceRange, reverse xs) of
      Just x -> Right (env2, x)
      _ -> Left exp
    _ -> Left exp
evaluate env1 exp@Conditional {conditionExp, trueExp, maybeFalseExp} = do
  (env2, conditionEval) <- evaluate env1 conditionExp
  case (conditionEval, maybeFalseExp) of
    (Bool True, Just _) -> evaluate env2 trueExp
    (Bool False, Just falseExp) -> evaluate env2 falseExp
    (Bool True, Nothing) -> evaluate env2 trueExp
    (Bool False, Nothing) -> Right (env2, Null)
    _ -> Left conditionExp
evaluate env1 exp@ConstructList {sourceRange, expressions} = do
  (env2, result) <- foldM (\(envA, List xs) b -> (\(e, x) -> (e, List (x : xs))) <$> evaluate envA b) (env1, List []) expressions
  case result of
    List xs -> Right (env2, List $ reverse xs)
    _ -> Left exp
evaluate env1 exp@ForEach {sourceRange, listExp, elementName, mapExp} = do
  (env2, listEval) <- evaluate env1 listExp
  case listEval of
    List xs ->
      evaluate
        env2
        ConstructList
          { sourceRange,
            expressions =
              map
                ( \x ->
                    Block
                      { sourceRange,
                        statements =
                          [ SetVar
                              { sourceRange,
                                name = elementName,
                                rhs = Literal {sourceRange, litValue = x}
                              },
                            mapExp
                          ]
                      }
                )
                xs
          }
    _ -> Left listExp
evaluate env exp@Block {statements = [x]} = evaluate env x -- possibly remove variables that are out of scope. after evaluating block remove variables that were created in this block from enviroment
evaluate env1 exp@Block {statements = x : xs} = do
  (env2, xEval) <- evaluate env1 x
  evaluate env2 exp {statements = xs}

lexer =
  makeTokenParser $
    emptyDef
      { commentStart = "/*",
        commentEnd = "*/",
        commentLine = "//",
        nestedComments = True,
        identStart = lower,
        identLetter = alphaNum,
        reservedNames = ["if", "else", "for", "in", "[]", "false", "true", "null"],
        reservedOpNames = [],
        caseSensitive = False
      }

expressionParser :: ParsecT String u Identity Expression
expressionParser =
  buildExpressionParser
    [ [un Not],
      [bin Range],
      [bin Pow],
      [bin Mul, bin Div, bin Mod],
      [bin Add, bin Sub],
      [bin LeftShift, bin RightShift],
      [bin LessThan, bin LessThanOrEqual, bin GreaterThan, bin GreaterThanOrEqual],
      [bin NotEqual, bin Equal],
      [bin BwAnd],
      [bin BwXor],
      [bin BwOr],
      [bin And],
      [bin Or]
    ]
    ( literalParser
        <|> parens lexer expressionParser
        <|> blockParser
        <|> conditionalParser
        <|> forEachParser
        <|> try functionParser
        <|> try setVarParser
        <|> readVarParser
        <|> constructListParser
        <|> try cellRangeParser
        <|> cellParser
    )
  where
    un t = Prefix $ do
      left <- getPosition
      reservedOp lexer (show t)
      right <- getPosition
      return $ \rhs -> Unary {sourceRange = SourcePosRange {left, right}, unaryType = t, rhs}
    bin' assoc t =
      Infix
        ( do
            left <- getPosition
            reservedOp lexer (show t)
            right <- getPosition
            return $ \lhs rhs -> Binary {sourceRange = SourcePosRange {left, right}, binaryType = t, rhs, lhs}
        )
        assoc
    bin = binl
    binl = bin' AssocLeft
    binr = bin' AssocRight
    literalParser = do
      left <- getPosition
      litValue <-
        (reserved lexer "true" >> return (Bool True))
          <|> (reserved lexer "false" >> return (Bool False))
          <|> (reserved lexer "null" >> return Null)
          <|> (reserved lexer "[]" >> return (List []))
          <|> try (Float . realToFrac <$> float lexer)
          <|> (Int . fromIntegral <$> integer lexer)
          <|> (String <$> stringLiteral lexer)
      right <- getPosition
      return $ Literal {sourceRange = SourcePosRange {left, right}, litValue}
    blockParser = do
      left <- getPosition
      statements <- braces lexer (semiSep1 lexer expressionParser)
      right <- getPosition
      return $ Block {sourceRange = SourcePosRange {left, right}, statements}
    conditionalParser = do
      left <- getPosition
      reserved lexer "if"
      conditionExp <- expressionParser
      trueExp <- blockParser
      maybeFalseExp <- optionMaybe (reserved lexer "else" >> blockParser)
      right <- getPosition
      let sourceRange = SourcePosRange {left, right}
      return $ Conditional {sourceRange, conditionExp, trueExp, maybeFalseExp}
    forEachParser = do
      left <- getPosition
      reserved lexer "for"
      elementName <- identifier lexer
      reserved lexer "in"
      listExp <- expressionParser
      mapExp <- blockParser
      right <- getPosition
      return $ ForEach {sourceRange = SourcePosRange {left, right}, elementName, listExp, mapExp}
    functionParser = do
      left <- getPosition
      name <- identifier lexer
      argsExpressions <- parens lexer (commaSep lexer expressionParser)
      right <- getPosition
      return $ Function {sourceRange = SourcePosRange {left, right}, name, argsExpressions}
    readVarParser = do
      left <- getPosition
      name <- identifier lexer
      right <- getPosition
      return $ ReadVar {sourceRange = SourcePosRange {left, right}, name}
    setVarParser = do
      left <- getPosition
      name <- identifier lexer
      symbol lexer "="
      rhs <- expressionParser
      right <- getPosition
      return $ SetVar {sourceRange = SourcePosRange {left, right}, name, rhs}
    constructListParser = do
      left <- getPosition
      expressions <- brackets lexer (commaSep lexer expressionParser)
      right <- getPosition
      return $ ConstructList {sourceRange = SourcePosRange {left, right}, expressions}
    cellParser = do
      left <- getPosition
      (col, row) <- lexeme lexer addressParser
      right <- getPosition
      return $ Cell {sourceRange = SourcePosRange {left, right}, address = (col, row)}
    cellRangeParser = do
      left <- getPosition
      (startAddress, endAddress) <- lexeme lexer addressRangeParser
      right <- getPosition
      return $ CellRange {sourceRange = SourcePosRange {left, right}, startAddress, endAddress}

addressParser = liftM2 (,) (lettersToNum <$> many1 (satisfy isUpper)) (read <$> many1 (satisfy isDigit))

addressRangeParser = do
  start <- addressParser
  string ":"
  end <- addressParser
  return (start, end)

mainParser :: ParsecT String u Identity Expression
mainParser = do
  exp <- mainBlock <|> expressionParser
  eof
  return exp
  where
    mainBlock = do
      left <- getPosition
      statements <- semiSep1 lexer expressionParser
      right <- getPosition
      return $ Block {sourceRange = SourcePosRange {left, right}, statements}