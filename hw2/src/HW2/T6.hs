{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module HW2.T6 
    ( Parser(..)
    , ParseError(..)
    , runP
    , pChar
    , pEof
    , pExpression
    , parseExpr
    ) where

import HW2.T5 ( ExceptState(..) )
import HW2.T4 ( Expr(..), Prim(Div, Mul, Sub, Add) )
import GHC.Natural ( Natural )
import HW2.T1 ( Except(..), Annotated(..), mapExcept )
import Data.Function ( (&) )
import Control.Applicative ( Alternative(..), optional )
import Data.Char (isSpace, digitToInt)
import Control.Monad ( MonadPlus, mfilter, msum )
import Data.Maybe (fromMaybe)
import GHC.Unicode (isDigit)

type ParserFunction a = (Natural, String) -> Except ParseError (Annotated (Natural, String) a)
data ParseError = ErrorAtPos Natural
    deriving Show


newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)


getResult :: Annotated (Natural, String) a -> a
getResult (ans :# _) = ans

runP :: Parser a -> String -> Except ParseError a
runP (P p) str =
    let result = runES p (0, str)
        in result & mapExcept getResult


parser :: ParserFunction a -> Parser a
parser p = P $ ES p

raise :: Natural -> Except ParseError a
raise = Error . ErrorAtPos

parseError :: Parser a
parseError = parser \(pos, _) -> raise pos


-- if string is not empty save char and parse tail, else return error
pChar :: Parser Char
pChar = parser \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

pCharThenSkip :: Char -> Parser()
pCharThenSkip a = do
    mfilter ( == a) pChar
    return ()



instance Alternative Parser where
  empty = parseError
  (<|>) (P firstParser) (P secondParser) =
        parser \state -> let firstResult = runES firstParser state
                             secondResult = runES secondParser state
                            in case firstResult of
                                   suc@(Success _) -> suc
                                   _               -> secondResult

instance MonadPlus Parser

pEof :: Parser ()
pEof = parser \case (pos, "")  -> Success (() :# (pos, ""))
                    (pos, _)   -> raise pos


readDigit :: Parser Char
readDigit = mfilter isDigit pChar


digitToRational :: Char -> Rational
digitToRational = toRational . toInteger . digitToInt

listToIntegerPart :: [Char] -> Rational
listToIntegerPart = foldl (\acc d -> acc * 10 + digitToRational d) 0

listToDecimalPart :: [Char] -> Rational
listToDecimalPart = foldr (\d acc -> (acc + digitToRational d) / 10) 0

readInt :: Parser String
readInt =  some readDigit

pDecimalPart :: Parser Rational
pDecimalPart = do
    pCharThenSkip '.'
    number <- readInt
    let num = listToDecimalPart number
    return num

pIntegerPart :: Parser Rational
pIntegerPart = listToIntegerPart <$> readInt

pDouble :: Parser Double
pDouble = do
    main <- pIntegerPart
    sub  <- optional pDecimalPart
    let subPart = fromMaybe 0 sub
    return $ fromRational (main + subPart)



pVal :: Parser Expr
pVal =  do
    skipSpaces
    Val <$> pDouble


skipSpaces :: Parser ()
skipSpaces = do
    many $ mfilter isSpace pChar
    return ()


type OperationParser = Parser (Expr -> Expr -> Expr)

pOperator :: (Expr -> Expr -> Prim Expr) -> Char -> OperationParser
pOperator operator char = (\a b -> Op $ operator a b) <$ pCharThenSkip char

pMul :: OperationParser
pMul = pOperator Mul '*'

pSub :: OperationParser
pSub = pOperator Sub '-'

pAdd :: OperationParser
pAdd = pOperator Add '+'

pDiv :: OperationParser
pDiv = pOperator Div '/'

pOperations :: [OperationParser] -> Parser Expr -> Parser Expr
pOperations parsers subtermParser = do
    let opParser = msum parsers
    pOperationsWithLeftTerm opParser subtermParser =<< subtermParser


pOperationsWithLeftTerm :: OperationParser -> Parser Expr -> Expr -> Parser Expr
pOperationsWithLeftTerm operations lowLevel leftTerm = do
    skipSpaces
    t <- optional operations
    case t of
        Just op -> do  rightTerm <- lowLevel
                       let expr = op leftTerm rightTerm
                       pOperationsWithLeftTerm operations lowLevel expr
        Nothing -> return leftTerm


pLowPriority :: Parser Expr
pLowPriority = pOperations [pAdd, pSub] pHighPriority


pHighPriority :: Parser Expr
pHighPriority = pOperations [pMul, pDiv] pTerm

pTerm :: Parser Expr
pTerm = pVal <|>
    do  skipSpaces
        pCharThenSkip '('
        term <- pLowPriority
        skipSpaces
        pCharThenSkip ')'
        return term

parseExpr :: String -> Except ParseError Expr
parseExpr = runP pExpression

pExpression :: Parser Expr
pExpression = do
    expr <- pLowPriority
    skipSpaces
    pEof
    return expr        