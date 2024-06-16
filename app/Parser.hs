module Parser where

import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.Pos

import Types

-- From the book, this is our grammar for now:
-- expression     → equality ;
-- equality       → comparison ( ( "!=" | "==" ) comparison )* ;
-- comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
-- term           → factor ( ( "-" | "+" ) factor )* ;
-- factor         → unary ( ( "/" | "*" ) unary )* ;
-- unary          → ( "!" | "-" ) unary
--                | primary ;
-- primary        → NUMBER | STRING | "true" | "false" | "nil"
--                | "(" expression ")" ;

type TokenParser a = ParsecT [Token] () Identity a

-- for tokens that have no content
matchToken :: TokenType -> TokenParser Token
matchToken toktype = token show (const (initialPos "borp")) $ \tok -> if tokenType tok == toktype then Just tok else Nothing

-- something between parentheses
grouping :: TokenParser Expression
grouping = do
  between (matchToken LEFT_PAREN) (matchToken RIGHT_PAREN) expression

-- For tokens that may have content
matchLiteral :: TokenParser LiteralContents
matchLiteral = token show (const (initialPos "borp")) $ \tok -> case tokenType tok of
  TRUE -> Just TrueLit
  FALSE -> Just FalseLit
  NIL -> Just NilLit
  NUMBER num -> Just $ NumberLit num
  STRING str -> Just $ StringLit str
  _ -> Nothing

literal :: TokenParser Expression
literal = do
  litContents <- matchLiteral
  return $ Literal litContents

equalityOperator :: TokenParser BinaryOperation
equalityOperator = do
  tok <- choice [matchToken EQUAL_EQUAL, matchToken BANG_EQUAL]
  return $ case tokenType tok of
    EQUAL_EQUAL -> DoubleEqual
    BANG_EQUAL -> NotEqual
    _ -> error "Unreachable"

comparisonOperator :: TokenParser BinaryOperation
comparisonOperator = do
  tok <- choice [matchToken LESS, matchToken LESS_EQUAL, matchToken GREATER, matchToken GREATER_EQUAL]
  return $ case tokenType tok of
    LESS -> LessThan
    LESS_EQUAL -> LessEqualThan
    GREATER -> GreaterThan
    GREATER_EQUAL -> GreaterEqualThan
    _ -> error "Unreachable"

addSubtractOperator :: TokenParser BinaryOperation
addSubtractOperator = do
  tok <- choice [matchToken PLUS, matchToken MINUS]
  return $ case tokenType tok of
    PLUS -> Add
    MINUS -> Subtract
    _ -> error "Unreachable"

divideMultiplyOperator :: TokenParser BinaryOperation
divideMultiplyOperator = do
  tok <- choice [matchToken STAR, matchToken SLASH]
  return $ case tokenType tok of
    STAR -> Multiply
    SLASH -> Divide
    _ -> error "Unreachable"

unaryOperator :: TokenParser UnaryOperation
unaryOperator = do
  tok <- choice [matchToken MINUS, matchToken BANG]
  return $ case tokenType tok of
    MINUS -> Negate
    BANG -> Bang
    _ -> error "Unreachable"

-- they all look the same anyway, only separated because of operator precedence
binaryGrammarRule element operators = do
  firstElement <- element
  nexts <- many $ do
    op <- operators
    nextElement <- element
    return (op,nextElement)
  return $ foldBinaryOps firstElement nexts

unary = do
  ops <- many unaryOperator
  prim <- primary
  return $ foldUnaryOps prim ops

foldBinaryOps :: Expression -> [(BinaryOperation, Expression)] -> Expression
foldBinaryOps first [] = first
foldBinaryOps first ((op,expr):xs) = foldBinaryOps (Binary op first expr) xs

foldUnaryOps :: Expression -> [UnaryOperation] -> Expression
foldUnaryOps prim [] = prim
-- note this should go "inside out" while the binary one goes the other way
foldUnaryOps prim (op:ops) = Unary op (foldUnaryOps prim ops)

expression = equality
equality = binaryGrammarRule comparison equalityOperator
comparison = binaryGrammarRule term comparisonOperator
term = binaryGrammarRule factor addSubtractOperator
factor = binaryGrammarRule unary divideMultiplyOperator

primary :: TokenParser Expression
primary = literal <|> grouping
