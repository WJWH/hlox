module Parser where

import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.Pos

import Types

-- From the book, this is our grammar for now:
-- expression     → literal
--                | unary
--                | binary
--                | grouping ;

-- literal        → NUMBER | STRING | "true" | "false" | "nil" ;
-- grouping       → "(" expression ")" ;
-- unary          → ( "-" | "!" ) expression ;
-- binary         → expression operator expression ;
-- operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
--                | "+"  | "-"  | "*" | "/" ;

type TokenParser a = ParsecT [Token] () Identity a

-- for tokens that have no content
matchToken :: TokenType -> TokenParser Token
matchToken toktype = token show (const (initialPos "borp")) $ \tok -> if tokenType tok == toktype then Just tok else Nothing

binaryOperator :: TokenParser BinaryOperation
binaryOperator = do
  tok <- choice [matchToken EQUAL_EQUAL, matchToken BANG_EQUAL, matchToken LESS, matchToken LESS_EQUAL,
                    matchToken GREATER, matchToken GREATER_EQUAL, matchToken PLUS, matchToken MINUS, matchToken STAR, matchToken SLASH]
  return $ case tokenType tok of
    EQUAL_EQUAL -> DoubleEqual
    BANG_EQUAL -> NotEqual
    LESS -> LessThan
    LESS_EQUAL -> LessEqualThan
    GREATER -> GreaterThan
    GREATER_EQUAL -> GreaterEqualThan
    PLUS -> Add
    MINUS -> Subtract
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

binary :: TokenParser Expression
binary = do
  leftSide <- expression
  operator <- binaryOperator
  rightSide <- expression
  return $ Binary operator leftSide rightSide

unary :: TokenParser Expression
unary = do
  operator <- unaryOperator
  expr <- expression
  return $ Unary operator expr

-- something between parentheses
grouping :: TokenParser Expression
grouping = do
  between (matchToken LEFT_PAREN) (matchToken RIGHT_PAREN) expression

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

expression :: TokenParser Expression
expression = do
  expr <- try unary <|> try grouping <|> try literal <|> binary
  matchToken SEMICOLON
  matchToken EOF
  return $ expr
-- This doesn't actually work at all, only literals get matched...
-- Soooooooo also this doesn't do any operator precendence.
