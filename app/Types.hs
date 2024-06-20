module Types where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M

-- Tokeniser related types
data TokenType = LEFT_PAREN -- single character tokens
               | RIGHT_PAREN
               | LEFT_BRACE
               | RIGHT_BRACE
               | COMMA
               | DOT
               | MINUS
               | PLUS
               | SEMICOLON
               | SLASH
               | STAR
               | BANG -- one or two characters
               | BANG_EQUAL
               | EQUAL
               | EQUAL_EQUAL
               | GREATER
               | GREATER_EQUAL
               | LESS
               | LESS_EQUAL
               | IDENTIFIER String -- literals
               | STRING String
               | NUMBER Double
               | AND -- reserved words
               | CLASS
               | ELSE
               | FALSE
               | FUN
               | FOR
               | IF
               | NIL
               | OR
               | PRINT
               | RETURN
               | SUPER
               | THIS
               | TRUE
               | VAR
               | WHILE
               | EOF -- eof, cannot really be typed in of course but still handy to have for the parser
               | ERROR String -- Special case for reporting errors
               deriving (Show,Eq)

data Token = Token { tokenType :: TokenType, lexeme :: String, line :: Int } deriving (Show,Eq)

-- Parser related types
data UnaryOperation = Negate | Bang deriving (Show,Eq)
data BinaryOperation = DoubleEqual | NotEqual | LessThan | LessEqualThan | GreaterThan | GreaterEqualThan
                     | Add | Subtract | Multiply | Divide deriving (Show,Eq)
data LogicalOperation = And | Or deriving (Show,Eq)
data LiteralContents = NumberLit Double
                     | StringLit String
                     | TrueLit
                     | FalseLit
                     | NilLit
                     deriving (Show,Eq)

data Expression = Grouping Expression
                | Unary UnaryOperation Expression
                | Binary BinaryOperation Expression Expression
                | Literal LiteralContents
                | Variable String
                | Assignment String Expression
                | Logical LogicalOperation Expression Expression
                deriving (Show,Eq)

data Statement = ExprStatement Expression
               | PrintStatement Expression
               | VariableDeclaration String (Maybe Expression)
               | Block [Statement]
               | IfStatement Expression Statement (Maybe Statement)
               | WhileStatement Expression Statement
               deriving (Show,Eq)

-- Types for the interpreter
data RuntimeValue = Number Double
                  | String String
                  | Boolean Bool
                  | Null
                  deriving (Show,Eq)

-- Inspired by https://github.com/ccntrq/loxomotive/blob/master/src/Loxomotive/Interpreter.hs,
-- the core interpreter type is an ExceptT StateT IO value:
-- - ExceptT so that we can throw exceptions from anywhere without having to wrap/unwrap Eithers all the time
-- - StateT to keep state about variable bindings etc
-- - IO as base monad is required because the PRINT method is baked right into the language
type Interpreter = ExceptT InterpreterError (StateT InterpreterState IO)
data Env = Env { parent :: Maybe Env, bindings :: M.Map String RuntimeValue } deriving (Show,Eq)
data InterpreterState = InterpreterState { env :: Env } deriving (Show,Eq)
data InterpreterError = ArgumentError { description :: String }
                      | RuntimeError { description :: String }
                      deriving (Show,Eq)
