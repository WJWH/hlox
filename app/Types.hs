module Types where

import Control.Monad.Except
import Control.Monad.State
import Data.IORef
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
               deriving (Show,Eq,Ord)

data Token = Token { tokenType :: TokenType, lexeme :: String, line :: Int, posOnLine :: Int } deriving (Show,Eq,Ord)

-- Parser related types
data UnaryOperation = Negate | Bang deriving (Show,Eq,Ord)
data BinaryOperation = DoubleEqual | NotEqual | LessThan | LessEqualThan | GreaterThan | GreaterEqualThan
                     | Add | Subtract | Multiply | Divide deriving (Show,Eq,Ord)
data LogicalOperation = And | Or deriving (Show,Eq,Ord)
data LiteralContents = NumberLit Double
                     | StringLit String
                     | TrueLit
                     | FalseLit
                     | NilLit
                     deriving (Show,Eq,Ord)

data Expression = Grouping Expression
                | Unary UnaryOperation Expression
                | Binary BinaryOperation Expression Expression
                | Literal LiteralContents
                | Variable Token
                | Assignment Token Expression
                | Logical LogicalOperation Expression Expression
                | Call Expression Token [Expression]
                | Get Expression Expression Token -- callee, property name (always an identifier), name token
                | Set Expression Expression Token Expression -- callee, property name, name token, value
                | Super Token Expression -- token for disambiguation and the identifier for the method to be called on the superclass
                | This Token
                deriving (Show,Eq,Ord)

data Statement = ExprStatement Expression
               | PrintStatement Expression
               | VariableDeclaration Token (Maybe Expression)
               | FunctionDeclaration Token [Token] Statement -- name, params, body (must be a Block)
               | Block [Statement]
               | IfStatement Expression Statement (Maybe Statement)
               | WhileStatement Expression Statement
               | EmptyStatement
               | ReturnStatement (Maybe Expression)
               | ClassDeclaration Token (Maybe Expression) [Statement] -- Tokens for the name, maybe a Variable expression for the superclass, and a list of methods
               deriving (Show,Eq)

-- Types for the interpreter
data RuntimeValue = Number Double
                  | String String
                  | Boolean Bool
                  | Null
                  | LoxFunction Int String [String] Statement Env Bool -- arity, name, arg names, body, closure, isInitializer
                  | NativeFunction Int String ([RuntimeValue] -> Interpreter RuntimeValue) -- arity, name, some code block to run
                  | LoxClass {className :: String, superclass :: Maybe RuntimeValue, classMethods :: (M.Map String RuntimeValue) } -- Class name, superclass and map (String -> LoxFunction) for the methods
                  | LoxInstance RuntimeValue (IORef (M.Map String RuntimeValue)) -- class type and a mutable map for the fields

instance Show RuntimeValue where
  show (Number num) = show num
  show (String str) = str
  show (Boolean b) = show b
  show Null = "null"
  show (LoxFunction _ name _ _ _ _) = "<user defined function" ++ name ++ ">"
  show (NativeFunction _ name _) = "<native function" ++ name ++ ">"
  show (LoxClass name _ _) = "<Class " ++ name ++ ">"
  show (LoxInstance name _fields) = "<Instance of" ++ (show name) ++ ">"

instance Eq RuntimeValue where
  (==) (Number num1) (Number num2) = num1 == num2
  (==) (String str1) (String str2) = str1 == str2
  (==) (Boolean b1) (Boolean b2) = b1 == b2
  (==) Null Null = True
  (==) (LoxFunction _ _ _ _ _ _) (LoxFunction _ _ _ _ _ _) = False -- functions cannot be equal-ed
  (==) (NativeFunction _ _ _) (NativeFunction _ _ _) = False
  (==) (LoxClass name1 _ _) (LoxClass name2 _ _) = name1 == name2
  (==) (LoxInstance name1 fields1) (LoxInstance name2 fields2) = name1 == name2 && fields1 == fields2
  (==) _ _ = False -- type mismatch

-- Inspired by https://github.com/ccntrq/loxomotive/blob/master/src/Loxomotive/Interpreter.hs,
-- the core interpreter type is an ExceptT StateT IO value:
-- - ExceptT so that we can throw exceptions from anywhere without having to wrap/unwrap Eithers all the time
-- - StateT to keep state about variable bindings etc
-- - IO as base monad is required because the PRINT method is baked right into the language
type Interpreter = ExceptT InterpreterError (StateT InterpreterState IO)
data Env = Env { parent :: Maybe Env
               , bindings :: IORef (M.Map String RuntimeValue)
               } deriving (Eq)
data InterpreterState = InterpreterState { env :: Env, globals :: Env, locals :: Locals } deriving (Eq)
data InterpreterError = ArgumentError { description :: String }
                      | RuntimeError { description :: String }
                      | ReturnValue { value :: RuntimeValue } -- using this to implement `return` statements
                      deriving (Show,Eq)

-- types for the resolver are very much like the types for the interpreter itself, since it also walks
-- the entire AST
type Resolver = ExceptT ResolverError (StateT ResolverState IO)
data ResolverError = ResolverError String deriving (Show,Eq)
data ResolverState = ResolverState { scopes :: [Scope]
                                   , resolverLocals :: Locals
                                   , currentFunction :: FunctionType
                                   , currentClass :: ClassType
                                   } deriving (Show,Eq)
type Scope = M.Map String Bool
type Locals = M.Map Expression Int
data FunctionType = None | Function | Method | Initializer deriving (Show,Eq)
data ClassType = NoClass | InClass | InSubClass deriving (Show,Eq)
