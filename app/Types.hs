module Types where

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

data UnaryOperation = Negate | Bang deriving (Show,Eq)
data BinaryOperation = DoubleEqual | NotEqual | LessThan | LessEqualThan | GreaterThan | GreaterEqualThan
                     | Add | Subtract | Multiply | Divide deriving (Show,Eq)
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