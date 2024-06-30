{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parser where

import Control.Monad
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
matchToken toktype = token show (\_ -> (initialPos "borp")) $ \tok -> if tokenType tok == toktype then Just tok else Nothing

-- something between parentheses
grouping :: TokenParser Expression
grouping = do
  expr <- between (matchToken LEFT_PAREN) (matchToken RIGHT_PAREN) expression
  return $ Grouping expr

identifier :: TokenParser Expression
identifier = token show (const (initialPos "borp")) $ \tok -> case tokenType tok of
  IDENTIFIER str -> Just $ Variable str
  _ -> Nothing

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

-- This feels like some common structure can be extracted, but it's tricky due to the tokens and AST nodes
-- not always having the same name.
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

orOperator :: TokenParser LogicalOperation
orOperator = do
  matchToken OR
  return Or

andOperator :: TokenParser LogicalOperation
andOperator = do
  matchToken AND
  return And

unaryOperator :: TokenParser UnaryOperation
unaryOperator = do
  tok <- choice [matchToken MINUS, matchToken BANG]
  return $ case tokenType tok of
    MINUS -> Negate
    BANG -> Bang
    _ -> error "Unreachable"

-- they all look the same anyway, only separated because of operator precedence
binaryGrammarRule :: TokenParser Expression -> TokenParser BinaryOperation -> TokenParser Expression
binaryGrammarRule element operators = do
  firstElement <- element
  nexts <- many $ do
    op <- operators
    nextElement <- element
    return (op,nextElement)
  return $ foldBinaryOps firstElement nexts

-- they all look the same anyway, only separated because of operator precedence
logicalGrammarRule :: TokenParser Expression -> TokenParser LogicalOperation -> TokenParser Expression
logicalGrammarRule element operators = do
  firstElement <- element
  nexts <- many $ do
    op <- operators
    nextElement <- element
    return (op,nextElement)
  return $ foldLogicalOps firstElement nexts

unary :: TokenParser Expression
unary = do
  ops <- many unaryOperator
  expr <- call
  return $ foldUnaryOps expr ops

call :: TokenParser Expression
call = do
  prim <- primary
  calls <- many $ do
    matchToken LEFT_PAREN
    args <- expression `sepBy` matchToken COMMA -- conveniently also handles case with zero arguments
    when ((length args) >= 255) $ fail "Can't have more than 255 arguments."
    endParen <- matchToken RIGHT_PAREN
    return $ (args,endParen)
  return $ foldCalls prim calls

assignment :: TokenParser Expression
assignment = do
  expr <- equality
  matchToken EQUAL
  value <- expression
  case expr of
    Variable str -> return $ Assignment str value
    _ -> unexpected "Invalid assignment target" -- needs better reporting?

primary :: TokenParser Expression
primary = literal <|> identifier <|> grouping

-- some utility functions to handle expressions with multiple operators like 1+2+3+4 and !!abc
foldBinaryOps :: Expression -> [(BinaryOperation, Expression)] -> Expression
foldBinaryOps first [] = first
foldBinaryOps first ((op,expr):xs) = foldBinaryOps (Binary op first expr) xs

foldLogicalOps :: Expression -> [(LogicalOperation, Expression)] -> Expression
foldLogicalOps first [] = first
foldLogicalOps first ((op,expr):xs) = foldLogicalOps (Logical op first expr) xs

foldUnaryOps :: Expression -> [UnaryOperation] -> Expression
foldUnaryOps callExpr [] = callExpr
-- note this should go "inside out" while the binary one goes the other way
foldUnaryOps callExpr (op:ops) = Unary op (foldUnaryOps callExpr ops)

foldCalls :: Expression -> [([Expression],Token)] -> Expression
foldCalls callee [] = callee
foldCalls callee ((args,tok):ops) = foldCalls (Call callee tok args) ops

-- The stack that defines operator precedence
expression :: TokenParser Expression
expression = try assignment <|> logicOr
logicOr :: TokenParser Expression
logicOr = logicalGrammarRule logicAnd orOperator
logicAnd :: TokenParser Expression
logicAnd = logicalGrammarRule equality andOperator
equality :: TokenParser Expression
equality = binaryGrammarRule comparison equalityOperator
comparison :: TokenParser Expression
comparison = binaryGrammarRule term comparisonOperator
term :: TokenParser Expression
term = binaryGrammarRule factor addSubtractOperator
factor :: TokenParser Expression
factor = binaryGrammarRule unary divideMultiplyOperator

returnStatement :: TokenParser Statement
returnStatement = do
  matchToken RETURN
  expr <- expression
  matchToken SEMICOLON <?> "semicolon at end of return statement"
  return $ ReturnStatement expr

printStatement :: TokenParser Statement
printStatement = do
  matchToken PRINT
  expr <- expression
  matchToken SEMICOLON <?> "semicolon at end of print statement"
  return $ PrintStatement expr

blockStatement :: TokenParser Statement
blockStatement = do
  stmts <- between (matchToken LEFT_BRACE) (matchToken RIGHT_BRACE) (many declaration)
  return $ Block stmts

ifStatement :: TokenParser Statement
ifStatement = do
  matchToken IF
  matchToken LEFT_PAREN <?> "'(' after 'if'."
  condition <- expression
  matchToken RIGHT_PAREN <?> "')' after if condition."
  thenBranch <- statement
  elseBranch <- option Nothing $ do
    matchToken ELSE
    stmt <- statement
    return $ Just stmt
  return $ IfStatement condition thenBranch elseBranch

whileStatement :: TokenParser Statement
whileStatement = do
  matchToken WHILE
  matchToken LEFT_PAREN <?> "'(' after 'while'."
  condition <- expression
  matchToken RIGHT_PAREN <?> "')' after if condition."
  body <- statement
  return $ WhileStatement condition body

-- for use in for loop definitions only, not allowed in "top level" statements
-- not techically according to the book but it's my language so I can do what I want
-- if it makes the parser nicer :)
emptyStatement :: TokenParser Statement
emptyStatement = do
  matchToken SEMICOLON
  return EmptyStatement

forStatement :: TokenParser Statement
forStatement = do
  matchToken FOR
  matchToken LEFT_PAREN
  initializer <- varDeclaration <|> expressionStatement <|> emptyStatement
  condition <- option Nothing (Just <$> expression)
  matchToken SEMICOLON
  increment <- option Nothing (Just <$> expression)
  matchToken RIGHT_PAREN
  body <- statement
  -- let's stitch this sucker back up into a while statement:
  let whileBody = maybe body (\inc -> Block [body, ExprStatement inc]) increment -- the increment, if any, comes after the loop body
  let whileCondition = maybe (Literal TrueLit) id condition -- if no condition is given, default to True for infinite looping
  let while = WhileStatement whileCondition whileBody
  return $ Block [initializer,while]

expressionStatement :: TokenParser Statement
expressionStatement = do
  expr <- expression
  matchToken SEMICOLON
  return $ ExprStatement expr

statement :: TokenParser Statement
statement = returnStatement <|> printStatement <|> blockStatement <|> ifStatement <|> whileStatement <|> forStatement <|> expressionStatement

varName :: Expression -> String
varName (Variable num) = num
varName _ = error "Unreachable, tried to call numVal on non-number runtime value"

varDeclaration :: TokenParser Statement
varDeclaration = do
  matchToken VAR
  name <- identifier <?> "variable name"
  initializer <- option Nothing $ do
    matchToken EQUAL
    expr <- expression
    return $ Just expr
  matchToken SEMICOLON <?> "semicolon at end of variable declaration"
  return $ VariableDeclaration (varName name) initializer

funDeclaration :: TokenParser Statement
funDeclaration = do
  matchToken FUN
  name <- identifier <?> "function name"
  matchToken LEFT_PAREN
  args <- identifier `sepBy` matchToken COMMA
  matchToken RIGHT_PAREN
  body <- blockStatement
  return $ FunctionDeclaration (varName name) (map varName args) body

declaration :: TokenParser Statement
declaration = funDeclaration <|> varDeclaration <|> statement

program :: TokenParser [Statement]
program = do
  decls <- many declaration
  _ <- matchToken EOF
  return decls
