module Scanner where

import Data.Char
import qualified Data.Map as M

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

reservedWords :: M.Map String TokenType
reservedWords = M.fromList [
    ("and",AND),
    ("class",CLASS),
    ("else",ELSE),
    ("false",FALSE),
    ("for",FOR),
    ("fun",FUN),
    ("if",IF),
    ("nil",NIL),
    ("or",OR),
    ("print",PRINT),
    ("return",RETURN),
    ("super",SUPER),
    ("this",THIS),
    ("true",TRUE),
    ("var",VAR),
    ("while",WHILE)
  ]

scanner :: Int -> String -> [Token]
-- EOF, natural end of recursion
scanner !linenr [] = [Token EOF "" linenr]
-- whitespace of various kinds. We just skip these, when encountering a newline we also increase the linenr
scanner !linenr (' ':xs)  = scanner linenr xs
scanner !linenr ('\r':xs) = scanner linenr xs
scanner !linenr ('\t':xs) = scanner linenr xs
scanner !linenr ('\n':xs) = scanner (linenr+1) xs
-- lexemes that MIGHT be multicharacter but could also be single character
scanner !linenr ('!':'=':xs) = Token BANG_EQUAL "!=" linenr : scanner linenr xs
scanner !linenr ('!':xs) = Token BANG "!" linenr : scanner linenr xs
scanner !linenr ('=':'=':xs) = Token EQUAL_EQUAL "==" linenr : scanner linenr xs
scanner !linenr ('=':xs) = Token EQUAL "=" linenr : scanner linenr xs
scanner !linenr ('>':'=':xs) = Token GREATER_EQUAL ">=" linenr : scanner linenr xs
scanner !linenr ('>':xs) = Token GREATER ">" linenr : scanner linenr xs
scanner !linenr ('<':'=':xs) = Token LESS_EQUAL "<=" linenr : scanner linenr xs
scanner !linenr ('<':xs) = Token LESS "<" linenr : scanner linenr xs
-- comment, no token generated but we just drop everything until the newline. The newline doesn't need dropping because we want
-- that to be handled by the `\n` handler which also updates the linenr
scanner !linenr ('/':'/':xs) = scanner linenr $ dropWhile (/= '\n') xs
scanner !linenr ('/':xs) = Token SLASH "/" linenr : scanner linenr xs
-- single token lexemes
scanner !linenr ('(':xs) = Token LEFT_PAREN "(" linenr : scanner linenr xs
scanner !linenr (')':xs) = Token RIGHT_PAREN ")" linenr : scanner linenr xs
scanner !linenr ('{':xs) = Token LEFT_BRACE "{" linenr : scanner linenr xs
scanner !linenr ('}':xs) = Token RIGHT_BRACE "}" linenr : scanner linenr xs
scanner !linenr (',':xs) = Token COMMA "," linenr : scanner linenr xs
scanner !linenr ('.':xs) = Token DOT "." linenr : scanner linenr xs
scanner !linenr ('-':xs) = Token MINUS "-" linenr : scanner linenr xs
scanner !linenr ('+':xs) = Token PLUS "+" linenr : scanner linenr xs
scanner !linenr (';':xs) = Token SEMICOLON ";" linenr : scanner linenr xs
scanner !linenr ('*':xs) = Token STAR "*" linenr : scanner linenr xs
-- strings
scanner !linenr ('"':xs) = Token (STRING str) str linenr : scanner (linenr + newlineCount) rest
  where (str,newlineCount,rest) = stringLiteral xs
scanner !linenr (c:xs)
  -- numbers
  | isDigit c = Token (NUMBER num) (show num) linenr : scanner (linenr) rest
  -- identifiers and reserved words
  | isAlpha c = Token (tokType) "" linenr : scanner (linenr) tokRest -- how to put the string for the lexeme in here properly?
  -- unknown character leads to error
  | otherwise = Token (ERROR $ concat ["Unknown character: ", show c, " on line ", show linenr]) "" linenr : scanner linenr xs
  where (num,rest) = numberLiteral (c:xs)
        (tokType,tokRest) = identifier (c:xs)

stringLiteral :: String -> (String,Int,String)
stringLiteral xs = (str,newlineCount, drop 1 rest) -- skip the closing '"' too
  where (str, rest) = span (/= '"') xs
        newlineCount = length $ filter (== '\n') str

numberLiteral :: String -> (Double,String)
numberLiteral xs = (num,rest)
  where (firstPart,xs') = span isDigit xs
        (num,rest) = case xs' of
          ('.':_) -> if (null fractionalPart)
            then (read firstPart, xs') -- the fractional part was the empty string so the dot was not part of the number
            else (read $ concat [firstPart, ".", fractionalPart], restOfInput)
          _ -> (read firstPart, xs') -- no fractional part
        (fractionalPart, restOfInput) = span isDigit (drop 1 xs')

identifier :: String -> (TokenType,String)
identifier xs = (token,rest)
  where (name,rest) = span isAlphaNum xs
        token = case M.lookup name reservedWords of
          Nothing -> IDENTIFIER name
          Just tok -> tok
