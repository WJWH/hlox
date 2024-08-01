module Scanner where

import Data.Char
import qualified Data.Map as M

import Types

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

scanner :: Int -> Int -> String -> [Token]
-- EOF, natural end of recursion
scanner !linenr !pos [] = [Token EOF "" linenr pos]
-- whitespace of various kinds. We just skip these, when encountering a newline we also increase the linenr
scanner !linenr !pos (' ':xs)  = scanner linenr (pos+1) xs
scanner !linenr !pos ('\r':xs) = scanner linenr (pos+1) xs
scanner !linenr !pos ('\t':xs) = scanner linenr (pos+1) xs
scanner !linenr _ ('\n':xs) = scanner (linenr+1) 0 xs
-- lexemes that MIGHT be multicharacter but could also be single character
scanner !linenr !pos ('!':'=':xs) = Token BANG_EQUAL "!=" linenr pos : scanner linenr (pos+2) xs
scanner !linenr !pos ('!':xs) = Token BANG "!" linenr pos : scanner linenr (pos+1) xs
scanner !linenr !pos ('=':'=':xs) = Token EQUAL_EQUAL "==" linenr pos : scanner linenr (pos+2) xs
scanner !linenr !pos ('=':xs) = Token EQUAL "=" linenr pos : scanner linenr (pos+1) xs
scanner !linenr !pos ('>':'=':xs) = Token GREATER_EQUAL ">=" linenr pos : scanner linenr (pos+2) xs
scanner !linenr !pos ('>':xs) = Token GREATER ">" linenr pos : scanner linenr (pos+1) xs
scanner !linenr !pos ('<':'=':xs) = Token LESS_EQUAL "<=" linenr pos : scanner linenr (pos+2) xs
scanner !linenr !pos ('<':xs) = Token LESS "<" linenr pos : scanner linenr (pos+1) xs
-- comment, no token generated but we just drop everything until the newline. The newline doesn't need dropping because we want
-- that to be handled by the `\n` handler which also updates the linenr. The pos doesn't need updates either because no token
-- is being generated
scanner !linenr !pos ('/':'/':xs) = scanner linenr pos $ dropWhile (/= '\n') xs
scanner !linenr !pos ('/':xs) = Token SLASH "/" linenr pos : scanner linenr (pos+1) xs
-- single token lexemes
scanner !linenr !pos ('(':xs) = Token LEFT_PAREN "(" linenr pos : scanner linenr (pos+1) xs
scanner !linenr !pos (')':xs) = Token RIGHT_PAREN ")" linenr pos : scanner linenr (pos+1) xs
scanner !linenr !pos ('{':xs) = Token LEFT_BRACE "{" linenr pos : scanner linenr (pos+1) xs
scanner !linenr !pos ('}':xs) = Token RIGHT_BRACE "}" linenr pos : scanner linenr (pos+1) xs
scanner !linenr !pos (',':xs) = Token COMMA "," linenr pos : scanner linenr (pos+1) xs
scanner !linenr !pos ('.':xs) = Token DOT "." linenr pos : scanner linenr (pos+1) xs
scanner !linenr !pos ('-':xs) = Token MINUS "-" linenr pos : scanner linenr (pos+1) xs
scanner !linenr !pos ('+':xs) = Token PLUS "+" linenr pos : scanner linenr (pos+1) xs
scanner !linenr !pos (';':xs) = Token SEMICOLON ";" linenr pos : scanner linenr (pos+1) xs
scanner !linenr !pos ('*':xs) = Token STAR "*" linenr pos : scanner linenr (pos+1) xs
-- strings
scanner !linenr !pos ('"':xs) = Token (STRING str) str linenr pos : scanner (linenr + newlineCount) (pos + (length str)) rest
  where (str,newlineCount,rest) = stringLiteral xs
scanner !linenr !pos (c:xs)
  -- numbers
  | isDigit c = Token (NUMBER num) (show num) linenr pos : scanner (linenr) (pos + (length $ show num)) rest
  -- identifiers and reserved words
  | isAlpha c = Token (tokType) lexeme linenr pos : scanner (linenr) (pos + (length lexeme)) tokRest
  -- unknown character leads to error
  | otherwise = Token (ERROR $ concat ["Unknown character: ", show c, " on line ", show linenr]) "" linenr pos : scanner linenr pos xs
  where (num,rest) = numberLiteral (c:xs)
        (tokType,lexeme,tokRest) = identifier (c:xs)

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

identifier :: String -> (TokenType,String,String)
identifier xs = (token,name,rest)
  where (name,rest) = span isAlphaNum xs
        token = case M.lookup name reservedWords of
          Nothing -> IDENTIFIER name
          Just tok -> tok
