module Scanner where

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
-- unknown character leads to error
scanner !linenr (c:xs) = Token (ERROR $ concat ["Unknown character: ", show c, " on line ", show linenr]) "" linenr : scanner linenr xs
