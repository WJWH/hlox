module Interpreter where

import Types

-- Inxpired by https://github.com/ccntrq/loxomotive/blob/master/src/Loxomotive/Interpreter.hs,
-- the core interpreter type is an ExceptT StateT IO value:
-- - ExceptT so that we can throw exceptions from anywhere without having to wrap/unwrap Eithers,
-- - StateT to keep state about variable bindings etc
-- - IO as base monad is required because the PRINT method is baked right into the language
type Interpreter = undefined

evaluate :: Expression -> RuntimeValue
evaluate (Grouping expr) = evaluate expr
evaluate (Unary op expr) = case op of
  Negate -> undefined
  Bang -> undefined
evaluate (Binary op left right) = undefined
evaluate (Literal (NumberLit num)) = Number num
evaluate (Literal (StringLit str)) = String str
evaluate (Literal TrueLit) = Boolean True
evaluate (Literal FalseLit) = Boolean False
evaluate (Literal NilLit) = Null

