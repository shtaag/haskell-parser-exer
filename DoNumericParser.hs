module DoNumericParser where
{- 
    Parser Exercise with do expression. ex. ghci> parseTest expr "1+(2*3)"
-}
import Data.Char (digitToInt)
import Text.Parsec (char, (<|>), oneOf, parseTest)
import Text.Parsec.String (Parser)

{- 
    BNF of one digit addition & multiplication
    expr ::= term '+' expr | term
    term ::= factor '*' term | factor
    factor ::= '(' expr ')' | nat
    nat ::= '0' | '1' | ... | '9'
-}
data Expr =
    Nat Int
  | Add Expr Expr
  | Multi Expr Expr
  deriving (Show, Eq)

nat :: Parser Expr
nat = do
    digitc <- oneOf ['0' .. '9']
    return $ Nat $ digitToInt digitc

factor :: Parser Expr
factor = do
    char '('
    e <- expr
    char ')'
    return e
    <|> nat

term :: Parser Expr
term = do
    f <- factor
    do
        char '*'
        t <- term
        return $ Multi f t
        <|> return f

expr :: Parser Expr
expr = do
    t <- term
    do
        char '+'
        e <- expr
        return $ Add t e
        <|> return t
