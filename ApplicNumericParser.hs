module ApplicNumericParser where
{- 
    Parser Exercise with Applicative style. ex. ghci> parseTest expr "1+(2*3)"
-}
import Data.Char (digitToInt)
import Control.Applicative ((<$>), (<*>), (*>), (<*), liftA2)
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
nat = Nat . digitToInt <$> oneOf ['0' .. '9']

factor :: Parser Expr
factor =
    (char '(' *> expr <* char ')')
    <|>
    nat

term :: Parser Expr
term =
    liftA2 Multi factor ( factor <* (char '*') *> term)
    -- (Multi <$> factor <* (char '*') *> term)
    <|> 
    factor

expr :: Parser Expr
expr =
    (Add <$> term <* char '+' *> expr)
    <|>
    term
