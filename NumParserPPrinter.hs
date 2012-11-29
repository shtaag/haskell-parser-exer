module NumParserPPrinter where

import Text.PrettyPrint
import DoNumericParser

printNat :: Expr -> Doc
printNat (Nat x) = int x

printAdd :: Expr -> Doc
printAdd (Add x@(Add _ _) y@(Add _ _)) =
    text "(" <> printExpr x <> text ")+(" <> printExpr y <> text ")"
printAdd (Add x y@(Add _ _)) =
    printExpr x <> text "+(" <> printExpr y <> text ")"
printAdd (Add x@(Add _ _) y) =
    text "(" <> printExpr x <> text ")+" <> printExpr y
printAdd (Add x y) =
    printExpr x <> text "+" <> printExpr y

printMulti :: Expr -> Doc
printMulti (Multi x@(Add _ _) y@(Add _ _)) =
    text "(" <> printExpr x <> text ")*(" <> printExpr y <> text ")"
printMulti (Multi x@(Add _ _) y) =
    text "(" <> printExpr x <> text ")*" <> printExpr y
printMulti (Multi x y@(Add _ _)) =
    printExpr x <> text "*(" <> printExpr y <> text ")"
printMulti (Multi x y) =
    printExpr x <> text "*" <> printExpr y

printExpr :: Expr -> Doc
printExpr (Nat x) = printNat (Nat x)
printExpr (Add x y) = printAdd (Add x y)
printExpr (Multi x y) = printMulti (Multi x y)
