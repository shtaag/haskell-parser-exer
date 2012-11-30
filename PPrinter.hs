module PPrinter where

import Text.PrettyPrint
import DoNumericParser (Expr(Nat, Add, Multi))


-- / convert Nat to Doc
-- Examples:
-- >>> render $ printNat (Nat 1)
-- 1
printNat :: Expr -> Doc
printNat (Nat x) = int x

-- / convert Add to Doc
printAdd :: Expr -> Doc
printAdd (Add x@(Add _ _) y@(Add _ _)) =
    text "(" <> printExpr x <> text ")+(" <> printExpr y <> text ")"
printAdd (Add x y@(Add _ _)) =
    printExpr x <> text "+(" <> printExpr y <> text ")"
printAdd (Add x@(Add _ _) y) =
    text "(" <> printExpr x <> text ")+" <> printExpr y
printAdd (Add x y) =
    printExpr x <> text "+" <> printExpr y

-- / convert Multi to Doc
printMulti :: Expr -> Doc
printMulti (Multi x@(Add _ _) y@(Add _ _)) =
    text "(" <> printExpr x <> text ")*(" <> printExpr y <> text ")"
printMulti (Multi x@(Add _ _) y) =
    text "(" <> printExpr x <> text ")*" <> printExpr y
printMulti (Multi x y@(Add _ _)) =
    printExpr x <> text "*(" <> printExpr y <> text ")"
printMulti (Multi x y) =
    printExpr x <> text "*" <> printExpr y

-- / convert Expr to Doc
printExpr :: Expr -> Doc
printExpr (Nat x) = printNat (Nat x)
printExpr (Add x y) = printAdd (Add x y)
printExpr (Multi x y) = printMulti (Multi x y)
