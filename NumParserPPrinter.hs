module NumParserPPrinter where

import Text.PrettyPrint
import DoNumericParser

printNat (Nat x) = int x

printAdd (Add (Nat x) (Nat y)) =
    int x <> text "+" <> int y
printAdd (Add (Nat x) (Add y z)) =
    int x <> text "+(" <> printExpr (Add y z) <> text ")"
printAdd (Add (Nat x) (Multi y z)) =
    int x <> text "+" <> printExpr (Multi y z)
printAdd (Add (Add x y) (Nat z)) =
    text "(" <> printExpr (Add x y) <> text ")+" <> int z
printAdd (Add (Add w x) (Add y z)) =
    text "(" <> printExpr (Add w x) <> text ")+(" <> printExpr (Add y z) <> text ")"
printAdd (Add (Add w x) (Multi y z)) =
    text "(" <> printExpr (Add w x) <> text ")+" <> printExpr (Multi y z)
printAdd (Add (Multi x y) (Nat z)) =
    printExpr (Multi x y) <> text "+" <> int z
printAdd (Add (Multi w x) (Add y z)) =
    printExpr (Multi w x) <> text "+(" <> printExpr (Add y z) <> text ")"
printAdd (Add (Multi w x) (Multi y z)) =
    printExpr (Multi w x) <> text "+" <> printExpr (Multi y z)

printMulti (Multi x@(Add _ _) y@(Add _ _)) =
    text "(" <> printExpr x <> text ")*(" <> printExpr y <> text ")"
printMulti (Multi x@(Add _ _) y) =
    text "(" <> printExpr x <> text ")*" <> printExpr y
printMulti (Multi x y@(Add _ _)) =
    printExpr x <> text "*(" <> printExpr y <> text ")"
printMulti (Multi x y) =
    printExpr x <> text "*" <> printExpr y

printExpr (Nat x) = printNat (Nat x)
printExpr (Add x y) = printAdd (Add x y)
printExpr (Multi x y) = (printExpr x) <> text "*" <> (printExpr y)
