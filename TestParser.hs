module TestParser where

import Test.QuickCheck (Arbitrary(..), oneof, choose, frequency)
import Control.Monad
import Control.Applicative

import Text.PrettyPrint
import Text.Parsec (parse)
import qualified DoNumericParser as NumParser
import qualified PPrinter as PP

checkParser :: NumParser.Expr -> Bool
checkParser expression =
    case (parse NumParser.expr "" (render $ PP.printExpr expression)) of
        Left _ -> False
        Right a -> expression == a
    where types = expression :: NumParser.Expr

instance Arbitrary NumParser.Expr where
    arbitrary = frequency
        [ (10, NumParser.Nat <$> natGenerator)
        , (1, liftM2 NumParser.Add arbitrary arbitrary)
        , (1, liftM2 NumParser.Multi arbitrary arbitrary)
        ]

natGenerator = choose (0, 9)
