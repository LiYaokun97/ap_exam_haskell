
module Test where

import Definitions
import Text.ParserCombinators.ReadP

import Data.Char
import qualified Data.Map as Map
import GHC.Conc (pseq)
import Data.Functor.Contravariant (phantom)

type ParseError = String -- you may replace this
type Parser a = ReadP a

test s = case s of
  x:_ | isLower x -> True
  _ -> False


test2 :: Parser String
test2 = do 
    manyTill get (string "---")

