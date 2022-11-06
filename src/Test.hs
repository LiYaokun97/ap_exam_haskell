
module Test where

import Definitions
import Text.ParserCombinators.ReadP

import Data.Char
import qualified Data.Map as Map
import GHC.Conc (pseq)
import Data.Functor.Contravariant (phantom)
import ParserImpl

type ParseError = String -- you may replace this
type Parser a = ReadP a


test_calc :: IO ()
test_calc = test_file "../examples/calc.appy"

test_boa :: IO ()
test_boa = test_file "../examples/boa.appy"

test_small_boa :: IO ()
test_small_boa = test_file "../examples/small_boa.appy"


test_file :: String -> IO ()
test_file file = do
                s <- myReadFile file
                case parseSpec s of
                    Left err -> putStrLn err
                    Right (s, grammar) -> putStrLn $ "the preamble is : \n " ++ s ++ "\nthe grammar is : \n" ++ show grammar


-- test2 :: Parser String
-- test2 = do 
--     manyTill get (string "---")

calcInputString :: String
calcInputString =
    "module WarmupReadP where\n " ++
    "import qualified Text.ParserCombinators.ReadP as RP\n" ++
    "import Data.Char (isDigit, isSpace, digitToInt)\n" ++
    "\n" ++
    "type ParseError = String\n"++
    "\n" ++
    "data Exp = Num Int | Negate Exp | Add Exp Exp\n" ++
    "  deriving (Eq, Show)\n" ++
    "\n" ++
    "parseString :: String -> Either ParseError Exp\n" ++
    "parseString = parseTop p_E\n" ++
    "\n" ++
    "---\n" ++
    "\n" ++
    "E ::=\n" ++
    "  E \"+\" T         {Add _1 _3}\n" ++
    "| E \"-\" T         {Add _1 (Negate _3)}\n" ++
    "| T               {_1}\n" ++
    "| \"-\" T           {Negate _2}.\n" ++
    "\n" ++
    "T ::=\n" ++
    "  num             {_1}\n" ++
    "| \"(\" E \")\"       {_2}.\n" ++
    "\n" ++
    "num ::= Digits    {Num _1}.\n" ++
    "\n" ++
    "Digits ::=\n" ++
    "  Digits Digit    {10*_1 + _2}\n" ++
    "| Digit           {_1}.\n" ++
    "\n" ++
    "-- or: Digit ::= '0' {0} | '1' {1} | '2' {2} | '3' {3} | ... | '9' {9}\n" ++
    "Digit ::= @{?isDigit} {digitToInt _1}.\n" ++
    "\n" ++
    "Whitespace ::= {()} | @{?isSpace} Whitespace {()}.\n" ++
    "\n" ++
    "_ ::= Whitespace.\n"


myReadFile :: FilePath -> IO String
myReadFile file = readFile file