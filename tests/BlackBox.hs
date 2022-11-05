-- Sample black-box test suite. Feel free to adapt, or start from scratch.

-- Do NOT import from your ModImpl files here. These tests should work with
-- any implementation of the APpy APIs. Put any white-box tests in
-- suite1/WhiteBox.hs.
import Definitions
import Parser
import Transformer

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Smoke tests" [
  testCase "Parser" $ parseSpec str @?= Right ("", eg),
  testCase "Transformer.convert" $
    convert eg @?= Right g] -- assumes that convert preserves input rule order
  where
    str = "---\n S ::= S \"a\" {_1+1} | \"b\" {0}.\n _ ::= {()}."
    eg = [(("S", RPlain, Nothing),
           EBar (ESeq [ESimple (SNTerm "S"), ESimple (SLit "a")] "_1+1")
                      (ESeq [ESimple (SLit "b")] "0")),
          (("_", RSep, Nothing), ESeq [] ("()"))]
    g = [(("S", RPlain, Nothing),
          [([SNTerm "S", SLit "a"], AUser "_1+1"),
           ([SLit "b"], AUser "0")]),
         (("_", RSep, Nothing), [([], AUser "()")])]

