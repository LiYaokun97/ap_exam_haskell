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
  testCase "Parser_testEBar1" $ parseSpec str2 @?= Right ("", result2),
  testCase "Parser_testEBar2" $ parseSpec str3 @?= Right ("", result3),
  testCase "Parser_testESeq1" $ parseSpec str4 @?= Right ("", result4),
  testCase "Parser_testESeq2" $ parseSpec str5 @?= Right ("", result5),
  testCase "Parser_testEOption" $ parseSpec str6 @?= Right ("", result6),
  testCase "Parser_testEOption2" $ parseSpec str7 @?= Right ("", result7),
  testCase "Parser_testEMany1" $ parseSpec str8 @?= Right ("", result8),
  testCase "Parser_testEMany2" $ parseSpec str9 @?= Right ("", result9),
  testCase "Parser_testEPred" $ parseSpec str10 @?= Right ("", result10),
  testCase "Parser_testENot" $ parseSpec str11 @?= Right ("", result11),
  testCase "Parser_testComment" $ parseSpec str12 @?= Right ("", result12),
  testCase "Parser_testComment2" $ parseSpec str13 @?= Right ("", result13),
  
  testCase "Transformer.convert" $
    convert eg @?= Right g] -- assumes that convert preserves input rule order
  where
    str = "---\n S ::= S \"a\" {_1+1} | \"b\" {0}.\n _ ::= {()}."
    str2 = "---\n Stmts {:[Stmt]} ::= Stmt         {[_1]}\n| Stmt \";\" Stmts  {_1 : _3}."
    result2 = [(("Stmts",RPlain,Just (AUser "[Stmt]")),EBar (ESeq [ESimple (SNTerm "Stmt")] "[_1]") (ESeq [ESimple (SNTerm "Stmt"),ESimple (SLit ";"),ESimple (SNTerm "Stmts")] "_1 : _3"))]
    str3 = "---\n Stmts {:[Stmt]} ::= Abcd         {[_1]}\n| Stmt \";\" Stmts  {_1 : _3}."
    result3 = [(("Stmts",RPlain,Just (AUser "[Stmt]")),EBar (ESeq [ESimple (SNTerm "Abcd")] "[_1]") (ESeq [ESimple (SNTerm "Stmt"),ESimple (SLit ";"),ESimple (SNTerm "Stmts")] "_1 : _3"))]

    str4 = "Whitespace {:()} ::= WSElt* !WSElt {()}.\n WSElt {:()} ::=\n  @{?isSpace} {()}\n| '#' Comment {()}.\n"
    result4 = [(("Whitespace",RPlain,Just (AUser "()")),ESeq [EMany (ESimple (SNTerm "WSElt")),ENot (ESimple (SNTerm "WSElt"))] "()"),(("WSElt",RPlain,Just (AUser "()")),EBar (ESeq [EPred (ESimple SAnyChar) "isSpace"] "()") (ESeq [ESimple (SChar '#'),ESimple (SNTerm "Comment")] "()"))]

    str5 = "Whitespace {:()} ::= XWSElt* !WSElt {()}.\n WSElt {:()} ::=\n  @{?isSpace} {()}\n| '#' Comment {()}.\n"
    result5 = [(("Whitespace",RPlain,Just (AUser "()")),ESeq [EMany (ESimple (SNTerm "XWSElt")),ENot (ESimple (SNTerm "WSElt"))] "()"),(("WSElt",RPlain,Just (AUser "()")),EBar (ESeq [EPred (ESimple SAnyChar) "isSpace"] "()") (ESeq [ESimple (SChar '#'),ESimple (SNTerm "Comment")] "()"))]

    str6 = "---\nWhitespace {:()} ::= KU? University {_1 _2} | Shanghai {_1}."    
    result6 = [(("Whitespace",RPlain,Just (AUser "()")),EBar (ESeq [EOption (ESimple (SNTerm "KU")),ESimple (SNTerm "University")] "_1 _2") (ESeq [ESimple (SNTerm "Shanghai")] "_1"))]

    str7 = "---\nWhitespace {:()} ::= ShanghaiTech? University {_1 _2} | Shanghai {_1}."    
    result7 = [(("Whitespace",RPlain,Just (AUser "()")),EBar (ESeq [EOption (ESimple (SNTerm "ShanghaiTech")),ESimple (SNTerm "University")] "_1 _2") (ESeq [ESimple (SNTerm "Shanghai")] "_1"))]

    str8 = "---\nWhitespace {:()} ::= KU* University {_1 _2} | Shanghai {_1}." 
    result8 = [(("Whitespace",RPlain,Just (AUser "()")),EBar (ESeq [EMany (ESimple (SNTerm "KU")),ESimple (SNTerm "University")] "_1 _2") (ESeq [ESimple (SNTerm "Shanghai")] "_1"))]

    str9 = "---\nWhitespace {:()} ::= KU* niversity {_1 _2}."
    result9 =  [(("Whitespace",RPlain,Just (AUser "()")),ESeq [EMany (ESimple (SNTerm "KU")),ESimple (SNTerm "niversity")] "_1 _2")]

    str10 = "---\nNLZDigits {:String} ::= '0'                           {[_1]}| Digit{?/='0'} Digit* !Digit   {_1:_2}."
    result10 =  [(("NLZDigits",RPlain,Just (AUser "String")),EBar (ESeq [ESimple (SChar '0')] "[_1]") (ESeq [EPred (ESimple (SNTerm "Digit")) "/='0'",EMany (ESimple (SNTerm "Digit")),ENot (ESimple (SNTerm "Digit"))] "_1:_2"))]
    
    str11 = "---\nIdent1 {:String} ::=IdChar{?not.isDigit} IdChar* !IdChar{_1 : _2}."    
    result11 =  [(("Ident1",RPlain,Just (AUser "String")),ESeq [EPred (ESimple (SNTerm "IdChar")) "not.isDigit",EMany (ESimple (SNTerm "IdChar")),ENot (ESimple (SNTerm "IdChar"))] "_1 : _2")]    

    str12 =   "---\nIdent1 {:String} --comment\n ::=IdChar{?not.isDigit} IdChar* !IdChar{_1 : _2}.\n-- comment "
    result12 =  [(("Ident1",RPlain,Just (AUser "String")),ESeq [EPred (ESimple (SNTerm "IdChar")) "not.isDigit",EMany (ESimple (SNTerm "IdChar")),ENot (ESimple (SNTerm "IdChar"))] "_1 : _2")]

    str13 =   "---\n \n \n \n Ident1 {:String} --comment\n ::=IdChar{?not.isDigit} IdChar* !IdChar{_1 : _2}.\n-- comment "
    result13 =  [(("Ident1",RPlain,Just (AUser "String")),ESeq [EPred (ESimple (SNTerm "IdChar")) "not.isDigit",EMany (ESimple (SNTerm "IdChar")),ENot (ESimple (SNTerm "IdChar"))] "_1 : _2")]

    eg = [(("S", RPlain, Nothing),
           EBar (ESeq [ESimple (SNTerm "S"), ESimple (SLit "a")] "_1+1")
                      (ESeq [ESimple (SLit "b")] "0")),
          (("_", RSep, Nothing), ESeq [] ("()"))]
    g = [(("S", RPlain, Nothing),
          [([SNTerm "S", SLit "a"], AUser "_1+1"),
           ([SLit "b"], AUser "0")]),
         (("_", RSep, Nothing), [([], AUser "()")])]

