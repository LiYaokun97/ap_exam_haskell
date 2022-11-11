-- Put yor parser implementation in this file
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module ParserImpl (parseSpec) where 
 
import Definitions
import Text.ParserCombinators.ReadP

import Data.Char
import qualified Data.Map as Map
import GHC.Conc (pseq)
import Data.Functor.Contravariant (phantom)

type ParseError = String 
type Parser a = ReadP a

parseSpec :: String -> EM (String, EGrammar)
parseSpec program = case readP_to_S pSpec program of
                   [] -> Left "empty parsing!!!"
                   x -> case x of
                           [ (e, "")] -> Right e
                           ls -> Left ("error! and the current parsing result is : " ++ show ls)

-- Spec ::= preamble ERules.
pSpec :: Parser (String, EGrammar)
pSpec = do
        s <- pPreamble
        skipSpaceAndComment
        erules <- pERules
        eof
        return (s, erules)

{-
    if there is preamble then parse it, otherwise return empty string as preamble
-}
pPreamble :: Parser String
pPreamble = do
        manyTill get (string "---\n")
    <++
        return ""

-- ERules ::= ERule | ERule ERules.
pERules :: Parser [ERule]
pERules = do
            rule <- pERule
            return [rule]
    +++
        do
            rule <- pERule
            rules <- pERules
            return $ rule:rules

-- ERule ::= LHS "::=" Alts ".".
pERule :: Parser ERule
pERule = do
            lhs <- pLHS
            string "::="
            skipSpaceAndComment
            alts <- pAlts
            string "."
            skipSpaceAndComment
            return (lhs, alts)

-- LHS ::= name OptType | name | "_".
pLHS :: Parser (NTName, RKind, Maybe Type)
pLHS =  do
            x <- string "_"
            skipSpaceAndComment
            return ("_", getLHSKind x, Nothing)
    +++
        do
            (SNTerm name) <- pName
            return (name, getLHSKind name, Nothing)
    +++
        do
            (SNTerm name) <- pName
            optType <- pOptType
            return (name, getLHSKind name, Just $ AUser optType)

-- get corresponding RKind according the name of LHS
getLHSKind :: String -> RKind
getLHSKind lhsName = case lhsName of
        "_" -> RSep
        x:_ | isLower x-> RToken
        _ -> RPlain


-- OptType ::= "{:" htext "}".
pOptType :: Parser HText
pOptType = do
        string "{:"
        skipSpaceAndComment
        htext <- pHtext False
        string "}"
        skipSpaceAndComment
        return htext

{-
    Any sequence of arbitrary characters, including any leading whitespace. Characters
    { and } to be included in the sequence must be written as {{ and }}, respectively.
    When following an opening {, the htext must not start with a : or ?.

    Bool : Whether the htext is following an opening {
-}
pHtext :: Bool -> Parser HText
pHtext isFollowPara = if isFollowPara
    then do
        c <- satisfy (\x -> x /= ':' && x /= '?')
        s <- pHtextGetString [c]
        skipSpaceAndComment
        return s
    else do
        s <- pHtextGetString ""
        skipSpaceAndComment
        return s

pHtextGetChar :: Parser Char
pHtextGetChar = do
            string "{{"
            return '{'
    <++
        do
            string "}}"
            return '}'
    <++
        do
            s <- look
            if head s == '{' || head s == '}' then
                return pfail "{ or } should be doubled in htext"
            else
                get

pHtextGetString :: String -> Parser String
pHtextGetString s = do
            c <- pHtextGetChar
            pHtextGetString (s ++ [c])
    <++ do  return s


-- Alts ::= Seq |Seq "|" Alts.
pAlts :: Parser ERHS
pAlts = do
            seq <- pSeq
            string "|"
            skipSpaceAndComment
            EBar seq <$> pAlts
    +++
        do
            pSeq

-- Seq ::= Simple | Simplez "{" htext "}". 
pSeq :: Parser ERHS
pSeq = do
            pSimple
    +++
        do
            simplez <- pSimplez
            string "{"
            skipSpaceAndComment
            htext <- pHtext True
            string "}"
            skipSpaceAndComment
            return $ ESeq simplez htext

-- Simplez ::= | Simple Simplez.
pSimplez :: Parser [ERHS]
pSimplez = do
            many pSimple

-- Simple ::=  Simple0 | Simple0 "?" | Simple0 "*" | "!" Simple0.  
pSimple:: Parser ERHS
pSimple = do
            pSimple0
    +++
        do
            simple0 <- pSimple0
            string "?"
            skipSpaceAndComment
            return $ EOption simple0
    +++
        do
            simple0 <- pSimple0
            string "*"
            skipSpaceAndComment
            return $ EMany simple0
    +++
        do
            string "!"
            skipSpaceAndComment
            ENot <$> pSimple0

-- Simple0 ::= Atom Simple0Helper.
pSimple0 :: Parser ERHS
pSimple0 = do
            atom <- pAtom
            pSimple0Helper atom

-- Simple0Helper = "{?" htext "}" Simple0Helper.
pSimple0Helper :: ERHS -> Parser ERHS
pSimple0Helper erhs = do
            string "{?"
            skipSpaceAndComment
            htext <- pHtext False
            string "}"
            skipSpaceAndComment
            simple0Helper <- pSimple0Helper erhs
            return $ EPred simple0Helper htext
        +++
            return erhs

-- Atom ::= name | tokLit | "@" | charLit | "(" Alts ")".
pAtom :: Parser ERHS
pAtom = do
            ESimple <$> pName
    +++
        do
            ESimple <$> pTokLit
    +++
        do
            string "@"
            skipSpaceAndComment
            return $ ESimple SAnyChar
    +++
        do
            ESimple <$> pCharLit
    +++
        do
            string "("
            skipSpaceAndComment
            alts <- pAlts
            string ")"
            skipSpaceAndComment
            return alts

-- Any printable character (including '), enclosed in single-quotes.
pCharLit :: Parser Simple
pCharLit = do
            s <-pCharLitString
            skipSpaceAndComment
            return $ SChar s

pCharLitString :: Parser Char
pCharLitString = do between (char '\'') (char '\'') (satisfy isPrint)

-- Any non-empty sequence of printable characters, enclosed in double-quotes. If the
-- sequence is itself to contain a double-quote, it must be written as "".
pTokLit :: Parser Simple
pTokLit = do
            s <- pTokLitString
            skipSpaceAndComment
            return $ SLit s

pTokLitString :: Parser String
pTokLitString = do
            string "\""
            tokLit <- pTokLitStringHelper
            string "\""
            return tokLit

pTokLitStringHelper :: Parser String
pTokLitStringHelper = do
            x <- getOneChar
            getMoreChar x

getOneChar :: Parser String
getOneChar = do
                string "\"\""
                return "\""
        <++
            do
                string "\""
                return pfail "\" in toklit should be double"
        <++ do
                a <- satisfy (\x -> isPrint x && x /= '\"' )
                return [a]

getMoreChar :: String -> Parser String
getMoreChar s = do
                x <- getOneChar
                getMoreChar (s ++ x)
        +++ do  return s


-- Name: Any sequence of (Unicode) letters, digits, and underscores, starting with a letter.
-- There are no reserved names.
pName :: Parser Simple
pName = do
            s <- pNameString
            skipSpaceAndComment
            return $ SNTerm s

pNameString :: Parser String
pNameString = do
            a <- satisfy isLetter
            as <- munch (\x -> (x =='_' || isLetter x || isDigit x) && isPrint x)
            return $ a:as

skipSpaceAndComment :: Parser()
skipSpaceAndComment = do
   skipSpaces
   s <- look
   if isComment s then
      do
         munch1 (/= '\n')
         skipSpaceAndComment
   else return ()
    -- return ()

isComment :: String -> Bool
isComment s = length s >= 2 && head s == '-' && head (tail s) == '-'
