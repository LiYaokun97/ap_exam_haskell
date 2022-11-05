-- Put yor parser implementation in this file
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module ParserImpl where

import Definitions
import Text.ParserCombinators.ReadP

import Data.Char
import qualified Data.Map as Map
import GHC.Conc (pseq)
import Data.Functor.Contravariant (phantom)

type ParseError = String -- you may replace this
type Parser a = ReadP a

parseSpec :: String -> EM (String, EGrammar)
parseSpec program = case readP_to_S pSpec program of
                   [] -> Left "empty parsing!!!"
                   x -> case x of
                           [ (e, "")] -> Right e
                           ls -> Left ("error! and the current parsing result is : \n" ++ show ls)

-- Spec ::= preamble ERules.
pSpec :: Parser (String, EGrammar)
pSpec = do
        s <- pPreamble
        erules <- pERules
        return (s, erules)

{-
    if there is preamble then parse it, otherwise return empty string as preamble
-}
pPreamble :: Parser String
pPreamble = do 
        manyTill get (string "---")
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
        htext <- pHtext False -- todo: 加上pHtext的限制
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
        skipSpaceAndComment
        pHtextGetString [c]
    else do 
        skipSpaceAndComment
        pHtextGetString ""

pHtextGetChar :: Parser Char
pHtextGetChar = do
            string "{{"
            return '{'
    <++
        do
            string "}}"
            return '}'
    <++ do  get


pHtextGetString :: String -> Parser String
pHtextGetString s = do
            c <- pHtextGetChar
            pHtextGetString (s ++ [c])
    <++ do  return s


-- Alts ::= Seq |Seq "|" Alts.
pAlts :: Parser ERHS
pAlts = do
            pSeq
    +++
        do
            seq <- pSeq
            string "|"
            skipSpaceAndComment
            alts <- pAlts
            return (ESeq [seq, alts] "{()}")

-- Seq ::= Simple | Simplez "{" htext "}" | "{" htext "}".
pSeq :: Parser ERHS
pSeq = do
            pSimple
    +++
        do
            simplez <- pSimplez
            string "{"
            htext <- pHtext True
            string "}"
            skipSpaceAndComment
            return $ ESeq [simplez] htext
    +++
        do
            string "{"
            htext <- pHtext True
            string "}"
            skipSpaceAndComment
            return $ ESeq [] htext

-- Simplez ::=  Simple Simplez | Simple.
pSimplez :: Parser ERHS
pSimplez = do
            simple <- pSimple
            simplez <- pSimplez
            skipSpaceAndComment
            return $ ESeq [simple, simplez] "{()}"
    +++
        do
            pSimple

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


-- Simple0 ::=  Atom | Simple0 "{?" htext "}".  
pSimple0 :: Parser ERHS
pSimple0 = do
            pAtom
    +++
        do
            simple0 <- pSimple0
            string "{?"
            htext <- pHtext False
            string "}"
            skipSpaceAndComment
            return $ EPred simple0 htext

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
pTokLitString = do between (char '"') (char '"') pTokLitStringHelper

pTokLitStringHelper :: Parser String
pTokLitStringHelper = do
            x <- getOneChar
            getMoreChar x

getOneChar :: Parser String
getOneChar = do
                string "\"\""
                return "\""
        <++ do
                a <- satisfy isPrint
                return [a]

getMoreChar :: String -> Parser String
getMoreChar s = do
                x <- getOneChar
                getMoreChar (s ++ x)
        <++ do  return s


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

isComment :: String -> Bool
isComment s = length s >= 2 && head s == '-' && head (tail s) == '-'
