-- Put yor parser implementation in this file
module ParserImpl where

import Definitions
import Text.ParserCombinators.ReadP

import Data.Char
import qualified Data.Map as Map
import GHC.Conc (pseq)
import Data.Functor.Contravariant (phantom)

type ParseError = String -- you may replace this
type Parser a = ReadP a
parseString :: String -> Either ParseError EGrammar
parseString s = case readP_to_S pERules s of
                   [] -> Left "empty parsing!!!"
                   x -> case x of
                           [ (e, "")] -> Right e
                           ls -> Left (show ls)


-- ERules ::= ERule | ERule ERules.
pERules :: Parser [ERule]
pERules = do
            rule <- pERule
            return [rule]
    <++
        do
            rule <- pERule
            rules <- pERules
            return $ rule:rules

-- ERule ::= LHS "::=" Alts ".".
pERule :: Parser ERule
pERule = do
            lhs <- pLHS
            string "::="
            alts <- pAlts
            string "."
            return (lhs, alts)

-- LHS ::= name OptType | name | "_".
pLHS :: Parser (NTName, RKind, Maybe Type)
pLHS =  do
            string "_"
            return ("_", getLHSKind "_", Nothing)
    <++
        do
            (SNTerm name) <- pName
            return (name, getLHSKind name, Nothing)
    <++
        do
            (SNTerm name) <- pName
            optType <-pOptType
            return (name, RToken, Just $ AUser optType)

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
        htext <- pHtext  -- todo: 加上pHtext的限制
        string "}"
        return htext


pHtext :: Parser HText
pHtext = undefined

-- Alts ::= Seq |Seq "|" Alts.
pAlts :: Parser ERHS
pAlts = do
            pSeq
    <++
        do
            seq <- pSeq
            string "|"
            alts <- pAlts
            return (ESeq [seq, alts] "{()}")

-- Seq ::= Simple | Simplez "{" htext "}" | "{" htext "}".
pSeq :: Parser ERHS
pSeq = do
            pSimple
    <++
        do
            simplez <- pSimplez
            string "{"
            htext <- pHtext
            string "}"
            return $ ESeq [simplez] htext
    <++
        do
            string "{"
            htext <- pHtext
            string "}"
            return $ ESeq [] htext

-- Simplez ::=  Simple Simplez | Simple.
pSimplez :: Parser ERHS
pSimplez = do
            simple <- pSimple
            simplez <- pSimplez
            return $ ESeq [simple, simplez] "{()}"
    <++
        do
            pSimple

-- Simple ::=  Simple0 | Simple0 "?" | Simple0 "*" | "!" Simple0.  
pSimple:: Parser ERHS
pSimple = do
            pSimple0
    <++
        do
            simple0 <- pSimple0
            string "?"
            return $ EOption simple0
    <++
        do
            simple0 <- pSimple0
            string "*"
            return $ EMany simple0
    <++
        do
            string "!"
            ENot <$> pSimple0


-- Simple0 ::=  Atom | Simple0 "{?" htext "}".  
pSimple0 :: Parser ERHS
pSimple0 = do
            pAtom
    <++
        do
            simple0 <- pSimple0
            string "{?"
            htext <- pHtext
            string "}"
            return $ EPred simple0 htext

-- Atom ::= name | tokLit | "@" | charLit | "(" Alts ")".
pAtom :: Parser ERHS
pAtom = do
            ESimple <$> pName
    <++
        do
            ESimple <$> pTokLit
    <++
        do
            string "@"
            return $ ESimple SAnyChar
    <++
        do
            ESimple <$> pCharLit
    <++
        do
            string "("
            alts <- pAlts
            string ")"
            return alts

-- Any printable character (including '), enclosed in single-quotes.
pCharLit :: Parser Simple
pCharLit = do SChar <$> pCharLitString 

pCharLitString :: Parser Char
pCharLitString = do between (char '\'') (char '\'') get

-- Any non-empty sequence of printable characters, enclosed in double-quotes. If the
-- sequence is itself to contain a double-quote, it must be written as "".
pTokLit :: Parser Simple
pTokLit = do SLit <$> pTokLitString

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
                return (s ++ x)
        <++ do  return s


-- Name: Any sequence of (Unicode) letters, digits, and underscores, starting with a letter.
-- There are no reserved names.
pName :: Parser Simple
pName = do SNTerm <$> pNameString

pNameString :: Parser String
pNameString = do
            a <- satisfy isLetter
            as <- munch (\x -> (x =='_' || isLetter x || isDigit x) && isPrint x)
            return $ a:as


-- pPreamble :: Parser String
-- pPreamble = undefined

