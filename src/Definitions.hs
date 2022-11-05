-- Do not modify anything in this file!
module Definitions where

type ErrMsg = String    -- human-readable error messages
type EM = Either ErrMsg -- can be used directly as a Monad

type EGrammar = [ERule]
type ERule = (RLHS, ERHS)

type RLHS = (NTName, RKind, Maybe Type)
type NTName = String

data ERHS =
    ESimple Simple
  | ESeq [ERHS] HText    {- Simplez "{" htext "}".-}
  | EBar ERHS ERHS        {- Nested choices -}
  | EOption ERHS          {- Simple0 "?" -}
  | EMany ERHS            {- Simple0 "*" -}
  | EPred ERHS HText      {- "{?" htext "}" Simple1 -}
  | ENot ERHS             {-"!" Simple0. -}
  deriving (Eq, Show, Read) 

data Simple =
    SLit String           {- tokLit -}
  | SNTerm String         {- name -}
  | SAnyChar              {- "@" -}
  | SChar Char            {- charLit -}
  | SNot Simple           {- 大概率是not followed by -}
  | SPred Simple HText    {- 1.preicate parsers  2. "@"    Digit ::= @{?isDigit}.  -}
  | SDummy                {- transformer中使用 -}
  deriving (Eq,Show,Read)

data RKind = RPlain | RToken | RSep
  deriving (Eq,Show,Read)


data Action =
    AUser HText
  | AVar String
  | ALam String Action
  | AApp Action Action
  | ACst String
  deriving (Eq,Show,Read)

type HText = String   -- Haskell text from user
type Type = Action


type Grammar = [Rule]
type Rule = (RLHS, [([Simple]{-seq-}, Action)]{-alts-})
