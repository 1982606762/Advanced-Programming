-- Do not modify anything in this file!
module Definitions where

type ErrMsg = String    -- human-readable error messages
type EM = Either ErrMsg -- can be used directly as a Monad

type EGrammar = [ERule]   -- Spec ::= ERules
type ERule = (RLHS, ERHS)   -- ERule ::= LHS "::=" Alts "."

type RLHS = (NTName, RKind, Maybe Type)  
--左边部分，LHS ::= name OptType | "_"，
-- name 是NTName，OptType 是Maybe Type,RKind 是三种之一（RPlain，RSeq，RList）
-- OptType ::= | "{:" htext "}".

type NTName = String -- non-terminal name

data ERHS =       
    ESimple Simple
  | ESeq [ERHS] HText
  | EBar ERHS ERHS        --Alts ::= Seq | Seq "|" Alts.
  | EOption ERHS
  | EMany ERHS
  | EPred ERHS HText
  | ENot ERHS
  deriving (Eq, Show, Read)

data Simple =
    SLit String
  | SNTerm String
  | SAnyChar
  | SChar Char
  | SNot Simple
  | SPred Simple HText
  | SDummy                 -- 1.2.2用到
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
