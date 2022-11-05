Spec ::= preamble ERules.
ERules ::= ERule | ERule ERules.
ERule ::= LHS "::=" Alts ".".
LHS ::= name OptType | "_".
OptType ::= | "{:" htext "}".


Alts ::= 
        Seq |
        Seq "|" Alts.


Seq ::= 
        Simple |
        Simplez "{" htext "}".|       semantic action 从这里解析出来
        "{" htext "}".

Simplez ::= 
        Simple Simplez|
        Simple

Simple ::= 
        Simple0 |
        Simple0 "?" |                 可选从这里解析出来
        Simple0 "*" |                 可重复从这里解析出来
         "!" Simple0.                 negated parsers 从这里解析出来

Simple0 ::= 
        Atom |
        Simple0 "{?" htext "}"|         preicate parsers从这里解析出来

Atom ::= 
        name |
        tokLit |   
        "@" |                single-character parser                   
        charLit | 
        "(" Alts ")".                 Nested choices

