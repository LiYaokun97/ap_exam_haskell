Spec ::= preamble ERules.
ERules ::= ERule | ERule ERules.
ERule ::= LHS "::=" Alts ".".
LHS ::= name OptType| name | "_".
OptType ::= "{:" htext "}".


Alts ::= 
        Seq |
        Seq "|" Alts.


Seq ::= Simple | Simplez "{" htext "}".  (原来)

Simplez ::= | Simple Simplez.   (原来)


Simple ::= 
        Simple0 |
        Simple0 "?" |                 可选从这里解析出来
        Simple0 "*" |                 可重复从这里解析出来
         "!" Simple0.                 negated parsers 从这里解析出来

Simple0 ::= Atom Simple0Helper.

Simple0Helper = |"{?" htext "}" Simple0Helper.        preicate parsers从这里解析出来

Atom ::= 
        name |
        tokLit |   
        "@" |                single-character parser                   
        charLit | 
        "(" Alts ")".                 Nested choices















S ::= S \"a\" {_1+1} | \"b\" {0}.\n.

[ 
                ( 
                    ("S",RPlain,Nothing), 
                    ESeq [ 
                        ESeq [
                            ESimple (SNTerm "S"), ESimple (SLit "a") 
                            ] 
                        "_1+1" ,
                        ESeq [ESimple (SLit "b")] "0"
                    ] 	
                    "{()}" 
                ) 
            ]
        ),  	




[
    (
        ("", 
            [ 
                ( 
                    ("S",RPlain,Nothing), 
                    ESeq [ 
                        ESeq [
                            ESimple (SNTerm "S"), ESimple (SLit "a") 
                            ] 
                        "_1+1" ,
                        ESeq [ESimple (SLit "b")] "0"
                    ] 	
                    "{()}" 
                ) 
            ]
        ),  		"_ ::= {()}." 
    ) 	
]



    
(
    ("S",RPlain,Nothing) , 
    ESeq [
        ESeq [ ESimple (SNTerm "S") , ESimple (SLit "a")] "_1+1",
        ESeq [ ESimple (SLit "b")] "0"
        ]
    "{()}" 
)
], "_ ::= {()}.") , 

( [ ( 
    ("S",RPlain,Nothing),
    ESeq [
        ESeq [ESimple (SNTerm "S"),ESimple (SLit "a")] "_1+1",
        ESeq [ESimple (SLit "b")] "0"
        ] 
    "{()}"
),
(("_",RSep,Nothing),ESeq [] "()"
)  
    



    T ::="(" E ")"       {_2}." 

 [
    (
        (\"\",
            [ 
                ( 
                    (\"T\",RPlain,Nothing),
                    ESeq [ESimple (SLit \"(\"),ESimple (SNTerm \"E\"),ESimple (SLit \")\")] \"_2\"
                )
            ]
        ),
        \"\"
    ),
    
    (   (\"\",
            [
                (
                    (\"T\",RPlain,Nothing),
                    ESeq [ESimple (SLit \"(\\\" E \\\")\")] \"_2\"
                )
            ]
        ),
        \"\"
    )
]

[((\"\",[((\"T\",RPlain,Nothing),ESeq [ESimple (SLit \"(\"),ESimple (SNTerm \"E\"),ESimple (SLit \")\")] \"_2\")]),\"\"),((\"\",[((\"T\",RPlain,Nothing),ESeq [ESimple (SLit \"(\\\" E \\\")\")] \"_2\")]),\"\")]"


" \"(\" E \")\"       {_2}."     
SLit "(\" E \")"
SLit "("), ESimple (SNTerm "E"), ESimple (SLit ")"

[
    (
        ("",
            [
                (
                    ("T",RPlain,Nothing),
                    ESeq [ESimple (SLit "("),ESimple (SNTerm "E"),ESimple (SLit ")")] "_2"
                )
            ]
        ), ""),

    (
        ("",
            [
                (
                    ("T",RPlain,Nothing),
                    ESeq [ESimple (SLit "(\" E \")")] "_2"
                )
            ]
        ),""
    )
]