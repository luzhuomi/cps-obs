module Language.C.Obfuscate.CPS
       where


{- Source Language SSA

(Prog)  p ::= t x (\bar{t x}) {\bar{d}; \bar{b} }

(Decl)  d ::= t x

(Block) b ::= l: {s} | l: {\bar{i} ; s} 

(Stmts) s ::= x = e; s | goto l; | return e; | e; s | if e { s } else { s } 

(Phi)   i ::= x = \phi(\bar{g})

(Exp)   e ::= v | e (\bar{e})

(Labelled argument) g ::= (l:e)

(Label) l ::= l0 | ... | ln

(Value) v ::= x | c

(Type)  t ::= int | bool | t* | t[] | | void

(Loop environment) \Delta ::= (l_if, e_cond, l_true, l_false)
-}

{- Target Language CPS
(Prog)  P ::= T X (\bar{T X}) {\bar{D}; \bar{B} }

(Block) B ::= S

(Decl)  D ::= T X | P /* nested functions */

(Stmts)  S ::= X = E ; S | return E; | E; S |  if E { S } else { S } 

(Exp) E ::=  V | E(\bar{E})

(Value) V :: = X | C | (\bar{T X}):T => {\bar{D}; \bar{B}}

(Variable) X ::= x | y | k | ...

(Type) T :: = int | bool | T * | T => T | T[] | void
-}


