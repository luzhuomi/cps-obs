{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Language.C.Obfuscate.CPS
       where

import qualified Language.C.Syntax.AST as AST
import qualified Language.C.Data.Node as N 
import Language.C.Syntax.Constants
import Language.C.Data.Ident

import Language.C.Obfuscate.CFG 
import Language.C.Obfuscate.SSA


-- import for testing
import Language.C (parseCFile, parseCFilePre)
import Language.C.System.GCC (newGCC)
import Language.C.Pretty (pretty)
import Text.PrettyPrint.HughesPJ (render, text, (<+>), hsep)

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

-- was imported from SSA

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

-- ^ a CPS function declaration AST
data CPS = CPS { cps_decls :: [AST.CDeclaration N.NodeInfo]  -- ^ main function decls
               , cps_stmts :: [AST.CCompoundBlockItem N.NodeInfo]  -- ^ main function stmts
               , cps_funcs :: [AST.CFunctionDef N.NodeInfo] -- ^ generated auxillary functions
               } deriving Show
                          
                          
                          
{-
top level translation   p => P

t => T    x => X     ti => Ti     xi => Xi     di => Di
\bar{b} |- \bar{\Delta}    id, \bar{\Delta} |- \bar{b} => \bar{P}
P1 = void f1 (void => void k) { B1 } 
-------------------------------------------------------------------------
|- t x (\bar{t x}) {\bar{d};\bar{b}}  => 
         T X (\bar{T X}) {\bar{D}; T rx; \bar{P}; f1(id); return rx; }
-}
                          
ssa2cps :: SSA -> CPS 
ssa2cps = undefined


class CPSize ssa cps where
  cps_trans :: ssa -> cps 
  
instance CPSize a b => CPSize [a] [b] where
  cps_trans as = map cps_trans as
  
  
instance CPSize (Ident, LabeledBlock) (AST.CFunctionDef N.NodeInfo) where  
  cps_trans (label, lb) = undefined
  
  
  
{-  
translation     d => D
t => T     x => X
----------------------
t x => T X
-}
instance CPSize (AST.CDeclaration N.NodeInfo) (AST.CDeclaration N.NodeInfo) where
  cps_trans decl = case decl of 
    { AST.CDecl tyspec tripls nodeInfo -> 
         let tyspec' = cps_trans tyspec
             tripls' = map (\(mb_decltr, mb_init, mb_size) -> 
                             let mb_decltr' = case mb_decltr of 
                                   { Nothing     -> Nothing 
                                   ; Just decltr -> Just decltr -- todo
                                   }
                                 mb_init' = case mb_init of 
                                   { Nothing     -> Nothing
                                   ; Just init   -> Just init   -- todo
                                   }
                                 mb_size' = case mb_size of
                                   { Nothing     -> Nothing
                                   ; Just size   -> Just size   -- todo
                                   }
                             in (mb_decltr', mb_init', mb_size')                
                             ) tripls
         in  AST.CDecl tyspec' tripls' nodeInfo
    -- ; AST.CStaticAssert e lit nodeInfo -> undefined
    }
    
{-                   
---------
t => T
-}
                   
instance CPSize (AST.CDeclarationSpecifier N.NodeInfo) (AST.CDeclarationSpecifier N.NodeInfo) where
  cps_trans tyspec = tyspec -- todo: unroll it
{-
data CDeclarationSpecifier a
  = CStorageSpec (CStorageSpecifier a) -- ^ storage-class specifier or typedef
  | CTypeSpec    (CTypeSpecifier a)    -- ^ type name
  | CTypeQual    (CTypeQualifier a)    -- ^ type qualifier
  | CFunSpec     (CFunctionSpecifier a) -- ^ function specifier
  | CAlignSpec   (CAlignmentSpecifier a) -- ^ alignment specifier
    deriving (Show, Data,Typeable {-! ,CNode ,Functor, Annotated !-})
-}
  
  
{-  
---------
x => X
-}



