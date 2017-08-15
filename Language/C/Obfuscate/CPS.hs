{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Language.C.Obfuscate.CPS
       where
import Data.Char
import qualified Data.Map as M
import qualified Language.C.Syntax.AST as AST
import qualified Language.C.Data.Node as N 
import Language.C.Syntax.Constants
import Language.C.Data.Ident

import Language.C.Obfuscate.Var
import Language.C.Obfuscate.CFG 
import Language.C.Obfuscate.SSA


-- import for testing
import Language.C (parseCFile, parseCFilePre)
import Language.C.System.GCC (newGCC)
import Language.C.Pretty (pretty)
import Text.PrettyPrint.HughesPJ (render, text, (<+>), hsep)



testCPS = do 
  { let opts = []
  ; ast <- errorOnLeftM "Parse Error" $ parseCFile (newGCC "gcc") Nothing opts {- "test/sort.c" -- -} "test/fibiter.c"
  ; case ast of 
    { AST.CTranslUnit (AST.CFDefExt fundef:_) nodeInfo -> 
         case runCFG fundef of
           { CFGOk (_, state) -> do 
                { -- putStrLn $ show $ buildDTree (cfg state)
                ; -- putStrLn $ show $ buildDF (cfg state)
                ; let cps = ssa2cps fundef (buildSSA (cfg state))
                ; -- putStrLn $ show $ cps
                ; putStrLn $ render $ pretty (cps_ctxt cps)
                ; mapM_  (\f -> putStrLn $ render $ pretty f) (cps_funcs cps)
                }
           ; CFGError s       -> error s
           }
    ; _ -> error "not fundec"
    }
  }


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
data CPS = CPS { cps_decls :: [AST.CCompoundBlockItem N.NodeInfo]  -- ^ main function decls
               , cps_stmts :: [AST.CCompoundBlockItem N.NodeInfo]  -- ^ main function stmts
               , cps_funcs :: [AST.CFunctionDef N.NodeInfo] -- ^ generated auxillary functions
               , cps_ctxt  :: AST.CDeclaration N.NodeInfo -- ^ the context for the closure
               } deriving Show
                          
-- global names                          
kParamName = "kParam"
ctxtParamName = "ctxtParam"                          


{-
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
translation  t => T

-----------
int => int

-----------
bool => bool

t => t
-------------
t* => T*

t => T
------------
t[] => T[]


-------------
void => void
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
--------- (Var)
x => X
-}

instance CPSize (AST.CDeclarator N.NodeInfo) (AST.CDeclarator N.NodeInfo) where
  cps_trans declr@(AST.CDeclr mb_ident derivedDecltrs mb_cstrLtr attrs nodeInfo) = declr 

-}


-- fn, K, \bar{\Delta} |- \bar{b} => \bar{P}
--translating the labeled blocks to function decls

{-
fn, K, \bar{\Delta}, \bar{b}  |- b_i => P_i
--------------------------------------------- 
fn, K, \bar{\Delta} |- \bar{b} => \bar{P}
-}

type ContextName = String 


cps_trans_lbs :: ContextName -> 
                 Ident ->  -- ^ top level function name
                 -- ^ \bar{\Delta} become part of the labelled block flag (loop) 
                 M.Map Ident LabeledBlock ->  -- ^ \bar{b}
                 [AST.CFunctionDef N.NodeInfo] 
cps_trans_lbs ctxtName fname lb_map = 
  map (\(id,lb) -> cps_trans_lb ctxtName fname lb_map id lb) (M.toList lb_map)

{- fn, \bar{\Delta}, \bar{b}  |- b => P -}


cps_trans_lb :: ContextName -> 
                Ident ->  -- ^ top level function name
                -- ^ \bar{\Delta} become part of the labelled block flag (loop) 
                M.Map Ident LabeledBlock ->  -- ^ \bar{b}
                Ident ->  -- ^ label for the current block
                LabeledBlock ->  -- ^ the block
                AST.CFunctionDef N.NodeInfo

{-

fn, k, \bar{\Delta}, \bar{b} |- s => S
----------------------------------------------------------------- (LabBlk)
fn, \bar{\Delta}, \bar{b}  |- l_i : {s} => void fn_i(void => void k) { S } 

fn, k, \bar{\Delta}, \bar{b} |- s => S
--------------------------------------------------------------------------- (PhiBlk)
fn, \bar{\Delta}, \bar{b}  |- l_i : {\bar{i}; s} => void fn_i(void => void k) { S }

-}


cps_trans_lb ctxtName fname lb_map ident lb = 
  let fname' = fname `app` ident
      tyVoid = [AST.CTypeSpec (AST.CVoidType N.undefNode)]
      declrs = []
      k      = internalIdent kParamName
      paramK = AST.CDecl tyVoid -- void (*k)(ctxt *)
               [(Just (AST.CDeclr (Just k) 
                       [ AST.CPtrDeclr [] N.undefNode
                       , AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CTypeDef (internalIdent ctxtName) N.undefNode) ] 
                                                [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] 
                                                N.undefNode], False)
                                       ) [] N.undefNode] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode 
      paramCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (internalIdent ctxtName) N.undefNode)]  -- ctxt* ctxt
                  [(Just (AST.CDeclr (Just (internalIdent ctxtParamName)) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
      paramDeclr = [paramK, paramCtxt]
      decltrs = [AST.CFunDeclr (Right ([paramK, paramCtxt],False)) [] N.undefNode] 
      mb_strLitr = Nothing
      attrs  =  []
      stmt' =  AST.CCompound [] (cps_trans_stmts ctxtName fname k lb_map ident (lb_loop lb) (lb_stmts lb)) N.undefNode      
  in AST.CFunDef tyVoid (AST.CDeclr (Just fname') decltrs mb_strLitr attrs N.undefNode) declrs stmt' N.undefNode
    
     
     
cps_trans_stmts :: ContextName -> 
                   Ident -> -- ^ fname 
                   Ident -> -- ^ K
                   -- ^ \bar{\Delta} become part of the labelled block flag (loop) 
                   M.Map Ident LabeledBlock ->  -- ^ \bar{b}
                   Ident ->  -- ^ label for the current block
                   Bool  ->  -- ^ whether label \in \Delta (is it a loop block)
                   [AST.CCompoundBlockItem N.NodeInfo] ->  -- ^ stmts
                   [AST.CCompoundBlockItem N.NodeInfo]
cps_trans_stmts ctxtName fname k lb_map ident inDelta stmts = concatMap (\stmt -> cps_trans_stmt ctxtName fname k lb_map ident inDelta stmt) stmts -- todo: maybe pattern match the CCompound constructor here?



-- fn, K, \bar{\Delta}, \bar{b} |-_l s => S
cps_trans_stmt :: ContextName -> Ident -> Ident ->  M.Map Ident LabeledBlock -> Ident -> Bool -> AST.CCompoundBlockItem N.NodeInfo -> [AST.CCompoundBlockItem N.NodeInfo]
{-
l_i : { \bar{i} ; s } \in \bar{b}   l, l_i |- \bar{i} => x1 = e1; ...; xn =en; 
----------------------------------------------------------------------------- (GT1)
fn, K, \bar{\Delta}, \bar{b} |-_l goto l_i => x1 = e1; ...; xn = en ; fnl_{i}(k)
-}
-- note that our target is C, hence besides k, the function call include argument such as context
cps_trans_stmt ctxtName fname k lb_map ident inDelta (AST.CBlockStmt (AST.CGoto li nodeInfo)) = case M.lookup li lb_map of 
  { Just lb | null (phis lb) -> 
       let asgmts  = cps_trans_phis ctxtName ident li (phis lb)
           fname'  = fname `app` li
           args    = [ (AST.CVar (internalIdent ctxtName) N.undefNode) .->. k
                     , AST.CVar (internalIdent ctxtName) N.undefNode ] 
           funcall = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (AST.CVar fname' N.undefNode) args N.undefNode)) N.undefNode)
       in asgmts ++ [ funcall ]
{-
l_i : { s } \in \bar{b}    
----------------------------------------------------------------------------- (GT2)
fn, K, \bar{\Delta}, \bar{b} |-_l goto l_i => fnl_{i}(k)
-}                                   
            | otherwise      -> 
         let fname'  = fname `app` li
             args    = [ (AST.CVar (internalIdent ctxtName) N.undefNode) .->. k
                       , AST.CVar (internalIdent ctxtName) N.undefNode ] 
             funcall = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (AST.CVar fname' N.undefNode) args N.undefNode)) N.undefNode)
         in [ funcall ]
  ; Nothing -> error "cps_trans_stmt failed at a non existent label."
  }
                                                                                        

cps_trans_stmt ctxtName fname k lb_map ident inDelta (AST.CBlockStmt (AST.CExpr (Just e) nodeInfo)) = 
  case e of 
   { AST.CAssign op lval rval ni -> 
{-                                                                                        
x => X; e => E; fn, K, \bar{\Delta}, \bar{b} |-_l s => S
----------------------------------------------------------------------------- (SeqAssign)
fn, K, \bar{\Delta}, \bar{b} |-_l x = e;s => X = E;S
-}             
     let e' = cps_trans_exp e
     in [AST.CBlockStmt (AST.CExpr (Just e') nodeInfo)]
   ; _ -> 
{-                                                                                        
e => E; fn, K, \bar{\Delta}, \bar{b} |-_l s => S
----------------------------------------------------------------------------- (SeqE)
fn, K, \bar{\Delta}, \bar{b} |-_l e;s => E;S
-}     
     let e' = cps_trans_exp e
     in [AST.CBlockStmt (AST.CExpr (Just e') nodeInfo)]
   }
{-
----------------------------------------------------------------------------- (returnNil)
fn, K, \bar{\Delta}, \bar{b} |-_l return; => K();
-}
-- C does not support higher order function.
-- K is passed in as a formal arg
-- K is a pointer to function
cps_trans_stmt ctxtName fname k lb_map ident inDelta (AST.CBlockStmt (AST.CReturn Nothing nodeInfo)) = 
  let funcall = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (AST.CUnary AST.CIndOp (AST.CVar k N.undefNode) N.undefNode ) [] N.undefNode)) N.undefNode)
  in [ funcall ]

{-
e => E
----------------------------------------------------------------------------- (returnE)
fn, K, \bar{\Delta}, \bar{b} |-_l return e; => x_r = E; K()
-}
-- C does not support higher order function.
-- x_r and K belong to the contxt
-- K is a pointer to function
cps_trans_stmt ctxtName fname k lb_map ident inDelta (AST.CBlockStmt (AST.CReturn (Just e) nodeInfo)) = 
  let funcall = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (AST.CUnary AST.CIndOp (AST.CVar k N.undefNode) N.undefNode ) [] N.undefNode)) N.undefNode)
      e' = cps_trans_exp e
      assign = AST.CAssign AST.CAssignOp ((AST.CVar (internalIdent ctxtName) N.undefNode) .->. (internalIdent "x_r")) e' N.undefNode
  in [ AST.CBlockStmt (AST.CExpr (Just assign) nodeInfo), funcall ]
  
     

cps_trans_stmt ctxtName fname k lb_map ident inDelta (AST.CBlockStmt (AST.CIf exp trueStmt mbFalseStmt nodeInfo)) 
{-
l \in \Delta
e => E
------------------------------------------------------------------------------------------------------- (IfLoop)
fn, K, \bar{\Delta}, \bar{b} |-_l if (e) { goto l1 } else { goto l2 } => loop(() => E, f_l1 ,f_l2, k) ; 
-}  
-- in C, f_l1 and f_l2 need to be passed in as address &
  | inDelta = 
    let exp' = cps_trans_exp exp
        lbl_tr = case trueStmt of 
          { AST.CGoto lbl _ -> lbl 
          ; _ -> error "cps_trans_stmt error: the true statement in a looping if statement does not contain goto"
          }
        lbl_fl = case mbFalseStmt of 
          { Just (AST.CGoto lbl _) -> lbl 
          ; _ -> error "cps_trans_stmt error: the false statement in a looping if statement does not contain goto"
          }
        fname' = internalIdent "cps_loop"
        args   = [ exp' -- todo put lambda
                 , AST.CUnary AST.CAdrOp (AST.CVar (fname `app` lbl_tr) N.undefNode) N.undefNode
                 , AST.CUnary AST.CAdrOp (AST.CVar (fname `app` lbl_fl) N.undefNode) N.undefNode
                 , (AST.CVar (internalIdent ctxtName) N.undefNode) .->. k
                 ]
        funcall = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (AST.CVar fname' N.undefNode) args N.undefNode)) N.undefNode)
    in [ funcall ]
{-
l \not \in \Delta
e => E
fn, K, \bar{\Delta}, \bar{b} |-_l s1 => S1
fn, K, \bar{\Delta}, \bar{b} |-_l s2 => S2
--------------------------------------------------------------------------------------------- (IfNotLoop)
fn, K, \bar{\Delta}, \bar{b} |-_l if (e) { s1 } else { s2 } => if (E) { S1 } else { S2 } ; 
-}  
  | otherwise = 
    let trueStmt'    = case trueStmt of 
          { AST.CCompound ids items ni -> AST.CCompound ids (cps_trans_stmts ctxtName fname k lb_map ident inDelta items) ni 
          ; _ -> case cps_trans_stmt ctxtName fname k lb_map ident inDelta (AST.CBlockStmt trueStmt) of 
            { [ AST.CBlockStmt trueStmt'' ] -> trueStmt''
            ; items -> AST.CCompound [] items N.undefNode
            }
          }
        mbFalseStmt' = case mbFalseStmt of 
          { Nothing        -> Nothing 
          ; Just (AST.CCompound ids items ni) -> Just $ AST.CCompound ids (cps_trans_stmts ctxtName fname k lb_map ident inDelta items) ni   
          ; Just falseStmt -> Just $ case cps_trans_stmt ctxtName fname k lb_map ident inDelta (AST.CBlockStmt falseStmt) of 
            { [  AST.CBlockStmt falseStmt'' ] -> falseStmt'' 
            ; items -> AST.CCompound [] items N.undefNode
            }
          }
        exp' = cps_trans_exp exp
    in [AST.CBlockStmt (AST.CIf exp' trueStmt' mbFalseStmt' nodeInfo)]


cps_trans_stmt ctxtName fname k lb_map ident inDelta stmt = undefined
     


cps_trans_phis ::  ContextName ->
                   Ident -> -- ^ source block label (where goto is invoked)
                   Ident -> -- ^ destination block label (where goto is jumping to)
                   [( Ident -- ^ var being redefined 
                    , [(Ident, Ident)])] ->  -- ^ incoming block x renamed variables
                   [AST.CCompoundBlockItem N.NodeInfo]
cps_trans_phis ctxtName src_lb dest_lb ps = map (cps_trans_phi ctxtName src_lb dest_lb) ps


(.->.) :: AST.CExpression N.NodeInfo -> Ident -> AST.CExpression N.NodeInfo
(.->.) struct member = AST.CMember struct member True N.undefNode

(...) :: AST.CExpression N.NodeInfo -> Ident -> AST.CExpression N.NodeInfo
(...) struct member = AST.CMember struct member False N.undefNode



{-
---------------------------------------
l_s, l_d |- \bar{i} => \bar{x = e}
-}

cps_trans_phi :: ContextName -> 
                 Ident -> -- ^ source block label (where goto is invoked)
                 Ident -> -- ^ destination block label (where goto is jumping to)
                 (Ident, [(Ident, Ident)]) -> 
                 AST.CCompoundBlockItem N.NodeInfo
cps_trans_phi ctxtName src_lb dest_lb (var, pairs) = 
  case lookup src_lb pairs of -- look for the matching label according to the source label
    { Nothing           -> error "cps_trans_phi failed: can't find the source label from the incoming block labels."
    ; Just redefined_lb -> -- lbl in which the var is redefined (it could be the precedence of src_lb)
      let lhs = (AST.CVar (internalIdent ctxtName) N.undefNode) .->. (var `app` dest_lb)
          rhs = (AST.CVar (internalIdent ctxtName) N.undefNode) .->. (var `app` redefined_lb)
      in AST.CBlockStmt (AST.CExpr (Just (AST.CAssign AST.CAssignOp lhs rhs N.undefNode)) N.undefNode) -- todo check var has been renamed with label
    }



-- e => E
{-
--------- (ExpVal)
 v => V

 e => E,      e_i => E_i
-------------------------- (ExpApp)
 e(\bar{e}) => E(\bar{E})
-}
-- it seems just to be identical 
cps_trans_exp :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
cps_trans_exp e = e

{-
top level translation   p => P

t => T    x => X     ti => Ti     xi => Xi     di => Di
\bar{b} |- \bar{\Delta}    k, \bar{\Delta} |- \bar{b} => \bar{P}
P1 = void f1 (void => void k) { B1 } 
------------------------------------------------------------------------
|- t x (\bar{t x}) {\bar{d};\bar{b}}  => 
         T X (\bar{T X}) {\bar{D}; T rx; \bar{P}; f1(id); return rx; }
-}
-- our target language C differs from the above specification.                          
-- 1. the top function's type signature is not captured within SSA
-- 2. \Delta is captured as the loop flag in LabaledBlock
-- 3. there is no lambda expression, closure needs to be created as a context
--     aux function that has type (void => void) => void should be in fact 
--    (void => void, ctxt*) => void
-- 4. \bar{D} should be the context malloc and initialization
-- 5. all the formal args should be copied to context
ssa2cps :: (AST.CFunctionDef N.NodeInfo) -> SSA -> CPS 
ssa2cps fundef (SSA scopedDecls labelledBlocks) = 
  let funName = case getFunName fundef of { Just s -> s ; Nothing -> "unanmed" }
      formalArgDecls :: [AST.CDeclaration N.NodeInfo]
      formalArgDecls = getFormalArgs fundef
      formalArgIds :: [Ident]
      formalArgIds = concatMap (\declaration -> getFormalArgIds declaration) formalArgDecls
      ctxtName = funName ++ "Ctxt"
      -- the context struct declaration
      context = mkContext ctxtName formalArgDecls scopedDecls 
      -- all the "nested" function declarations
      ps = cps_trans_lbs ctxtName (internalIdent funName) {- (internalIdent "id") -} labelledBlocks
      main_decls = 
        -- 1. malloc the context
        -- ctxtTy * ctxt = (ctxtTy *) malloc(sizeof(ctxtTy));
        [ AST.CBlockDecl (AST.CDecl 
                          [AST.CTypeSpec (AST.CTypeDef (internalIdent ctxtName) N.undefNode)] 
                          [(Just (AST.CDeclr (Just (internalIdent "ctxt")) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),
                            Just (AST.CInitExpr 
                                  (AST.CCast (AST.CDecl [AST.CTypeSpec (AST.CTypeDef (internalIdent ctxtName) N.undefNode)] 
                                              [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode) 
                                   (AST.CCall (AST.CVar (internalIdent "malloc") N.undefNode) 
                                    [AST.CSizeofType (AST.CDecl [AST.CTypeSpec (AST.CTypeDef (internalIdent ctxtName) N.undefNode)] [] N.undefNode) N.undefNode] N.undefNode) N.undefNode) N.undefNode),Nothing)] N.undefNode)
        ] 
      -- formalArgs = undefined
      main_stmts = 
        -- 2. initialize the counter-part  in the context of the formal args
        -- forall arg. ctxt->arg 
        [ AST.CBlockStmt (AST.CExpr (Just (AST.CAssign AST.CAssignOp ((AST.CVar (internalIdent "ctxt") N.undefNode) .->. arg) (AST.CVar arg N.undefNode) N.undefNode)) N.undefNode) | 
          arg <- formalArgIds
        ] 
      
  in CPS main_decls main_stmts ps context 

getFunName :: (AST.CFunctionDef N.NodeInfo) -> Maybe String
getFunName (AST.CFunDef tySpecfs declarator decls stmt nodeInfo) = getDeclrName declarator

getDeclrName :: (AST.CDeclarator N.NodeInfo) -> Maybe String
getDeclrName (AST.CDeclr Nothing decltrs mb_strLtr attrs nodeInfo) = Nothing
getDeclrName (AST.CDeclr (Just (Ident str _ _)) decltrs mb_strLtr attrs nodeInfo)  = Just str

getFormalArgs :: (AST.CFunctionDef N.NodeInfo) -> [AST.CDeclaration N.NodeInfo]
getFormalArgs (AST.CFunDef tySpecfs declarator decls stmt nodeInfo) = getFormalArgsFromDeclarator declarator

getFormalArgsFromDeclarator :: (AST.CDeclarator N.NodeInfo) -> [AST.CDeclaration N.NodeInfo]
getFormalArgsFromDeclarator (AST.CDeclr mb_ident derivedDecltrs mb_ctrlit attrs nodeInfo) = concatMap getFormalArgsFromDerivedDeclarator derivedDecltrs

getFormalArgsFromDerivedDeclarator :: (AST.CDerivedDeclarator N.NodeInfo) -> [AST.CDeclaration N.NodeInfo]
getFormalArgsFromDerivedDeclarator (AST.CFunDeclr either_old_new attrs nodeInfo) =  -- either_old_new :: Either [Ident] ([CDeclaration a],Bool)
  case either_old_new of 
   { Left idents -> [] -- todo: check what the old style is like?
   ; Right (declarations, flag) -> declarations --  concatMap getFormalArg declarations
   }



getFormalArgIds :: (AST.CDeclaration N.NodeInfo) -> [Ident]
getFormalArgIds (AST.CDecl tySpecs trips nodeInfo) = concatMap (\(mb_decltr, mb_init, mb_exp) -> case mb_decltr of 
                                                                   { Nothing -> [] 
                                                                   ; Just (AST.CDeclr (Just id) derived mb_clit attrs _) -> [id]
                                                                   ; Just _ -> []
                                                                   }) trips

mkContext :: String -> [AST.CDeclaration N.NodeInfo] -> [AST.CDeclaration N.NodeInfo] -> AST.CDeclaration N.NodeInfo
mkContext name formal_arg_decls local_var_decls = -- todo the loop higher function stack
  let structName  = internalIdent name
      ctxtAlias = AST.CDeclr (Just (internalIdent (map toLower name))) [] Nothing [] N.undefNode
      attrs     = []
      decls'    = formal_arg_decls ++ local_var_decls
      tyDef     = AST.CStorageSpec (AST.CTypedef N.undefNode)
      structDef =
        AST.CTypeSpec (AST.CSUType
                       (AST.CStruct AST.CStructTag (Just structName) (Just decls') attrs N.undefNode) N.undefNode) 
  in AST.CDecl [tyDef, structDef] [(Just ctxtAlias, Nothing, Nothing)] N.undefNode


{-

typedef struct FibCtxt {
  int x_0;
  int (*cond2)(struct FibCtxt *);
  void (*visitor3)(void (*k)(struct FibCtxt*), struct FibCtxt*);
} fibctxt;



CTranslUnit 
  [CDeclExt (CDecl  -- type specifier
             [ CStorageSpec (CTypedef (NodeInfo ("test.c": line 3) (("test.c": line 3),7) (Name {nameId = 0}))) -- typedef
             , CTypeSpec (CSUType {- struct or union type -} (CStruct  -- typename struct
                                   CStructTag  -- to indicate that itis a struct not a union but should not CStruct do the job?
                                   (Just (Ident "FibCtxt" 144044346 (NodeInfo ("test.c": line 3) (("test.c": line 3),7) (Name {nameId = 1}))))  -- structName
                                   (Just [CDecl 
                                          [CTypeSpec (CIntType (NodeInfo ("test.c": line 4) (("test.c": line 4),3) (Name {nameId = 3})))] 
                                          [(Just (CDeclr (Just (Ident "x_0" 798712 (NodeInfo ("test.c": line 4) (("test.c": line 4),3) (Name {nameId = 2})))) [] Nothing [] (NodeInfo ("test.c": line 4) (("test.c": line 4),3) (Name {nameId = 4}))),Nothing,Nothing)] 
                                          (NodeInfo ("test.c": line 4) (("test.c": line 4),3) (Name {nameId = 5}))
                                               
                                         ,CDecl 
                                          [CTypeSpec (CIntType (NodeInfo ("test.c": line 5) (("test.c": line 5),3) (Name {nameId = 6})))] 
                                          [(Just (CDeclr (Just (Ident "cond2" 211531797 (NodeInfo ("test.c": line 5) (("test.c": line 5),5) (Name {nameId = 7})))) [CPtrDeclr [] (NodeInfo ("test.c": line 5) (("test.c": line 5),5) (Name {nameId = 9})),CFunDeclr (Right ([CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just (Ident "FibCtxt" 144044346 (NodeInfo ("test.c": line 5) (("test.c": line 5),7) (Name {nameId = 10})))) Nothing [] (NodeInfo ("test.c": line 5) (("test.c": line 5),7) (Name {nameId = 11}))) (NodeInfo ("test.c": line 5) (("test.c": line 5),7) (Name {nameId = 12})))] [(Just (CDeclr Nothing [CPtrDeclr [] (NodeInfo ("test.c": line 5) (("test.c": line 5),1) (Name {nameId = 13}))] Nothing [] (OnlyPos <no file> (<no file>,-1))),Nothing,Nothing)] (NodeInfo ("test.c": line 5) (("test.c": line 5),1) (Name {nameId = 14}))],False)) [] (NodeInfo ("test.c": line 5) (("test.c": line 5),1) (Name {nameId = 15}))] Nothing [] (NodeInfo ("test.c": line 5) (("test.c": line 5),5) (Name {nameId = 8}))),Nothing,Nothing)] 
                                          (NodeInfo ("test.c": line 5) (("test.c": line 5),1) (Name {nameId = 16}))
                                               
                                         ,CDecl 
                                          [CTypeSpec (CVoidType (NodeInfo ("test.c": line 6) (("test.c": line 6),4) (Name {nameId = 17})))] 
                                          [(Just (CDeclr (Just (Ident "visitor3" 330935530 (NodeInfo ("test.c": line 6) (("test.c": line 6),8) (Name {nameId = 18})))) [CPtrDeclr [] (NodeInfo ("test.c": line 6) (("test.c": line 6),8) (Name {nameId = 20})),CFunDeclr (Right ([CDecl [CTypeSpec (CVoidType (NodeInfo ("test.c": line 6) (("test.c": line 6),4) (Name {nameId = 21})))] [(Just (CDeclr (Just (Ident "k" 107 (NodeInfo ("test.c": line 6) (("test.c": line 6),1) (Name {nameId = 22})))) [CPtrDeclr [] (NodeInfo ("test.c": line 6) (("test.c": line 6),1) (Name {nameId = 24})),CFunDeclr (Right ([CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just (Ident "FibCtxt" 144044346 (NodeInfo ("test.c": line 6) (("test.c": line 6),7) (Name {nameId = 25})))) Nothing [] (NodeInfo ("test.c": line 6) (("test.c": line 6),7) (Name {nameId = 26}))) (NodeInfo ("test.c": line 6) (("test.c": line 6),7) (Name {nameId = 27})))] [(Just (CDeclr Nothing [CPtrDeclr [] (NodeInfo ("test.c": line 6) (("test.c": line 6),1) (Name {nameId = 28}))] Nothing [] (OnlyPos <no file> (<no file>,-1))),Nothing,Nothing)] (NodeInfo ("test.c": line 6) (("test.c": line 6),1) (Name {nameId = 29}))],False)) [] (NodeInfo ("test.c": line 6) (("test.c": line 6),1) (Name {nameId = 30}))] Nothing [] (NodeInfo ("test.c": line 6) (("test.c": line 6),1) (Name {nameId = 23}))),Nothing,Nothing)] (NodeInfo ("test.c": line 6) (("test.c": line 6),1) (Name {nameId = 31})),CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just (Ident "FibCtxt" 144044346 (NodeInfo ("test.c": line 6) (("test.c": line 6),7) (Name {nameId = 32})))) Nothing [] (NodeInfo ("test.c": line 6) (("test.c": line 6),7) (Name {nameId = 33}))) (NodeInfo ("test.c": line 6) (("test.c": line 6),7) (Name {nameId = 34})))] [(Just (CDeclr Nothing [CPtrDeclr [] (NodeInfo ("test.c": line 6) (("test.c": line 6),1) (Name {nameId = 35}))] Nothing [] (OnlyPos <no file> (<no file>,-1))),Nothing,Nothing)] (NodeInfo ("test.c": line 6) (("test.c": line 6),1) (Name {nameId = 36}))],False)) [] (NodeInfo ("test.c": line 6) (("test.c": line 6),1) (Name {nameId = 37}))] Nothing [] (NodeInfo ("test.c": line 6) (("test.c": line 6),8) (Name {nameId = 19}))),Nothing,Nothing)] 
                                          (NodeInfo ("test.c": line 6) (("test.c": line 6),1) (Name {nameId = 38}))]) 
                                   [] 
                                   (NodeInfo ("test.c": line 3) (("test.c": line 7),1) (Name {nameId = 40}))) 
                          (NodeInfo ("test.c": line 3) (("test.c": line 7),1) (Name {nameId = 41})))
                   ] 
             -- mb_declarator,mb_init,mib
             [(Just (CDeclr (Just (Ident "fibctxt" 211153242 (NodeInfo ("test.c": line 7) (("test.c": line 7),7) (Name {nameId = 39})))) [] Nothing [] (NodeInfo ("test.c": line 7) (("test.c": line 7),7) (Name {nameId = 42}))),Nothing,Nothing)] 
             (NodeInfo ("test.c": line 3) (("test.c": line 7),1) (Name {nameId = 43})))] 
  (NodeInfo ("test.c": line 3) (("test.c": line 7),1) (Name {nameId = 44}))


-}

