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

-- TODO LIST:
-- 1. check whether k vs kParam (done)
-- 2. id, loop an loop lambda (done)
-- 3. pop and push (done)
-- 4. function signatures (done)
-- 5. max stack size
-- 6. curr_stack_size + 1 or - 1

testCPS = do 
  { let opts = []
  ; ast <- errorOnLeftM "Parse Error" $ parseCFile (newGCC "gcc") Nothing opts {- "test/sort.c" -- -} "test/fibiter.c"
  ; case ast of 
    { AST.CTranslUnit (AST.CFDefExt fundef:_) nodeInfo -> 
         case runCFG fundef of
           { CFGOk (_, state) -> do 
                { -- putStrLn $ show $ buildDTree (cfg state)
                ; -- putStrLn $ show $ buildDF (cfg state)
                ; let (SSA scopedDecls labelledBlocks sdom) = buildSSA (cfg state)
                      visitors = allVisitors sdom labelledBlocks
                      exits    = allExits labelledBlocks
                ; putStrLn $ show $ visitors
                ; putStrLn $ show $ exits                  

                ; let cps = ssa2cps fundef (buildSSA (cfg state))
                ; -- putStrLn $ show $ cps
                ; putStrLn $ render $ pretty (cps_ctxt cps)
                ; mapM_  (\d -> putStrLn $ render $ pretty d) (cps_funcsigs cps)
                ; mapM_  (\f -> putStrLn $ render $ pretty f) ((cps_funcs cps) ++ [cps_main cps])

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
               , cps_funcsigs :: [AST.CDeclaration N.NodeInfo] -- ^ the signatures decls of the auxillary functions
               , cps_funcs :: [AST.CFunctionDef N.NodeInfo] -- ^ the auxillary functions
               , cps_ctxt  :: AST.CDeclaration N.NodeInfo -- ^ the context for the closure
               , cps_main ::  AST.CFunctionDef N.NodeInfo -- ^ the main function
               } deriving Show
                          
-- global names                          
kParamName = "kParam"
ctxtParamName = "ctxtParam"                          
condParamName = "condParam"
visitorParamName = "visitorParam"
exitParamName = "exitParam"
currStackSizeName = "curr_stack_size"

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

type Visitors = M.Map Ident Ident -- ^ last label of the visitor -> label of the loop
type Exits    = M.Map Ident Ident -- ^ label of the exit -> label of the loop

cps_trans_lbs :: ContextName -> 
                 Ident ->  -- ^ top level function name
                 -- ^ \bar{\Delta} become part of the labelled block flag (loop) 
                 Visitors ->
                 Exits ->                 
                 M.Map Ident LabeledBlock ->  -- ^ \bar{b}
                 [AST.CFunctionDef N.NodeInfo] 
cps_trans_lbs ctxtName fname visitors exits lb_map = 
  map (\(id,lb) -> cps_trans_lb ctxtName fname lb_map id visitors exits lb) (M.toList lb_map)

{- fn, \bar{\Delta}, \bar{b}  |- b => P -}


cps_trans_lb :: ContextName -> 
                Ident ->  -- ^ top level function name
                -- ^ \bar{\Delta} become part of the labelled block flag (loop) 
                M.Map Ident LabeledBlock ->  -- ^ \bar{b}
                Ident ->  -- ^ label for the current block
                Visitors ->
                Exits ->                
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


cps_trans_lb ctxtName fname lb_map ident visitors exits  lb  = 
  let fname' = fname `app` ident
      tyVoid = [AST.CTypeSpec (AST.CVoidType N.undefNode)]
      declrs = []
      k      = iid kParamName
      paramK = AST.CDecl tyVoid -- void (*k)(ctxt *)
               [(Just (AST.CDeclr (Just k) 
                       [ AST.CPtrDeclr [] N.undefNode
                       , AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode) ] 
                                                [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] 
                                                N.undefNode], False)
                                       ) [] N.undefNode] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode 
      paramCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)]  -- ctxt* ctxt
                  [(Just (AST.CDeclr (Just (iid ctxtParamName)) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
      paramDeclr = [paramK, paramCtxt]
      decltrs = [AST.CFunDeclr (Right ([paramK, paramCtxt],False)) [] N.undefNode] 
      mb_strLitr = Nothing
      attrs  =  []
      pop   | ident `M.member` exits = [ AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` (iid "pop"))) [cvar $ iid ctxtParamName] N.undefNode)) N.undefNode) ] 
            | otherwise              = []
      stmt' =  AST.CCompound [] (pop ++ (cps_trans_stmts ctxtName fname k lb_map ident (lb_loop lb) visitors exits (lb_stmts lb))) N.undefNode      
  in AST.CFunDef tyVoid (AST.CDeclr (Just fname') decltrs mb_strLitr attrs N.undefNode) declrs stmt' N.undefNode
    
     
     
cps_trans_stmts :: ContextName -> 
                   Ident -> -- ^ fname 
                   Ident -> -- ^ K
                   -- ^ \bar{\Delta} become part of the labelled block flag (loop) 
                   M.Map Ident LabeledBlock ->  -- ^ \bar{b}
                   Ident ->  -- ^ label for the current block
                   Bool  ->  -- ^ whether label \in \Delta (is it a loop block)
                   Visitors ->
                   Exits ->
                   [AST.CCompoundBlockItem N.NodeInfo] ->  -- ^ stmts
                   [AST.CCompoundBlockItem N.NodeInfo]
cps_trans_stmts ctxtName fname k lb_map ident inDelta visitors exits stmts = concatMap (\stmt -> cps_trans_stmt ctxtName fname k lb_map ident inDelta visitors exits stmt) stmts -- todo: maybe pattern match the CCompound constructor here?



-- fn, K, \bar{\Delta}, \bar{b} |-_l s => S
cps_trans_stmt :: ContextName -> 
                  Ident ->  -- ^ fname
                  Ident ->  -- ^ K
                   -- ^ \bar{\Delta} become part of the labelled block flag (loop)                   
                  M.Map Ident LabeledBlock -> -- ^ \bar{b}
                  Ident ->  -- ^ label for the current block
                  Bool -> -- ^ whether label \in \Delta (is it a loop block)
                  Visitors ->
                  Exits ->
                  AST.CCompoundBlockItem N.NodeInfo -> 
                  [AST.CCompoundBlockItem N.NodeInfo]
{-
l_i : { \bar{i} ; s } \in \bar{b}   l, l_i |- \bar{i} => x1 = e1; ...; xn =en; 
----------------------------------------------------------------------------- (GT1)
fn, K, \bar{\Delta}, \bar{b} |-_l goto l_i => x1 = e1; ...; xn = en ; fnl_{i}(k)
-}
-- note that our target is C, hence besides k, the function call include argument such as context
cps_trans_stmt ctxtName fname k lb_map ident inDelta visitors exits (AST.CBlockStmt (AST.CGoto li nodeInfo)) = case M.lookup li lb_map of 
  { Just lb | not (null (phis lb)) -> 
       let asgmts  = cps_trans_phis ctxtName ident li (phis lb)
           fname'  = fname `app` li
           args    = [ cvar k
                     , cvar (iid ctxtParamName)] 
           funcall = case M.lookup ident visitors of -- in case it is the last block of descending from the loop-if then branch, we call (*k)(ctxt) instead
             { Just loop_lbl | li == loop_lbl -> AST.CBlockStmt (AST.CExpr (Just (AST.CCall (ind $ cvar k) [cvar (iid ctxtParamName)] N.undefNode)) N.undefNode) 
             ; _ -> AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar fname') args N.undefNode)) N.undefNode)
             }
       in asgmts ++ [ funcall ]
{-
l_i : { s } \in \bar{b}    
----------------------------------------------------------------------------- (GT2)
fn, K, \bar{\Delta}, \bar{b} |-_l goto l_i => fnl_{i}(k)
-}                                   
            | otherwise      -> 
         let fname'  = fname `app` li
             args    = [ cvar k
                       , cvar (iid ctxtParamName) ] 
             funcall = case M.lookup ident visitors of -- in case it is the last block of descending from the loop-if then branch, we call (*k)(ctxt) instead
               { Just loop_lbl | li == loop_lbl -> AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar k) [cvar (iid ctxtName)] N.undefNode)) N.undefNode) 
               ; _ -> AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar fname') args N.undefNode)) N.undefNode)
               }
         in [ funcall ]
  ; Nothing -> error "cps_trans_stmt failed at a non existent label."
  }
                                                                                        

cps_trans_stmt ctxtName fname k lb_map ident inDelta visitors exits (AST.CBlockStmt (AST.CExpr (Just e) nodeInfo)) = 
  case e of 
   { AST.CAssign op lval rval ni -> 
{-                                                                                        
x => X; e => E; fn, K, \bar{\Delta}, \bar{b} |-_l s => S
----------------------------------------------------------------------------- (SeqAssign)
fn, K, \bar{\Delta}, \bar{b} |-_l x = e;s => X = E;S
-}             
     let e' = cps_trans_exp ctxtName e
     in [AST.CBlockStmt (AST.CExpr (Just e') nodeInfo)]
   ; _ -> 
{-                                                                                        
e => E; fn, K, \bar{\Delta}, \bar{b} |-_l s => S
----------------------------------------------------------------------------- (SeqE)
fn, K, \bar{\Delta}, \bar{b} |-_l e;s => E;S
-}     
     let e' = cps_trans_exp ctxtName e
     in [AST.CBlockStmt (AST.CExpr (Just e') nodeInfo)]
   }
{-
----------------------------------------------------------------------------- (returnNil)
fn, K, \bar{\Delta}, \bar{b} |-_l return; => K();
-}
-- C does not support higher order function.
-- K is passed in as a formal arg
-- K is a pointer to function
cps_trans_stmt ctxtName fname k lb_map ident inDelta visitors exits (AST.CBlockStmt (AST.CReturn Nothing nodeInfo)) = 
  let funcall = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (ind (cvar k)) [(cvar (iid ctxtParamName))] N.undefNode)) N.undefNode)
  in [ funcall ]

{-
e => E
----------------------------------------------------------------------------- (returnE)
fn, K, \bar{\Delta}, \bar{b} |-_l return e; => x_r = E; K()
-}
-- C does not support higher order function.
-- x_r and K belong to the contxt
-- K is a pointer to function
cps_trans_stmt ctxtName fname k lb_map ident inDelta visitors exits (AST.CBlockStmt (AST.CReturn (Just e) nodeInfo)) = 
  let funcall = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (ind (cvar k)) [(cvar (iid ctxtParamName))] N.undefNode)) N.undefNode)
      e' = cps_trans_exp ctxtName e
      assign = ((cvar (iid ctxtParamName)) .->. (iid "func_result")) .=. e' 
  in [ AST.CBlockStmt (AST.CExpr (Just assign) nodeInfo), funcall ]
  
     

cps_trans_stmt ctxtName fname k lb_map ident inDelta  visitors exits (AST.CBlockStmt (AST.CIf exp trueStmt mbFalseStmt nodeInfo)) 
{-
l \in \Delta
e => E
------------------------------------------------------------------------------------------------------- (IfLoop)
fn, K, \bar{\Delta}, \bar{b} |-_l if (e) { goto l1 } else { goto l2 } => loop(() => E, f_l1 ,f_l2, k) ; 
-}  
-- in C, f_l1 and f_l2 need to be passed in as address &
-- we need to 1. insert pop(ctxt) in f_l2
--            2. let f_l3 be the closest descendant of f_l1 that call f_l(k,ctxt), replace the call by (*k)(ctxt)  
-- for 1, we can create a map to keep track of all the l2
-- for 2, we can perform a check at any l3 where it contains a "goto l;", look up l we find l1 is the true branch visitor
--        we need to check whether l1 sdom l3, if so, replace "goto l"  by (*k())
  | inDelta = 
    let exp' = cps_trans_exp ctxtName exp
        lbl_tr = case trueStmt of 
          { AST.CGoto lbl _ -> lbl 
          ; _ -> error "cps_trans_stmt error: the true statement in a looping if statement does not contain goto"
          }
        lbl_fl = case mbFalseStmt of 
          { Just (AST.CGoto lbl _) -> lbl 
          ; _ -> error "cps_trans_stmt error: the false statement in a looping if statement does not contain goto"
          }
        push_args :: [AST.CExpression N.NodeInfo]
        push_args = [ AST.CUnary AST.CAdrOp (cvar $ fname `app` (iid "cond") `app` ident) N.undefNode
                    , AST.CUnary AST.CAdrOp (cvar (fname `app` lbl_tr)) N.undefNode
                    , AST.CUnary AST.CAdrOp (cvar (fname `app` lbl_fl)) N.undefNode
                    , cvar k
                    , cvar (iid ctxtParamName)
                    ]
        push = fname `app` (iid "push")
        call_push = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar push) push_args N.undefNode)) N.undefNode)
        
        loop =  fname `app` iid "loop"
        loop_args  = [ (cvar (iid ctxtParamName) .->. (iid "loop_conds")) .!!. (cvar (iid ctxtParamName) .->. (iid currStackSizeName))
                     , (cvar (iid ctxtParamName) .->. (iid "loop_visitors")) .!!. (cvar (iid ctxtParamName) .->. (iid currStackSizeName))
                     , (cvar (iid ctxtParamName) .->. (iid "loop_exits")) .!!. (cvar (iid ctxtParamName) .->. (iid currStackSizeName))
                     , (cvar (iid ctxtParamName) .->. (iid "loop_ks")) .!!. (cvar (iid ctxtParamName) .->. (iid currStackSizeName))
                     ]
                 
        call_loop = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar loop) loop_args N.undefNode)) N.undefNode)
    in [ call_push, call_loop ]
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
          { AST.CCompound ids items ni -> AST.CCompound ids (cps_trans_stmts ctxtName fname k lb_map ident inDelta visitors exits items) ni 
          ; _ -> case cps_trans_stmt ctxtName fname k lb_map ident inDelta visitors exits (AST.CBlockStmt trueStmt) of 
            { [ AST.CBlockStmt trueStmt'' ] -> trueStmt''
            ; items -> AST.CCompound [] items N.undefNode
            }
          }
        mbFalseStmt' = case mbFalseStmt of 
          { Nothing        -> Nothing 
          ; Just (AST.CCompound ids items ni) -> Just $ AST.CCompound ids (cps_trans_stmts ctxtName fname k lb_map ident inDelta visitors exits items) ni   
          ; Just falseStmt -> Just $ case cps_trans_stmt ctxtName fname k lb_map ident inDelta visitors exits (AST.CBlockStmt falseStmt) of 
            { [  AST.CBlockStmt falseStmt'' ] -> falseStmt'' 
            ; items -> AST.CCompound [] items N.undefNode
            }
          }
        exp' = cps_trans_exp ctxtName exp
    in [AST.CBlockStmt (AST.CIf exp' trueStmt' mbFalseStmt' nodeInfo)]


cps_trans_stmt ctxtName fname k lb_map ident inDelta visitors exits stmt = error "cps_trans_stmt error: unhandled case"
     


cps_trans_phis ::  ContextName ->
                   Ident -> -- ^ source block label (where goto is invoked)
                   Ident -> -- ^ destination block label (where goto is jumping to)
                   [( Ident -- ^ var being redefined 
                    , [(Ident, Ident)])] ->  -- ^ incoming block x renamed variables
                   [AST.CCompoundBlockItem N.NodeInfo]
cps_trans_phis ctxtName src_lb dest_lb ps = map (cps_trans_phi ctxtName src_lb dest_lb) ps






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
      let lhs = (cvar (iid ctxtParamName)) .->. (var `app` dest_lb)
          rhs = (cvar (iid ctxtParamName)) .->. (var `app` redefined_lb)
      in AST.CBlockStmt (AST.CExpr (Just (lhs .=. rhs)) N.undefNode) -- todo check var has been renamed with label
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
-- for C target, we need to rename x to ctxt->x
cps_trans_exp :: ContextName -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
cps_trans_exp ctxtName (AST.CAssign op lhs rhs nodeInfo)    = AST.CAssign op (cps_trans_exp ctxtName lhs) (cps_trans_exp ctxtName rhs) nodeInfo
cps_trans_exp ctxtName (AST.CComma es nodeInfo)             = AST.CComma (map (cps_trans_exp ctxtName) es) nodeInfo
cps_trans_exp ctxtName (AST.CCond e1 Nothing e3 nodeInfo)   = AST.CCond (cps_trans_exp ctxtName e1) Nothing (cps_trans_exp ctxtName e3) nodeInfo
cps_trans_exp ctxtName (AST.CCond e1 (Just e2) e3 nodeInfo) = AST.CCond (cps_trans_exp ctxtName e1) (Just $ cps_trans_exp ctxtName e2) (cps_trans_exp ctxtName e3) nodeInfo
cps_trans_exp ctxtName (AST.CBinary op e1 e2 nodeInfo)  = AST.CBinary op (cps_trans_exp ctxtName e1)  (cps_trans_exp ctxtName e2) nodeInfo
cps_trans_exp ctxtName (AST.CCast decl e nodeInfo)      = AST.CCast decl (cps_trans_exp ctxtName e) nodeInfo
cps_trans_exp ctxtName (AST.CUnary op e nodeInfo)       = AST.CUnary op (cps_trans_exp ctxtName e) nodeInfo
cps_trans_exp ctxtName (AST.CSizeofExpr e nodeInfo)     = AST.CSizeofExpr (cps_trans_exp ctxtName e) nodeInfo
cps_trans_exp ctxtName (AST.CSizeofType decl nodeInfo)  = AST.CSizeofType decl nodeInfo
cps_trans_exp ctxtName (AST.CAlignofExpr e nodeInfo)    = AST.CAlignofExpr (cps_trans_exp ctxtName e) nodeInfo
cps_trans_exp ctxtName (AST.CAlignofType decl nodeInfo) = AST.CAlignofType decl nodeInfo
cps_trans_exp ctxtName (AST.CComplexReal e nodeInfo)    = AST.CComplexReal (cps_trans_exp ctxtName e) nodeInfo
cps_trans_exp ctxtName (AST.CComplexImag e nodeInfo)    = AST.CComplexImag (cps_trans_exp ctxtName e) nodeInfo
cps_trans_exp ctxtName (AST.CIndex arr idx nodeInfo)    = AST.CIndex (cps_trans_exp ctxtName arr) (cps_trans_exp ctxtName idx) nodeInfo
cps_trans_exp ctxtName (AST.CCall f args nodeInfo)      = AST.CCall (cps_trans_exp ctxtName f) (map (cps_trans_exp ctxtName) args) nodeInfo
cps_trans_exp ctxtName (AST.CMember e ident deref nodeInfo) = AST.CMember  (cps_trans_exp ctxtName e) ident deref nodeInfo
cps_trans_exp ctxtName (AST.CVar id _)                      = (cvar (iid ctxtParamName)) .->. id
cps_trans_exp ctxtName (AST.CConst c)                       = AST.CConst c
cps_trans_exp ctxtName (AST.CCompoundLit decl initList nodeInfo) = AST.CCompoundLit decl initList nodeInfo -- todo check this
cps_trans_exp ctxtName (AST.CStatExpr stmt nodeInfo )       = AST.CStatExpr stmt nodeInfo  -- todo GNU C compount statement as expr
cps_trans_exp ctxtName (AST.CLabAddrExpr ident nodeInfo )   = AST.CLabAddrExpr ident nodeInfo -- todo  
cps_trans_exp ctxtName (AST.CBuiltinExpr builtin )          = AST.CBuiltinExpr builtin -- todo build in expression

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
ssa2cps fundef (SSA scopedDecls labelledBlocks sdom) = 
  let -- scraping the information from the top level function under obfuscation
      funName = case getFunName fundef of { Just s -> s ; Nothing -> "unanmed" }
      formalArgDecls :: [AST.CDeclaration N.NodeInfo]
      formalArgDecls = getFormalArgs fundef
      formalArgIds :: [Ident]
      formalArgIds = concatMap (\declaration -> getFormalArgIds declaration) formalArgDecls
      returnTy = getFunReturnTy fundef
      ctxtStructName = funName ++ "Ctxt"
      -- the context struct declaration
      labels = M.keys labelledBlocks
      context = mkContext ctxtStructName labels formalArgDecls scopedDecls returnTy
      ctxtName = map toLower ctxtStructName -- alias name is inlower case and will be used in the the rest of the code
      -- finding the visitor labels and the exit labels
      visitors = allVisitors sdom labelledBlocks
      exits    = allExits labelledBlocks
      -- all the conditional function 
      conds    = allLoopConds ctxtName funName labelledBlocks 
      -- loop_cps, loop_lambda, id and pop and push
      loop_cps = loopCPS ctxtName funName
      lambda_loop_cps = lambdaLoopCPS ctxtName funName
      id_cps = idCPS ctxtName funName
      push_cps = pushCPS ctxtName funName
      pop_cps = popCPS ctxtName funName
      -- all the "nested/helper" function declarations
      ps = cps_trans_lbs ctxtName (iid funName) {- (iid "id") -} visitors exits labelledBlocks 
      -- all function signatures
      funcSignatures = map funSig (ps ++ conds ++ [loop_cps, lambda_loop_cps, id_cps, push_cps, pop_cps])
      main_decls = 
        -- 1. malloc the context obj in the main func
        -- ctxtTy * ctxt = (ctxtTy *) malloc(sizeof(ctxtTy));
        [ AST.CBlockDecl (AST.CDecl 
                          [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)] 
                          [(Just (AST.CDeclr (Just (iid "ctxt")) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),
                            Just (AST.CInitExpr 
                                  (AST.CCast (AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)] 
                                              [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode) 
                                   (AST.CCall (AST.CVar (iid "malloc") N.undefNode) 
                                    [AST.CSizeofType (AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)] [] N.undefNode) N.undefNode] N.undefNode) N.undefNode) N.undefNode),Nothing)] N.undefNode)
        ] 
      main_stmts = 
        -- 2. initialize the counter-part  in the context of the formal args
        -- forall arg. ctxt->arg 
        [ AST.CBlockStmt (AST.CExpr (Just (((cvar (iid ctxtParamName)) .->. arg) .=. (AST.CVar arg N.undefNode))) N.undefNode) | 
          arg <- formalArgIds
        ] ++ [
          AST.CBlockStmt (AST.CExpr (Just (AST.CCall (AST.CVar ((iid funName) `app` (iid (labPref ++ "0" ))) N.undefNode) 
                                           [ AST.CUnary AST.CAdrOp (AST.CVar ((iid funName) `app` (iid "id")) N.undefNode) N.undefNode
                                           , AST.CVar (iid "ctxt") N.undefNode ] N.undefNode)) N.undefNode)
          ]
        
      main_func = case fundef of 
        { AST.CFunDef tySpecfs declarator decls _ nodeInfo -> 
             AST.CFunDef tySpecfs declarator decls (AST.CCompound [] (main_decls ++ main_stmts) N.undefNode) nodeInfo 
        }
  in CPS main_decls main_stmts funcSignatures (ps ++ conds ++ [loop_cps, lambda_loop_cps, id_cps, push_cps, pop_cps])  context main_func 
     
-- ^ get all visitor labels from te labeledblocks map     
allVisitors :: SDom -> M.Map NodeId LabeledBlock -> M.Map NodeId NodeId
allVisitors sdom lbs = M.fromList $  -- l is a visitor label if (1) succ of l, say l1 is loop, (2) l2, the then-branch label of l1 is sdom'ing l
  [ (l, succ) 
  | (l, lblk) <- M.toList lbs
  , succ <- lb_succs lblk
  , case M.lookup succ lbs of { Nothing -> False
                              ; Just lblk' -> (lb_loop lblk') && (domBy sdom (lb_succs lblk' !! 0) l)
                              }
  ]
     
-- ^ get all exit labels from the labeledblocks map
allExits :: M.Map NodeId LabeledBlock -> M.Map NodeId NodeId
allExits lbs = M.fromList $  
  [ ((lb_succs lblk) !! 1, l) | (l, lblk) <- M.toList lbs
  , (lb_loop lblk) && (length (lb_succs lblk) > 1) ] 
               

-- ^ retrieve all condional test from the loops and turn them into functions     
allLoopConds :: ContextName -> String ->  M.Map NodeId LabeledBlock -> [AST.CFunctionDef N.NodeInfo]
allLoopConds ctxtName fname lbs = 
  concatMap (\(id,lb) -> loopCond ctxtName fname (id,lb)) (M.toList lbs)

loopCond :: ContextName -> String -> (NodeId, LabeledBlock) -> [AST.CFunctionDef N.NodeInfo]
loopCond ctxtName fname (l,blk) 
  | lb_loop blk =
    let stmts = lb_stmts blk
        conds = [ cps_trans_exp ctxtName e | AST.CBlockStmt (AST.CIf e tt ff _) <- stmts ]
        formalArgCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)]  -- ctxt* ctxt
                   [(Just (AST.CDeclr (Just (iid ctxtParamName)) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
        decls = [ fun [AST.CTypeSpec boolTy] (iid fname `app` iid "cond" `app` l) [formalArgCtxt] [AST.CBlockStmt (AST.CReturn (Just cond) N.undefNode)]
                | cond <- conds ]
    in decls
  | otherwise  = [] 
                 

-- ^ push

{-
void push(int (*cond)(struct SortCtxt *),
	  void (*visitor)(void (*k)(struct SortCtxt*), struct SortCtxt*),
	  void (*exit)(void (*k)(struct SortCtxt*), struct SortCtxt*),
	  void (*k)(struct SortCtxt*),
	  sortctxt *ctxt) {
  ctxt->curr_stack_size = ctxt->curr_stack_size + 1;  
  ctxt->loop_conds[ctxt->curr_stack_size] = cond;
  ctxt->loop_visitors[ctxt->curr_stack_size] = visitor;
  ctxt->loop_exits[ctxt->curr_stack_size] = exit;
  ctxt->loop_ks[ctxt->curr_stack_size] = k;
}
-}
                 
pushCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo 
pushCPS ctxtName fname = 
  let cond          = iid condParamName
      formalArgCond = AST.CDecl [AST.CTypeSpec intTy] -- int (*cond)(ctxt *)
                      [(Just (AST.CDeclr (Just cond) 
                              [ AST.CPtrDeclr [] N.undefNode
                              , AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode) ] 
                                                       [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] 
                                                       N.undefNode], False)
                                              ) [] N.undefNode] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode 
      visitor       = iid visitorParamName
      formalArgX x = AST.CDecl [AST.CTypeSpec voidTy] -- void (*X)(void (*k)(ctxt*), ctxt*)
                         [(Just (AST.CDeclr (Just x)
                                 [ AST.CPtrDeclr [] N.undefNode
                                 , AST.CFunDeclr (Right ([ AST.CDecl [AST.CTypeSpec voidTy] [(Just (AST.CDeclr (Just k) [AST.CPtrDeclr [] N.undefNode
                                                                                                                        ,AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)] [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode], False)) [] N.undefNode] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode
                                                         , AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)] [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode
                                                         ],False)) [] N.undefNode
                                 ] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode
      formalArgVisitor = formalArgX visitor
      exit          = iid exitParamName
      formalArgExit = formalArgX exit
      k             = iid kParamName      
      formalArgK = AST.CDecl [AST.CTypeSpec voidTy] -- void (*k)(ctxt *)
               [(Just (AST.CDeclr (Just k) 
                       [ AST.CPtrDeclr [] N.undefNode
                       , AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode) ] 
                                                [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] 
                                                N.undefNode], False)
                                       ) [] N.undefNode] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode 
      ctxt          = iid ctxtParamName                 
      formalArgCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)]  -- ctxt* ctxt
                 [(Just (AST.CDeclr (Just ctxt) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
  in fun [AST.CTypeSpec voidTy] (iid fname `app` iid "push") [formalArgCond, formalArgVisitor, formalArgExit, formalArgK, formalArgCtxt] 
     [ AST.CBlockStmt (AST.CExpr (Just ((cvar ctxt .->. (iid currStackSizeName)) .=. (AST.CBinary AST.CAddOp (cvar ctxt .->. (iid currStackSizeName)) (AST.CConst (AST.CIntConst (cInteger 1) N.undefNode)) N.undefNode))) N.undefNode)
     , AST.CBlockStmt (AST.CExpr (Just (((cvar ctxt .->. (iid "loop_conds")) .!!. (cvar ctxt .->. (iid currStackSizeName))) .=. (cvar cond))) N.undefNode)
     , AST.CBlockStmt (AST.CExpr (Just (((cvar ctxt .->. (iid "loop_visitors")) .!!. (cvar ctxt .->. (iid currStackSizeName))) .=. (cvar visitor))) N.undefNode)
     , AST.CBlockStmt (AST.CExpr (Just (((cvar ctxt .->. (iid "loop_exits")) .!!. (cvar ctxt .->. (iid currStackSizeName))) .=. (cvar exit))) N.undefNode)
     , AST.CBlockStmt (AST.CExpr (Just (((cvar ctxt .->. (iid "loop_ks")) .!!. (cvar ctxt .->. (iid currStackSizeName))) .=. (cvar k))) N.undefNode)
     ]
                 

{-     
void pop(sortctxt *ctxt) {
  ctxt->curr_stack_size = ctxt->curr_stack_size - 1;
}
-}
     
popCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo 
popCPS ctxtName fname = 
  let ctxt          = iid ctxtParamName                 
      formalArgCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)]  -- ctxt* ctxt
                 [(Just (AST.CDeclr (Just ctxt) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
  in fun [AST.CTypeSpec voidTy] (iid fname `app` iid "pop") [formalArgCtxt]
     [ AST.CBlockStmt (AST.CExpr (Just ((cvar ctxt .->. (iid currStackSizeName)) .=. (AST.CBinary AST.CSubOp (cvar ctxt .->. (iid currStackSizeName)) (AST.CConst (AST.CIntConst (cInteger 1) N.undefNode)) N.undefNode))) N.undefNode)
     ]

-- ^ loop_cps 
{-
void loop_cps(int (*cond)(sortctxt*),
	      void (*visitor)(void (*k)(sortctxt*), sortctxt*),
	      void (*exit)(void (*k)(sortctxt*), sortctxt*),
	      void (*k)(sortctxt *), sortctxt* ctxt) {
  
  if ((*cond)(ctxt)) {
    (*visitor)(&lambda_loop_cps, ctxt);
  } else {
    (*exit)(k,ctxt);
  }
}

-}
loopCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo 
loopCPS ctxtName fname = 
  let cond          = iid condParamName
      formalArgCond = AST.CDecl [AST.CTypeSpec intTy] -- int (*cond)(ctxt *)
                      [(Just (AST.CDeclr (Just cond) 
                              [ AST.CPtrDeclr [] N.undefNode
                              , AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode) ] 
                                                       [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] 
                                                       N.undefNode], False)
                                              ) [] N.undefNode] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode 
      visitor       = iid visitorParamName
      formalArgX x = AST.CDecl [AST.CTypeSpec voidTy] -- void (*X)(void (*k)(ctxt*), ctxt*)
                         [(Just (AST.CDeclr (Just x)
                                 [ AST.CPtrDeclr [] N.undefNode
                                 , AST.CFunDeclr (Right ([ AST.CDecl [AST.CTypeSpec voidTy] [(Just (AST.CDeclr (Just k) [AST.CPtrDeclr [] N.undefNode
                                                                                                                        ,AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)] [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode], False)) [] N.undefNode] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode
                                                         , AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)] [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode
                                                         ],False)) [] N.undefNode
                                 ] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode
      formalArgVisitor = formalArgX visitor
      exit          = iid exitParamName
      formalArgExit = formalArgX exit
      k             = iid kParamName      
      formalArgK = AST.CDecl [AST.CTypeSpec voidTy] -- void (*k)(ctxt *)
               [(Just (AST.CDeclr (Just k) 
                       [ AST.CPtrDeclr [] N.undefNode
                       , AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode) ] 
                                                [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] 
                                                N.undefNode], False)
                                       ) [] N.undefNode] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode 
      ctxt          = iid ctxtParamName                 
      formalArgCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)]  -- ctxt* ctxt
                 [(Just (AST.CDeclr (Just ctxt) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
  in fun [AST.CTypeSpec voidTy] (iid fname `app` iid "loop") [formalArgCond, formalArgVisitor, formalArgExit, formalArgK, formalArgCtxt] 
     [
       AST.CBlockStmt (AST.CIf (funCall (ind (cvar cond)) [(cvar ctxt)]) 
                       (AST.CCompound [] [AST.CBlockStmt ( AST.CExpr (Just $ funCall (ind (cvar visitor)) [ adr (cvar (iid fname `app` iid "lambda_loop"))
                                                                                                       , cvar ctxt ]) N.undefNode ) ] N.undefNode)
                       (Just (AST.CCompound [] [AST.CBlockStmt ( AST.CExpr (Just $ funCall (ind (cvar exit)) [ cvar k
                                                                                                             , cvar ctxt ]) N.undefNode) ] N.undefNode))
                       N.undefNode)
     ]
{-                 
void lambda_loop_cps(sortctxt* ctxt) {
  loop_cps(ctxt->loop_conds[ctxt->loop_stack_size],
	   ctxt->loop_visitors[ctxt->loop_stack_size],
	   ctxt->loop_exits[ctxt->loop_stack_size],
	   ctxt->loop_ks[ctxt->loop_stack_size], ctxt);
}
-}
lambdaLoopCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo 
lambdaLoopCPS ctxtName fname = 
  let ctxt          = iid ctxtParamName                 
      formalArgCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)]  -- ctxt* ctxt
                 [(Just (AST.CDeclr (Just ctxt) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
  in fun [AST.CTypeSpec voidTy] (iid fname `app` iid "lambda_loop") [formalArgCtxt] 
       [ AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (iid fname `app` iid "loop"))  
                                          [ ((cvar ctxt) .->. (iid "loop_conds")) .!!. ((cvar ctxt) .->. (iid currStackSizeName))
                                          , ((cvar ctxt) .->. (iid "loop_visitors")) .!!. ((cvar ctxt) .->. (iid currStackSizeName))
                                          , ((cvar ctxt) .->. (iid "loop_exits")) .!!. ((cvar ctxt) .->. (iid currStackSizeName))                                            
                                          , ((cvar ctxt) .->. (iid "loop_ks")) .!!. ((cvar ctxt) .->. (iid currStackSizeName)) 
                                          , (cvar ctxt)
                                          ] N.undefNode)) N.undefNode) ]
                 

{-
void id(sortctxt *ctxt) { return; }

-}

idCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo 
idCPS ctxtName fname = 
  let ctxt          = iid ctxtParamName                 
      formalArgCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)]  -- ctxt* ctxt
                 [(Just (AST.CDeclr (Just ctxt) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
  in fun [AST.CTypeSpec voidTy] (iid fname `app` iid "id") [formalArgCtxt]
     [ AST.CBlockStmt (AST.CReturn Nothing N.undefNode) ]
     


-- ^ generate function signature declaration from function definition
funSig :: AST.CFunctionDef N.NodeInfo -> AST.CDeclaration N.NodeInfo
funSig (AST.CFunDef tySpecfs declarator op_decls stmt nodeInfo) = AST.CDecl tySpecfs [(Just declarator, Nothing, Nothing)] N.undefNode
  


-- some AST boilerplate to extract parts from the function declaration
getFunReturnTy :: AST.CFunctionDef N.NodeInfo -> [AST.CDeclarationSpecifier N.NodeInfo]
getFunReturnTy (AST.CFunDef tySpecfs declarator decls stmt nodeInfo) = tySpecfs 

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


-- combinators for the ease of constructing the AST 

(.=.) :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.=.) lhs rhs = AST.CAssign AST.CAssignOp lhs rhs N.undefNode

(.->.) :: AST.CExpression N.NodeInfo -> Ident -> AST.CExpression N.NodeInfo
(.->.) struct member = AST.CMember struct member True N.undefNode

(...) :: AST.CExpression N.NodeInfo -> Ident -> AST.CExpression N.NodeInfo
(...) struct member = AST.CMember struct member False N.undefNode

-- ^ array idex
(.!!.) :: AST.CExpression N.NodeInfo ->  AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.!!.) arr idx = AST.CIndex arr idx N.undefNode

cvar :: Ident -> AST.CExpression N.NodeInfo
cvar id = AST.CVar id N.undefNode

iid :: String -> Ident
iid id = internalIdent id

ind :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo 
ind e = AST.CUnary AST.CIndOp e N.undefNode

adr :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo 
adr e = AST.CUnary AST.CAdrOp e N.undefNode

voidTy = AST.CVoidType N.undefNode
intTy  = AST.CIntType N.undefNode
boolTy = intTy


-- ^ define a function
fun :: [AST.CDeclarationSpecifier N.NodeInfo] ->  -- ^ return type
       Ident ->  -- ^ name 
       [AST.CDeclaration N.NodeInfo] -> -- ^ params
       [AST.CCompoundBlockItem N.NodeInfo] -> 
       AST.CFunctionDef N.NodeInfo
fun tySpec fname params stmts = AST.CFunDef tySpec (AST.CDeclr (Just fname) [AST.CFunDeclr (Right (params,False)) [] N.undefNode] Nothing [] N.undefNode) [] (AST.CCompound [] stmts N.undefNode) N.undefNode

-- ^ call a function
funCall :: AST.CExpression N.NodeInfo -> [AST.CExpression N.NodeInfo] -> AST.CExpression N.NodeInfo
funCall f args = AST.CCall f args N.undefNode

-- ^ making the context struct declaration
mkContext :: String -> [Ident] -> [AST.CDeclaration N.NodeInfo] -> [AST.CDeclaration N.NodeInfo] -> [AST.CDeclarationSpecifier N.NodeInfo] -> AST.CDeclaration N.NodeInfo
mkContext name labels formal_arg_decls local_var_decls returnType = -- todo the loop higher order function stack
  let structName  = iid name
      ctxtAlias   = AST.CDeclr (Just (internalIdent (map toLower name))) [] Nothing [] N.undefNode
      attrs       = []
      stackSize   = 20
      unaryFuncStack fname = AST.CDecl [AST.CTypeSpec voidTy] -- void (*loop_ks[2])(struct FuncCtxt*);
                             -- todo : these are horrible to read, can be simplified via some combinators
                             
                    [(Just (AST.CDeclr (Just $ iid fname) [ AST.CArrDeclr [] (AST.CArrSize False (AST.CConst (AST.CIntConst (cInteger stackSize) N.undefNode))) N.undefNode
                                                              , AST.CPtrDeclr [] N.undefNode
                                                              , AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CSUType (AST.CStruct AST.CStructTag (Just structName) Nothing [] N.undefNode) N.undefNode)] [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode],False)) [] N.undefNode] Nothing [] N.undefNode) ,Nothing,Nothing)] N.undefNode
      binaryFuncStack fname = AST.CDecl [AST.CTypeSpec voidTy] 
                              [(Just (AST.CDeclr (Just $ iid fname) [ AST.CArrDeclr [] (AST.CArrSize False (AST.CConst (AST.CIntConst (cInteger stackSize) N.undefNode))) N.undefNode
                                                                    , AST.CPtrDeclr [] N.undefNode
                                                                    , AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CVoidType N.undefNode)] [(Just (AST.CDeclr (Just (iid kParamName)) [AST.CPtrDeclr [] N.undefNode, AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CSUType (AST.CStruct AST.CStructTag (Just structName) Nothing [] N.undefNode) N.undefNode)] [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode],False)) [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode, AST.CDecl [AST.CTypeSpec (AST.CSUType (AST.CStruct AST.CStructTag (Just structName) Nothing [] N.undefNode) N.undefNode)] [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode],False)) [] N.undefNode] Nothing [] N.undefNode) ,Nothing,Nothing)] N.undefNode
      ksStack      = unaryFuncStack "loop_ks"
      condStack    = unaryFuncStack "loop_conds"                                    
      visitorStack = binaryFuncStack "loop_visitors"
      exitStack    = binaryFuncStack "loop_exits"
      currStackSize = AST.CDecl [AST.CTypeSpec intTy] [(Just (AST.CDeclr (Just $ iid currStackSizeName) [] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode
      funcResult    = AST.CDecl returnType [(Just (AST.CDeclr (Just $ iid "func_result") [] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode
      decls'        = formal_arg_decls ++ concatMap (\d -> renameDeclWithLabels d labels) local_var_decls ++ [ksStack, condStack, visitorStack, exitStack, currStackSize, funcResult]
      tyDef         = AST.CStorageSpec (AST.CTypedef N.undefNode)
      structDef     =
        AST.CTypeSpec (AST.CSUType
                       (AST.CStruct AST.CStructTag (Just structName) (Just decls') attrs N.undefNode) N.undefNode) 
  in AST.CDecl [tyDef, structDef] [(Just ctxtAlias, Nothing, Nothing)] N.undefNode



renameDeclWithLabels :: AST.CDeclaration N.NodeInfo -> [Ident] -> [AST.CDeclaration N.NodeInfo]
renameDeclWithLabels decl labels = map (renameDeclWithLabel decl) labels
  
renameDeclWithLabel decl label =   
  let rnState = RSt label M.empty [] 
  in case renamePure rnState decl of 
    { (decl', rstate') -> decl' }






