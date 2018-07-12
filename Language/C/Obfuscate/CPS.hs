{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Language.C.Obfuscate.CPS
       where
import Data.Char
import Data.List (nubBy)
import Data.Maybe 
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.C.Syntax.AST as AST
import qualified Language.C.Data.Node as N 
import Language.C.Syntax.Constants
import Language.C.Data.Ident

import Language.C.Obfuscate.Var
import Language.C.Obfuscate.ASTUtils
import Language.C.Obfuscate.CFG 
import Language.C.Obfuscate.SSA


-- import for testing
import Language.C (parseCFile, parseCFilePre)
import Language.C.System.GCC (newGCC)
import Language.C.Pretty (pretty)
import Text.PrettyPrint.HughesPJ (render, text, (<+>), hsep)

import System.IO.Unsafe (unsafePerformIO)


-- TODO LIST:
-- 1. check whether k vs kParam (done)
-- 2. id, loop an loop lambda (done)
-- 3. pop and push (done)
-- 4. function signatures (done)
-- 5. max stack size
-- 6. curr_stack_size + 1 or - 1

testCPS = do 
  { let opts = []
  ; ast <- errorOnLeftM "Parse Error" $ parseCFile (newGCC "gcc") Nothing opts   "test/sort.c" -- -} "test/fibiter.c"
  ; case ast of 
    { AST.CTranslUnit (AST.CFDefExt fundef:_) nodeInfo -> 
         case runCFG fundef of
           { CFGOk (_, state) -> do 
                { -- putStrLn $ show $ buildDTree (cfg state)
                ; -- putStrLn $ show $ buildDF (cfg state)
                ; let (SSA scopedDecls labelledBlocks sdom _ _) = buildSSA (cfg state) (formalArgs state)
                      visitors = allVisitors sdom labelledBlocks
                      exits    = allExits labelledBlocks
                -- ; putStrLn $ show $ visitors
                -- ; putStrLn $ show $ exits                  

                ; let cps = ssa2cps fundef (buildSSA (cfg state) (formalArgs state))
                ; putStrLn $ prettyCPS $ cps
                ; -- putStrLn $ render $ pretty (cps_ctxt cps)
                ; -- mapM_  (\d -> putStrLn $ render $ pretty d) (cps_funcsigs cps)
                ; -- mapM_  (\f -> putStrLn $ render $ pretty f) ((cps_funcs cps) ++ [cps_main cps])

                }
           ; CFGError s       -> error s
           }
    ; _ -> error "not fundec"
    }
  }

prettyCPS :: CPS -> String 
prettyCPS cps = 
  let declExts = map (\d -> AST.CDeclExt d) ([(cps_ctxt cps)] ++ (cps_funcsigs cps))
      funDeclExts = map (\f -> AST.CFDefExt f) ((cps_funcs cps) ++ [cps_main cps])
  in render $ pretty (AST.CTranslUnit (declExts ++ funDeclExts) N.undefNode)



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
-}
    
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
t[] => T*

-------------
void => void
-}
                   
{-                    
instance CPSize (AST.CDeclarationSpecifier N.NodeInfo) (AST.CDeclarationSpecifier N.NodeInfo) where
  cps_trans tyspec = tyspec -- todo: unroll it
-}
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

{-
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

cps_trans_lbs :: Bool -> -- ^ is return void
                 S.Set Ident -> -- ^ local vars
                 S.Set Ident -> -- ^ formal args
                 ContextName -> 
                 Ident ->  -- ^ top level function name
                 -- ^ \bar{\Delta} become part of the labelled block flag (loop) 
                 Visitors ->
                 Exits ->                 
                 M.Map Ident LabeledBlock ->  -- ^ \bar{b}
                 [AST.CFunctionDef N.NodeInfo] 
cps_trans_lbs isReturnVoid localVars fargs ctxtName fname visitors exits lb_map = 
  map (\(id,lb) -> cps_trans_lb isReturnVoid localVars fargs  ctxtName fname lb_map id visitors exits lb) (M.toList lb_map)

{- fn, \bar{\Delta}, \bar{b}  |- b => P -}


cps_trans_lb :: Bool -> -- ^ is return void
                S.Set Ident -> -- ^ local vars
                S.Set Ident -> -- ^ formal args
                ContextName -> 
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


cps_trans_lb isReturnVoid localVars fargs ctxtName fname lb_map ident visitors exits lb  = 
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
      stmt' =  AST.CCompound [] (pop ++ (cps_trans_stmts isReturnVoid localVars fargs ctxtName fname k lb_map ident (lb_loop lb) visitors exits (lb_stmts lb))) N.undefNode      
  in AST.CFunDef tyVoid (AST.CDeclr (Just fname') decltrs mb_strLitr attrs N.undefNode) declrs stmt' N.undefNode
    
     
     
cps_trans_stmts :: Bool -> -- ^ is return type void
                   S.Set Ident -> -- ^ local vars
                   S.Set Ident -> -- ^ formal args
                   ContextName -> 
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
cps_trans_stmts isReturnVoid localVars fargs ctxtName fname k lb_map ident inDelta visitors exits stmts = -- todo: maybe pattern match the CCompound constructor here?
  concatMap (\stmt -> cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k lb_map ident inDelta visitors exits stmt) stmts 



-- fn, K, \bar{\Delta}, \bar{b} |-_l s => S
cps_trans_stmt :: Bool -> -- ^ is return type void
                  S.Set Ident -> -- ^ local vars
                  S.Set Ident -> -- ^ formal args
                  ContextName -> 
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
cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k lb_map ident inDelta visitors exits (AST.CBlockStmt (AST.CGoto li nodeInfo)) = case M.lookup li lb_map of 
  { Just lb | not (null (lb_phis lb)) -> 
       let asgmts  = cps_trans_phis ctxtName ident li (lb_phis lb)
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
               { Just loop_lbl | li == loop_lbl -> AST.CBlockStmt (AST.CExpr (Just (AST.CCall (ind $ cvar k) [cvar (iid ctxtParamName)] N.undefNode)) N.undefNode) 
               ; _ -> AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar fname') args N.undefNode)) N.undefNode)
               }
         in [ funcall ]
  ; Nothing -> error "cps_trans_stmt failed at a non existent label."
  }
                                                                                        

cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k lb_map ident inDelta visitors exits (AST.CBlockStmt (AST.CExpr (Just e) nodeInfo)) = 
  case e of 
    -- todo: deal with array element assignment, we need to assign the array first. we should do it at the block level
   { AST.CAssign op lval rval ni -> 
{-                                                                                        
x => X; e => E; fn, K, \bar{\Delta}, \bar{b} |-_l s => S
----------------------------------------------------------------------------- (SeqAssign)
fn, K, \bar{\Delta}, \bar{b} |-_l x = e;s => X = E;S
-}             
     let e' = cps_trans_exp localVars fargs ctxtName e
     in [AST.CBlockStmt (AST.CExpr (Just e') nodeInfo)]
   ; _ -> 
{-                                                                                        
e => E; fn, K, \bar{\Delta}, \bar{b} |-_l s => S
----------------------------------------------------------------------------- (SeqE)
fn, K, \bar{\Delta}, \bar{b} |-_l e;s => E;S
-}     
     let e' = cps_trans_exp localVars fargs ctxtName e
     in [AST.CBlockStmt (AST.CExpr (Just e') nodeInfo)]
   }
{-
----------------------------------------------------------------------------- (returnNil)
fn, K, \bar{\Delta}, \bar{b} |-_l return; =>  id(); 
-}
-- C does not support higher order function.
-- K is passed in as a formal arg
-- K is a pointer to function
-- updated: change from K(); to id(); because of return in a loop, (see noreturn.c)
cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k lb_map ident inDelta visitors exits (AST.CBlockStmt (AST.CReturn Nothing nodeInfo)) = 
  let funcall = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` (iid "id"))) [(cvar (iid ctxtParamName))] N.undefNode)) N.undefNode)
  in [ funcall ]

{-
e => E
----------------------------------------------------------------------------- (returnE)
fn, K, \bar{\Delta}, \bar{b} |-_l return e; => x_r = E;  id()
-}
-- C does not support higher order function.
-- x_r and K belong to the contxt
-- K is a pointer to function
cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k lb_map ident inDelta visitors exits (AST.CBlockStmt (AST.CReturn (Just e) nodeInfo)) 
  | isReturnVoid = 
    let funcall = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` (iid "id"))) [(cvar (iid ctxtParamName))] N.undefNode)) N.undefNode) 
    in [ funcall ]
  | otherwise = 
      let funcall = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` (iid "id"))) [(cvar (iid ctxtParamName))] N.undefNode)) N.undefNode)
          e' = cps_trans_exp localVars fargs ctxtName e
          assign = ((cvar (iid ctxtParamName)) .->. (iid "func_result")) .=. e' 
      in [ AST.CBlockStmt (AST.CExpr (Just assign) nodeInfo), funcall ]
  
     

cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k lb_map ident inDelta  visitors exits (AST.CBlockStmt (AST.CIf exp trueStmt mbFalseStmt nodeInfo)) 
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
    let exp' = cps_trans_exp localVars fargs ctxtName exp
        (lbl_tr, lb_tr) = case trueStmt of 
          { AST.CGoto lbl _ -> case M.lookup lbl lb_map of 
               { Nothing -> error "cps_trans_stmt error: label block is not found in the true statement in a loop."
               ; Just lb -> (lbl, lb)
               }
          ; _ -> error "cps_trans_stmt error: the true statement in a looping if statement does not contain goto"
          }
        asgmts_tr = cps_trans_phis ctxtName ident lbl_tr (lb_phis lb_tr)
        (lbl_fl, lb_fl) = case mbFalseStmt of 
          { Just (AST.CGoto lbl _) -> case M.lookup lbl lb_map of 
               { Nothing -> error "cps_trans_stmt error: label block is not found in the false statement in a loop."
               ; Just lb -> (lbl, lb)
               }
          ; _ -> error "cps_trans_stmt error: the false statement in a looping if statement does not contain goto"
          }
        asgmts_fl = cps_trans_phis ctxtName ident lbl_fl (lb_phis lb_fl)
        -- supposed to push asgmts_tr and asgmnts_fl to before f_l1 and f_l2, but does not work here.
        push_args :: [AST.CExpression N.NodeInfo]
        push_args = [ AST.CUnary AST.CAdrOp (cvar $ fname `app` (iid "cond") `app` ident) N.undefNode
                    , AST.CUnary AST.CAdrOp (cvar (fname `app` lbl_tr)) N.undefNode
                    , AST.CUnary AST.CAdrOp (cvar (fname `app` lbl_fl)) N.undefNode
                    , cvar k
                    , cvar (iid ctxtParamName)
                    ]
        push = fname `app` (iid "push")
        call_push = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar push) push_args N.undefNode)) N.undefNode)
        
        loop =  fname `app` iid "loop__entry"
        loop_args  = [ (cvar (iid ctxtParamName) .->. (iid "loop_conds")) .!!. (cvar (iid ctxtParamName) .->. (iid currStackSizeName))
                     , (cvar (iid ctxtParamName) .->. (iid "loop_visitors")) .!!. (cvar (iid ctxtParamName) .->. (iid currStackSizeName))
                     , (cvar (iid ctxtParamName) .->. (iid "loop_exits")) .!!. (cvar (iid ctxtParamName) .->. (iid currStackSizeName))
                     , (cvar (iid ctxtParamName) .->. (iid "loop_ks")) .!!. (cvar (iid ctxtParamName) .->. (iid currStackSizeName))
                     , cvar (iid ctxtParamName)
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
          { AST.CCompound ids items ni -> AST.CCompound ids (cps_trans_stmts isReturnVoid localVars fargs ctxtName fname k lb_map ident inDelta visitors exits items) ni 
          ; _ -> case cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k lb_map ident inDelta visitors exits (AST.CBlockStmt trueStmt) of 
            { [ AST.CBlockStmt trueStmt'' ] -> trueStmt''
            ; items -> AST.CCompound [] items N.undefNode
            }
          }
        mbFalseStmt' = case mbFalseStmt of 
          { Nothing        -> Nothing 
          ; Just (AST.CCompound ids items ni) -> Just $ AST.CCompound ids (cps_trans_stmts isReturnVoid localVars fargs ctxtName fname k lb_map ident inDelta visitors exits items) ni   
          ; Just falseStmt -> Just $ case cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k lb_map ident inDelta visitors exits (AST.CBlockStmt falseStmt) of 
            { [  AST.CBlockStmt falseStmt'' ] -> falseStmt'' 
            ; items -> AST.CCompound [] items N.undefNode
            }
          }
        exp' = cps_trans_exp localVars fargs ctxtName exp
    in [AST.CBlockStmt (AST.CIf exp' trueStmt' mbFalseStmt' nodeInfo)]

cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k lb_map ident inDelta visitors exits (AST.CBlockStmt (AST.CCompound ids stmts nodeInfo)) = 
  -- todo: do we need to do anything with the ids (local labels)?
  let stmts' = cps_trans_stmts isReturnVoid localVars fargs ctxtName fname k lb_map ident inDelta visitors exits stmts
  in [AST.CBlockStmt (AST.CCompound ids stmts' nodeInfo)]
  

cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k lb_map ident inDelta visitors exits stmt = 
  error ("cps_trans_stmt error: unhandled case" ++ (show stmt)) -- (render $ pretty stmt))
     


cps_trans_phis ::  ContextName ->
                   Ident -> -- ^ source block label (where goto is invoked)
                   Ident -> -- ^ destination block label (where goto is jumping to)
                   [( Ident -- ^ var being redefined 
                    , [(Ident, Maybe Ident)])] ->  -- ^ incoming block x renamed variables
                   [AST.CCompoundBlockItem N.NodeInfo]
cps_trans_phis ctxtName src_lb dest_lb ps = map (cps_trans_phi ctxtName src_lb dest_lb) ps






{-
---------------------------------------
l_s, l_d |- \bar{i} => \bar{x = e}
-}

cps_trans_phi :: ContextName -> 
                 Ident -> -- ^ source block label (where goto is invoked)
                 Ident -> -- ^ destination block label (where goto is jumping to)
                 (Ident, [(Ident, Maybe Ident)]) -> 
                 AST.CCompoundBlockItem N.NodeInfo
cps_trans_phi ctxtName src_lb dest_lb (var, pairs) = 
  case lookup src_lb pairs of -- look for the matching label according to the source label
    { Nothing           -> error "cps_trans_phi failed: can't find the source label from the incoming block labels."
    ; Just redefined_lb -> -- lbl in which the var is redefined (it could be the precedence of src_lb)
      let lhs = (cvar (iid ctxtParamName)) .->. (var `app` dest_lb)
          rhs = (cvar (iid ctxtParamName)) .->. case redefined_lb of  
            { Just l -> (var `app` l)
            ; Nothing -> var 
            }
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
cps_trans_exp :: S.Set Ident -> S.Set Ident -> ContextName -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CAssign op lhs rhs nodeInfo)    = AST.CAssign op (cps_trans_exp localVars fargs ctxtName lhs) (cps_trans_exp localVars fargs ctxtName rhs) nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CComma es nodeInfo)             = AST.CComma (map (cps_trans_exp localVars fargs ctxtName) es) nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CCond e1 Nothing e3 nodeInfo)   = AST.CCond (cps_trans_exp localVars fargs ctxtName e1) Nothing (cps_trans_exp localVars fargs ctxtName e3) nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CCond e1 (Just e2) e3 nodeInfo) = AST.CCond (cps_trans_exp localVars fargs ctxtName e1) (Just $ cps_trans_exp localVars fargs ctxtName e2) (cps_trans_exp localVars fargs ctxtName e3) nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CBinary op e1 e2 nodeInfo)  = AST.CBinary op (cps_trans_exp localVars fargs ctxtName e1)  (cps_trans_exp localVars fargs ctxtName e2) nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CCast decl e nodeInfo)      = AST.CCast decl (cps_trans_exp localVars fargs ctxtName e) nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CUnary op e nodeInfo)       = AST.CUnary op (cps_trans_exp localVars fargs ctxtName e) nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CSizeofExpr e nodeInfo)     = AST.CSizeofExpr (cps_trans_exp localVars fargs ctxtName e) nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CSizeofType decl nodeInfo)  = AST.CSizeofType decl nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CAlignofExpr e nodeInfo)    = AST.CAlignofExpr (cps_trans_exp localVars fargs ctxtName e) nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CAlignofType decl nodeInfo) = AST.CAlignofType decl nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CComplexReal e nodeInfo)    = AST.CComplexReal (cps_trans_exp localVars fargs ctxtName e) nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CComplexImag e nodeInfo)    = AST.CComplexImag (cps_trans_exp localVars fargs ctxtName e) nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CIndex arr idx nodeInfo)    = AST.CIndex (cps_trans_exp localVars fargs ctxtName arr) (cps_trans_exp localVars fargs ctxtName idx) nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CCall f args nodeInfo)      = AST.CCall (cps_trans_exp localVars fargs ctxtName f) (map (cps_trans_exp localVars fargs ctxtName) args) nodeInfo 
cps_trans_exp localVars fargs ctxtName (AST.CMember e ident deref nodeInfo) = AST.CMember  (cps_trans_exp localVars fargs ctxtName e) ident deref nodeInfo
cps_trans_exp localVars fargs ctxtName (AST.CVar id _) =  
  case unApp id of
    { Nothing -> cvar id
    ; Just id' | id' `S.member` (localVars `S.union` fargs) -> 
      -- let io = unsafePerformIO $ print (localVars `S.union` fargs) >> print id'
      -- in io `seq`
      (cvar (iid ctxtParamName)) .->. id
               | otherwise -> cvar id -- global
    }

cps_trans_exp localVars fargs ctxtName (AST.CConst c)                       = AST.CConst c
cps_trans_exp localVars fargs ctxtName (AST.CCompoundLit decl initList nodeInfo) = AST.CCompoundLit decl initList nodeInfo -- todo check this
cps_trans_exp localVars fargs ctxtName (AST.CStatExpr stmt nodeInfo )       = AST.CStatExpr stmt nodeInfo  -- todo GNU C compount statement as expr
cps_trans_exp localVars fargs ctxtName (AST.CLabAddrExpr ident nodeInfo )   = AST.CLabAddrExpr ident nodeInfo -- todo  
cps_trans_exp localVars fargs ctxtName (AST.CBuiltinExpr builtin )          = AST.CBuiltinExpr builtin -- todo build in expression

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
ssa2cps fundef (SSA scopedDecls labelledBlocks sdom local_decl_vars fargs) = 
  let -- scraping the information from the top level function under obfuscation
      funName        = case getFunName fundef of { Just s -> s ; Nothing -> "unanmed" }
      formalArgDecls :: [AST.CDeclaration N.NodeInfo]
      formalArgDecls = getFormalArgs fundef
      formalArgIds :: [Ident]
      formalArgIds   = concatMap (\declaration -> getFormalArgIds declaration) formalArgDecls
      (returnTy,ptrArrs) = getFunReturnTy fundef
      isReturnVoid       = isVoidDeclSpec returnTy
      ctxtStructName     = funName ++ "Ctxt"
      
      -- the context struct declaration
      context        = mkContext ctxtStructName labelledBlocks formalArgDecls scopedDecls returnTy ptrArrs local_decl_vars fargs
      ctxtName       = map toLower ctxtStructName -- alias name is inlower case and will be used in the the rest of the code
      
      -- finding the visitor labels and the exit labels
      visitors       = allVisitors sdom labelledBlocks
      exits          = allExits labelledBlocks
      
      -- all the conditional function 
      conds          = allLoopConds local_decl_vars fargs ctxtName funName labelledBlocks 
      
      -- loop_cps, loop_lambda, id and pop and push
      loop_cps        = loopCPS ctxtName funName
      lambda_loop_cps = lambdaLoopCPS ctxtName funName
      id_cps          = idCPS ctxtName funName
      push_cps        = pushCPS ctxtName funName
      pop_cps         = popCPS ctxtName funName
      
      -- all the "nested/helper" function declarations 
      -- todo: packing all the variable into a record 
      ps              = cps_trans_lbs isReturnVoid  local_decl_vars fargs ctxtName (iid funName) {- (iid "id") -} visitors exits labelledBlocks 
      
      -- all function signatures
      funcSignatures  = map funSig (ps ++ conds ++ [loop_cps, lambda_loop_cps, id_cps, push_cps, pop_cps, fundef]) -- include the source func, in case of recursion
      main_decls = 
        -- 1. malloc the context obj in the main func
        -- ctxtTy * ctxt = (ctxtTy *) malloc(sizeof(ctxtTy));
        [ AST.CBlockDecl (AST.CDecl 
                          [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)] 
                          [(Just (AST.CDeclr (Just (iid ctxtParamName)) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),
                            Just (AST.CInitExpr 
                                  (AST.CCast (AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)] 
                                              [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode) 
                                   (AST.CCall (AST.CVar (iid "malloc") N.undefNode) 
                                    [AST.CSizeofType (AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)] [] N.undefNode) N.undefNode] N.undefNode) N.undefNode) N.undefNode),Nothing)] N.undefNode)
        ] 
      main_stmts = 
        -- 2. initialize the counter-part  in the context of the formal args
        -- forall arg. ctxt->arg_label0 = arg 
        [ AST.CBlockStmt (AST.CExpr (Just (((cvar (iid ctxtParamName)) .->. (arg `app` (iid $ labPref ++ "0" ))) .=. (AST.CVar arg N.undefNode))) N.undefNode) | arg <- formalArgIds] ++ 
        -- 3. initialize the collection which are the local vars, e.g. a locally declared array
        --  int a[3];
        -- will be reinitialized as ctxt->a_0 = (int *)malloc(sizeof(int)*3);
        [ AST.CBlockStmt (AST.CExpr (Just (((cvar (iid ctxtParamName)) .->. (var `app` (iid $ labPref ++ "0" ))) .=. rhs)) N.undefNode) 
        | scopedDecl <- scopedDecls
        , isJust (containerDeclToInit scopedDecl)
        , let (Just (var,rhs)) = containerDeclToInit scopedDecl ] ++ 
        
        -- 4. initialize the context->curr_stack_size = 0;
        [ AST.CBlockStmt (AST.CExpr (Just (((cvar (iid ctxtParamName)) .->. (iid currStackSizeName) .=. (AST.CConst (AST.CIntConst (cInteger 0) N.undefNode))))) N.undefNode) ] ++ 
        -- 3. calling block 0
        [ AST.CBlockStmt (AST.CExpr (Just (AST.CCall (AST.CVar ((iid funName) `app` (iid (labPref ++ "0" ))) N.undefNode) 
                                           [ AST.CUnary AST.CAdrOp (AST.CVar ((iid funName) `app` (iid "id")) N.undefNode) N.undefNode
                                           , AST.CVar (iid ctxtParamName) N.undefNode ] N.undefNode)) N.undefNode)
        , if isReturnVoid 
          then AST.CBlockStmt (AST.CReturn Nothing N.undefNode) 
          else AST.CBlockStmt (AST.CReturn (Just $ (cvar (iid ctxtParamName)) .->. (iid "func_result")) N.undefNode)
        ]
        
      main_func = case fundef of 
        { AST.CFunDef tySpecfs declarator decls _ nodeInfo -> 
             AST.CFunDef tySpecfs declarator decls (AST.CCompound [] (main_decls ++ main_stmts) N.undefNode) nodeInfo 
        }
  in CPS main_decls main_stmts funcSignatures (ps ++ conds ++ [loop_cps, lambda_loop_cps, id_cps, push_cps, pop_cps])  context main_func 
     
-- ^ turn a scope declaration into a rhs initalization.     
-- ^ refer to local_array.c
containerDeclToInit :: AST.CDeclaration N.NodeInfo -> Maybe (Ident, AST.CExpression N.NodeInfo)
containerDeclToInit (AST.CDecl typespecs tripls nodeInfo0) = case tripls of 
  { (Just decl@(AST.CDeclr (Just arrName) [arrDecl] _ _ _), _, _):_ -> 
       case arrDecl of 
         { AST.CArrDeclr _ (AST.CArrSize _ size) _ -> 
              let 
                ptrToTy = AST.CDecl typespecs [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
                malloc = AST.CCall (AST.CVar (iid "malloc") N.undefNode) 
                         [AST.CBinary AST.CMulOp (AST.CSizeofType (AST.CDecl typespecs [] N.undefNode) N.undefNode) size N.undefNode] N.undefNode
                cast = AST.CCast ptrToTy malloc N.undefNode
              in Just (arrName, cast)
         ; _ -> Nothing
         }
  ; _ -> Nothing
  }
  
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
allLoopConds :: S.Set Ident -> S.Set Ident -> ContextName -> String ->  M.Map NodeId LabeledBlock -> [AST.CFunctionDef N.NodeInfo]
allLoopConds local_vars fargs ctxtName fname lbs = 
  concatMap (\(id,lb) -> loopCond local_vars fargs ctxtName fname (id,lb)) (M.toList lbs)

loopCond :: S.Set Ident -> S.Set Ident -> ContextName -> String -> (NodeId, LabeledBlock) -> [AST.CFunctionDef N.NodeInfo]
loopCond  local_vars fargs ctxtName fname (l,blk) 
  | lb_loop blk =
    let stmts = lb_stmts blk
        conds = [ cps_trans_exp local_vars fargs ctxtName e | AST.CBlockStmt (AST.CIf e tt ff _) <- stmts ]
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
  in fun [AST.CTypeSpec voidTy] (iid fname `app` iid "loop__entry") [formalArgCond, formalArgVisitor, formalArgExit, formalArgK, formalArgCtxt] 
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
       [ AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (iid fname `app` iid "loop__entry"))  
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
  



-- ^ making the context struct declaration
mkContext :: String -> -- ^ context name
             M.Map Ident LabeledBlock ->  -- ^ labeled blocks
             [AST.CDeclaration N.NodeInfo] ->  -- ^ formal arguments
             [AST.CDeclaration N.NodeInfo] ->  -- ^ local variable declarations
             [AST.CDeclarationSpecifier N.NodeInfo] ->  -- ^ return Type
             [AST.CDerivedDeclarator N.NodeInfo] -> -- ^ the pointer or array postfix
             S.Set Ident -> S.Set Ident -> 
             AST.CDeclaration N.NodeInfo
mkContext name labeledBlocks formal_arg_decls local_var_decls returnType ptrArrs local_decl_vars fargs = 
  let structName  = iid name
      ctxtAlias   = AST.CDeclr (Just (internalIdent (map toLower name))) [] Nothing [] N.undefNode
      attrs       = []
      stackSize   = 20
      
      isReturnVoid   = isVoidDeclSpec returnType
      unaryFuncStack ty fname = AST.CDecl [AST.CTypeSpec ty] -- void (*loop_ks[2])(struct FuncCtxt*);
                             -- todo : these are horrible to read, can be simplified via some combinators
                             
                    [(Just (AST.CDeclr (Just $ iid fname) [ AST.CArrDeclr [] (AST.CArrSize False (AST.CConst (AST.CIntConst (cInteger stackSize) N.undefNode))) N.undefNode
                                                          , AST.CPtrDeclr [] N.undefNode
                                                          , AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CSUType (AST.CStruct AST.CStructTag (Just structName) Nothing [] N.undefNode) N.undefNode)] [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode],False)) [] N.undefNode] Nothing [] N.undefNode) ,Nothing,Nothing)] N.undefNode
      binaryFuncStack fname = AST.CDecl [AST.CTypeSpec voidTy] 
                              [(Just (AST.CDeclr (Just $ iid fname) [ AST.CArrDeclr [] (AST.CArrSize False (AST.CConst (AST.CIntConst (cInteger stackSize) N.undefNode))) N.undefNode
                                                                    , AST.CPtrDeclr [] N.undefNode
                                                                    , AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CVoidType N.undefNode)] [(Just (AST.CDeclr (Just (iid kParamName)) [AST.CPtrDeclr [] N.undefNode, AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CSUType (AST.CStruct AST.CStructTag (Just structName) Nothing [] N.undefNode) N.undefNode)] [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode],False)) [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode, AST.CDecl [AST.CTypeSpec (AST.CSUType (AST.CStruct AST.CStructTag (Just structName) Nothing [] N.undefNode) N.undefNode)] [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode],False)) [] N.undefNode] Nothing [] N.undefNode) ,Nothing,Nothing)] N.undefNode
      ksStack      = unaryFuncStack voidTy "loop_ks"
      condStack    = unaryFuncStack intTy "loop_conds"                                    
      visitorStack = binaryFuncStack "loop_visitors"
      exitStack    = binaryFuncStack "loop_exits"
      currStackSize = AST.CDecl [AST.CTypeSpec intTy] [(Just (AST.CDeclr (Just $ iid currStackSizeName) [] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode
      funcResult | isReturnVoid = [] 
                 | otherwise    = [AST.CDecl returnType [(Just (AST.CDeclr (Just $ iid "func_result") ptrArrs Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode]
      decls'        = -- formal_arg_decls ++ 
        -- note: we remove local decl duplicate, maybe we should let different label block to have different type decl in the ctxt, see test/scoped_dup_var.c
                      concatMap (\d -> renameDeclWithLabeledBlocks d labeledBlocks local_decl_vars fargs) (nubBy declLHSEq $ map (cps_trans_declaration . dropConstTyQual) (formal_arg_decls ++ local_var_decls)) ++ 
                      [ksStack, condStack, visitorStack, exitStack, currStackSize] ++ funcResult
      tyDef         = AST.CStorageSpec (AST.CTypedef N.undefNode)
      structDef     =
        AST.CTypeSpec (AST.CSUType
                       (AST.CStruct AST.CStructTag (Just structName) (Just decls') attrs N.undefNode) N.undefNode) 
  in AST.CDecl [tyDef, structDef] [(Just ctxtAlias, Nothing, Nothing)] N.undefNode

-- eq for the declaration nub, see the above
declLHSEq (AST.CDecl declSpecifiers1 trips1 _) (AST.CDecl declSpecifiers2 trips2 _) = 
  let getIds trips = map (\(mb_decl, mb_init, mb_size) -> case mb_decl of 
                             { Just (AST.CDeclr mb_id derivedDeclarators mb_strLit attrs nInfo) -> mb_id 
                             ; Nothing -> Nothing }) trips
  in (getIds trips1) == (getIds trips2)

-- we need to drop the constant type specifier since we need to initialize them in block 0, see test/const.c
dropConstTyQual :: AST.CDeclaration N.NodeInfo -> AST.CDeclaration N.NodeInfo 
dropConstTyQual decl = case decl of                    
  { AST.CDecl declSpecifiers trips ni -> AST.CDecl (filter (not . isConst) declSpecifiers) trips ni }
  where isConst (AST.CTypeQual (AST.CConstQual _)) = True
        isConst _ = False


-- renameDeclWithLabels :: AST.CDeclaration N.NodeInfo -> [Ident] -> [AST.CDeclaration N.NodeInfo]
-- renameDeclWithLabels decl labels = map (renameDeclWithLabel decl) labels


renameDeclWithLabeledBlocks :: AST.CDeclaration N.NodeInfo -> M.Map Ident LabeledBlock -> S.Set Ident -> S.Set Ident -> [AST.CDeclaration N.NodeInfo]
renameDeclWithLabeledBlocks decl labeledBlocks local_decl_vars fargs = 
  let idents = getFormalArgIds decl
  in do 
    { ident    <- idents
    ; (lb,blk) <- M.toList labeledBlocks 
    ; if (null (lb_preds blk)) ||  -- it's the entry block
         (ident `elem` (lb_lvars blk)) ||  -- the var is in the lvars
         (ident `elem` (map fst (lb_phis blk))) || -- the var is in the phi
         (ident `elem` (lb_containers blk)) -- the var is one of the lhs container ids such as array or members
      then return (renameDeclWithLabel decl lb local_decl_vars fargs) 
      else []
    }


  
renameDeclWithLabel decl label local_decl_vars fargs =   
  let rnState = RSt label M.empty [] [] local_decl_vars fargs
  in case renamePure rnState decl of 
    { (decl', rstate', containers) -> decl' }


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
t[] => T*

-------------
void => void
-}
                   
cps_trans_declaration :: AST.CDeclaration N.NodeInfo -> AST.CDeclaration N.NodeInfo
cps_trans_declaration (AST.CDecl declSpecifiers trips ni) = 
  AST.CDecl (map cps_trans_declspec declSpecifiers) (map (\(mb_decl, mb_init, mb_size) -> 
                                                           let mb_decl' = case mb_decl of
                                                                 { Nothing -> Nothing
                                                                 ; Just decl -> Just (cps_trans_decltr decl)
                                                                 }
                                                           in (mb_decl', mb_init, mb_size)) trips) ni
     
-- lhs of a declaration     
cps_trans_declspec :: AST.CDeclarationSpecifier N.NodeInfo -> AST.CDeclarationSpecifier N.NodeInfo
cps_trans_declspec (AST.CStorageSpec storageSpec) = AST.CStorageSpec storageSpec -- auto, register, static, extern etc
cps_trans_declspec (AST.CTypeSpec tySpec) = AST.CTypeSpec tySpec -- simple type, void, int, bool etc
cps_trans_declspec (AST.CTypeQual tyQual) = AST.CTypeQual tyQual -- qual, CTypeQual, CVolatQual, etc
-- cps_trans_declspec (AST.CFunSpec  funSpec) = AST.CFunSpec funSpec -- todo 
-- cps_trans_declspec (AST.CAlignSpec alignSpec) = AST.CAlignSpec alignSpec -- todo

-- rhs (after the variable)
cps_trans_decltr :: AST.CDeclarator N.NodeInfo -> AST.CDeclarator N.NodeInfo 
cps_trans_decltr (AST.CDeclr mb_id derivedDeclarators mb_strLit attrs nInfo) = 
  AST.CDeclr mb_id (map cps_trans_derived_decltr derivedDeclarators) mb_strLit attrs nInfo
  
cps_trans_derived_decltr :: AST.CDerivedDeclarator N.NodeInfo -> AST.CDerivedDeclarator N.NodeInfo
cps_trans_derived_decltr (AST.CPtrDeclr tyQuals ni) = AST.CPtrDeclr tyQuals ni
cps_trans_derived_decltr (AST.CArrDeclr tyQuals arrSize ni) = AST.CPtrDeclr tyQuals ni
cps_trans_derived_decltr (AST.CFunDeclr either_id_decls  attrs ni) = AST.CFunDeclr either_id_decls  attrs ni



{-
data CDeclarationSpecifier a
  = CStorageSpec (CStorageSpecifier a) -- ^ storage-class specifier or typedef
  | CTypeSpec    (CTypeSpecifier a)    -- ^ type name
  | CTypeQual    (CTypeQualifier a)    -- ^ type qualifier
  | CFunSpec     (CFunctionSpecifier a) -- ^ function specifier
  | CAlignSpec   (CAlignmentSpecifier a) -- ^ alignment specifier
    deriving (Show, Data,Typeable {-! ,CNode ,Functor, Annotated !-})
-}
  

