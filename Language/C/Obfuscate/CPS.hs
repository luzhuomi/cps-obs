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


-- ^ a CPS function declaration AST
data CPS = CPS { cps_decls :: [AST.CCompoundBlockItem N.NodeInfo]  -- ^ main function decls
               , cps_stmts :: [AST.CCompoundBlockItem N.NodeInfo]  -- ^ main function stmts
               , cps_funcsigs :: [AST.CDeclaration N.NodeInfo] -- ^ the signatures decls of the auxillary functions
               , cps_funcs :: [AST.CFunctionDef N.NodeInfo] -- ^ the auxillary functions
               , cps_ctxt  :: AST.CDeclaration N.NodeInfo -- ^ the context for the closure
               , cps_main ::  AST.CFunctionDef N.NodeInfo -- ^ the main function
               } deriving Show
                          
-- global (parameter) name prefices

bindName = "bind"
bindPushName = "bind_push"
bindPeekMName = "bind_peek_m"
bindPeekFName = "bind_peek_f"
bindPopName = "bind_pop"
lambdaBindName = "lambda_bind"

ifcCondName = "ifcCond"
ifcTrName = "ifcTr"
ifcFlName = "ifcFl"
ifcName   = "ifc"

ifcPushName = "ifc_push"
ifcPeekCName = "ifc_peek_c"
ifcPeekTrName = "ifc_peek_tr"
ifcPeekFlName = "ifc_peek_fl"
ifcPopName = "ifc_pop"
lambdaIfcName = "lambda_ifc"


loopCondName = "loopCond"
loopVisitName = "loopVisit"
loopExitName = "loopExit"
loopName   = "loop"

loopPushName = "loop_push"
loopPeekCName = "loop_peek_c"
loopPeekVName = "loop_peek_v"
loopPeekEName = "loop_peek_e"
loopPopName = "loop_pop"
lambdaLoopName = "lambda_loop"


kPeekName = "k_peek"
kPopName = "k_pop"


retName = "ret"

ctxt_arr_bool = "ctxt_arr_bool"
ctxt_arr_void = "ctxt_arr_void"

ctxtParamName = "ctxtParam"                          

kParamName = "kParam"
mParamName = "mParam"
fParamName = "fParam"

condParamName = "condParam"
visitorParamName = "visitorParam"
exitParamName = "exitParam"
trueParamName = "trueParam"
falseParamName = "falseParam"

kStackTop = "k_stack_top"
loopStackTop = "loop_stack_top"
ifcStackTop = "ifc_stack_top"
bindStackTop = "bind_stack_top"

kStackName = "k_stack"
loopStackCName = "loop_stack_c"
loopStackVName = "loop_stack_v"
loopStackEName = "loop_stack_e"

ifcStackCName = "ifc_stack_c"
ifcStackTrName = "ifc_stack_tr"
ifcStackFlName = "ifc_stack_fl"

bindStackFName = "bind_stack_f"
bindStackMName = "bind_stack_m"




-- todo : remove
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


-- CL_cps
cps_trans_lb :: Bool -> -- ^ is return void
                S.Set Ident -> -- ^ local vars
                S.Set Ident -> -- ^ formal args
                ContextName -> 
                Ident ->  -- ^ top level function name
                -- 
                Ident ->  -- ^ label for the current block                
                M.Map Ident LabeledBlock ->  -- ^ KEnv
                SSA -> -- ^ functions as the cfg
                ([AST.CFunctionDef N.NodeInfo], AST.CExpression N.NodeInfo)

cps_trans_lb isReturnVoid localVars fargs ctxtName fname l_i kenv cfg = 
  case adjacent cfg l_i of 
    { [ l_j ] -> 
{-
-- in target langauge
CL_cps l_i kenv cfg 
   | \exists l_j: (l_i,l_j) \in cfg /\ 
     \not (\exists l_k : l_j \= l_k /\ (l_i, l_k) \in cfg) 
   = (D::\overline{D}, bind(f_i,E))  
   where (\overline{D}, E) = CL_cps l_j kenv cfg
         (\overline{phi}, s) = kenv l_i
         k is a fresh variable
         S = CS_cps s kenv l_i k
         D = (void => void) => void f_i = (void => void k) => { S }
-- in c 
CL_cps l_i kenv cfg 
   | \exists l_j: (l_i,l_j) \in cfg /\ 
     \not (\exists l_k : l_j \= l_k /\ (l_i, l_k) \in cfg) 
   = (D::Bind_i::\overline{D}, bind_i) 
   where (\overline{D}, E) = CL_cps l_j kenv cfg
         (\overline{phi}, s) = kenv l_i
         k is a fresh variable
         S = CS_cps s kenv l_i k
         D = void f_i (ctxt* c) { S }
         Bind_i = void bind_i (ctxt* c) { bind_push(&f_i, E, c); lambda_bind(c); }
-}

         let f_iname = fname `app` l_i
             tyVoid = [AST.CTypeSpec (AST.CVoidType N.undefNode)]
             declrs = []
             paramCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)]  -- ctxt* ctxt
                         [(Just (AST.CDeclr (Just (iid ctxtParamName)) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
             paramDeclr = [paramCtxt]      
             decltrs = [AST.CFunDeclr (Right (paramDeclr,False)) [] N.undefNode] 
             mb_strLitr = Nothing
             attrs  =  []
             
             k      = iid kParamName
             
             lb     = fromJust (M.lookup l_i kenv)
             stmt' =  AST.CCompound [] ((cps_trans_stmts isReturnVoid localVars fargs ctxtName fname k kenv l_i (lb_stmts lb)) {- ++ [peek, pop, kApp] -}) N.undefNode 
             f_iD   = AST.CFunDef tyVoid (AST.CDeclr (Just f_iname) decltrs mb_strLitr attrs N.undefNode) declrs stmt' N.undefNode
                          
             (ds,e) = cps_trans_lb isReturnVoid localVars fargs ctxtName fname l_j kenv cfg
             -- NOTE: each bind, loop and ifc, ret operators need to be "qualified" by the function name
             bind  = fname `app` iid bindName
             bindpush = fname `app` iid bindPushName
             lambdabind = fname `app` iid lambdaBindName
             bind_i = bind `app` l_i
             bindPush = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` bindpush)) [ (adr $ cvar f_iname), (adr e),(cvar $ iid ctxtParamName)] N.undefNode)) N.undefNode) -- bind_push(&f_i, &e, ctxt)
             lambdaBindApp = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` lambdabind)) [(cvar $ iid ctxtParamName)] N.undefNode)) N.undefNode) -- lambdaBind(ctxt)
             bindStmt = AST.CCompound [] [ bindPush, lambdaBindApp] N.undefNode
             bind_iD = AST.CFunDef tyVoid (AST.CDeclr (Just bind_i) decltrs mb_strLitr attrs N.undefNode) declrs bindStmt N.undefNode
             
        in (f_iD:bind_iD:ds, cvar bind_i) 
    ; [ l_j, l_k ] | pathExists cfg l_j l_i && pathExists cfg l_k l_i -> error "syntactic non-terminalting loop" 
    ; [ l_j, l_k ] | pathExists cfg l_j l_i -> 
{-
-- in target langauge
CL_cps l_i kenv cfg 
   | \exists l_j, l_k: l_j \= l_k /\ (l_i,l_j) \in cfg /\ (l_i, l_k) \in cfg 
     /\ \exists l' : (l_j, l', l_i) \in cfg 
     /\ \not (\exists  l'' : (l_k, l'', l_i) \in cfg)
   = (\overline{D_j} ++ \overline{D_k}, loop( () => { return E}, E_j,E_k ))  
   where (\overline{\phi}, if e { goto l_j; } else { goto l_k; }) = kenv(l_i)
         (\overline{D_j}, E_j) = CL_cps l_j kenv cfg
         (\overline{D_k}, E_k) = CL_cps l_k kenv cfg
         E = CE_cps e
-- in c 
CL_cps l_i kenv cfg 
   | \exists l_j, l_k: l_j \= l_k /\ (l_i,l_j) \in cfg /\ (l_i, l_k) \in cfg 
     /\ \exists l' : (l_j, l', l_i) \in cfg 
     /\ \not (\exists  l'' : (l_k, l'', l_i) \in cfg)
   = (\overline{D_j} ++ \overline{D_k}, loop( () => { return E}, E_j,E_k ))  
   where (\overline{\phi}, if e { goto l_j; } else { goto l_k; }) = kenv(l_i)
         (\overline{D_j}, E_j) = CL_cps l_j kenv cfg
         (\overline{D_k}, E_k) = CL_cps l_k kenv cfg
         E = CE_cps e
         LoopCond_i = int loopCond_i (ctxt* c) { return E;}
         LoopVisit_i = void loopExit_i (ctxt* c) { (CF_cps l_i l_j); return E_j; }
         LoopExit_i = void loopExit_i (ctxt* c) { (CF_cps l_i l_k); return E_k; }
         Loop_i = void loop_i (ctxt* c) { loop_push(&loopCind_i, &loopVisit_i, &Loop_Exit_i, c); lambda_loop(c); }
-}           
           case M.lookup l_i kenv of 
             { Just n -> case lb_stmts n of  
                  { [ AST.CBlockStmt 
                      (AST.CIf exp trueStmt@(AST.CGoto l_j _) (Just falseStmt@(AST.CGoto l_k _)) nodeInfo) ] -> 
                       let (d_j,e_j) = cps_trans_lb isReturnVoid localVars fargs ctxtName fname l_j kenv cfg
                           (d_k,e_k) = cps_trans_lb isReturnVoid localVars fargs ctxtName fname l_k kenv cfg
                           exp'      = cps_trans_exp localVars fargs ctxtName exp
                           tyVoid    = [AST.CTypeSpec (AST.CVoidType N.undefNode)]
                           tyInt     = [AST.CTypeSpec (AST.CIntType N.undefNode)]
                           declrs = []
                           paramCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)]  -- ctxt* ctxt
                                       [(Just (AST.CDeclr (Just (iid ctxtParamName)) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
                           paramDeclr = [paramCtxt]      
                           decltrs    = [AST.CFunDeclr (Right (paramDeclr,False)) [] N.undefNode] 
                           mb_strLitr = Nothing
                           attrs      =  []
                           
                           loopCond   = fname `app` iid loopCondName
                           loopCond_i  = loopCond `app` l_i 
                           loopCondStmt = AST.CCompound [] [AST.CBlockStmt (AST.CReturn (Just exp') N.undefNode)] N.undefNode
                           loopCond_iD = AST.CFunDef tyInt (AST.CDeclr (Just loopCond_i) decltrs mb_strLitr attrs N.undefNode) declrs loopCondStmt N.undefNode
                           
                           loopVisit   = fname `app` iid loopVisitName
                           loopVisit_i = loopVisit `app` l_i
                           
                           loopVisitPhis = case M.lookup l_j kenv of 
                             { Nothing -> [] 
                             ; Just n  -> cps_trans_phis ctxtName l_i l_j (lb_phis n)
                             }
                           loopVisitStmt = AST.CCompound [] (loopVisitPhis ++ [AST.CBlockStmt (AST.CReturn (Just e_j) N.undefNode)]) N.undefNode 
                           loopVisit_iD = AST.CFunDef tyVoid (AST.CDeclr (Just loopVisit_i) decltrs mb_strLitr attrs N.undefNode) declrs loopVisitStmt N.undefNode
                           
                           
                           loopExit    = fname `app` iid loopExitName
                           loopExit_i  = loopExit `app` l_i
                           loopExitPhis = case M.lookup l_k kenv of 
                             { Nothing -> [] 
                             ; Just n  -> cps_trans_phis ctxtName l_i l_k (lb_phis n)
                             } 
                           loopExitStmt = AST.CCompound [] (loopExitPhis ++ [AST.CBlockStmt (AST.CReturn (Just e_k) N.undefNode)]) N.undefNode 
                           loopExit_iD = AST.CFunDef tyVoid (AST.CDeclr (Just loopExit_i) decltrs mb_strLitr attrs N.undefNode) declrs loopExitStmt N.undefNode

                           
                           loop        = fname `app` iid loopName
                           loop_i      = loop `app` l_i
                           loopPush    = fname `app` iid loopPushName
                           lambdaloop  = fname `app` iid lambdaLoopName
                           loopStmt    = AST.CCompound [] 
                                        [ AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` loopPush)) [ (adr (cvar loopCond_i)), (adr (cvar loopVisit_i)), (adr (cvar loopExit_i)), (cvar $ iid ctxtParamName) ] N.undefNode)) N.undefNode)
                                        , (AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` lambdaloop)) [ (cvar $ iid ctxtParamName) ] N.undefNode)) N.undefNode))
                                        ] 
                                        N.undefNode
                           loop_iD = AST.CFunDef tyVoid (AST.CDeclr (Just loop_i) decltrs mb_strLitr attrs N.undefNode) declrs loopStmt N.undefNode
                       in ([loopCond_iD, loopVisit_iD, loopExit_iD, loop_iD] ++ d_j ++ d_k, cvar loop_i)

                  ; _ -> error "not possible"
                  }
             ; _ -> error "not possible"
             } 
                                               
    ; [ l_j, l_k ] | pathExists cfg l_k l_i ->  -- same as the above case, just swapping k and j 
           case M.lookup l_i kenv of 
             { Just n -> case lb_stmts n of  
                  { [ AST.CBlockStmt 
                      (AST.CIf exp trueStmt@(AST.CGoto l_j _) (Just falseStmt@(AST.CGoto l_k _)) nodeInfo) ] -> 
                       let (d_j,e_j) = cps_trans_lb isReturnVoid localVars fargs ctxtName fname l_j kenv cfg
                           (d_k,e_k) = cps_trans_lb isReturnVoid localVars fargs ctxtName fname l_k kenv cfg
                           exp'      = cps_trans_exp localVars fargs ctxtName exp
                           tyVoid    = [AST.CTypeSpec (AST.CVoidType N.undefNode)]
                           tyInt     = [AST.CTypeSpec (AST.CIntType N.undefNode)]
                           declrs = []
                           paramCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)]  -- ctxt* ctxt
                                       [(Just (AST.CDeclr (Just (iid ctxtParamName)) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
                           paramDeclr = [paramCtxt]      
                           decltrs    = [AST.CFunDeclr (Right (paramDeclr,False)) [] N.undefNode] 
                           mb_strLitr = Nothing
                           attrs      =  []
                           
                           loopCond   = fname `app` iid loopCondName
                           loopCond_i  = loopCond `app` l_i 
                           loopCondStmt = AST.CCompound [] [AST.CBlockStmt (AST.CReturn (Just exp') N.undefNode)] N.undefNode
                           loopCond_iD = AST.CFunDef tyInt (AST.CDeclr (Just loopCond_i) decltrs mb_strLitr attrs N.undefNode) declrs loopCondStmt N.undefNode
                           
                           loopVisit   = fname `app` iid loopVisitName
                           loopVisit_i = loopVisit `app` l_i
                           
                           loopVisitPhis = case M.lookup l_k kenv of 
                             { Nothing -> [] 
                             ; Just n  -> cps_trans_phis ctxtName l_i l_k (lb_phis n)
                             }
                           loopVisitStmt = AST.CCompound [] (loopVisitPhis ++ [AST.CBlockStmt (AST.CReturn (Just e_k) N.undefNode)]) N.undefNode 
                           loopVisit_iD = AST.CFunDef tyVoid (AST.CDeclr (Just loopVisit_i) decltrs mb_strLitr attrs N.undefNode) declrs loopVisitStmt N.undefNode
                           
                           
                           loopExit    = fname `app` iid loopExitName
                           loopExit_i  = loopExit `app` l_i
                           loopExitPhis = case M.lookup l_j kenv of 
                             { Nothing -> [] 
                             ; Just n  -> cps_trans_phis ctxtName l_i l_j (lb_phis n)
                             } 
                           loopExitStmt = AST.CCompound [] (loopExitPhis ++ [AST.CBlockStmt (AST.CReturn (Just e_j) N.undefNode)]) N.undefNode 
                           loopExit_iD = AST.CFunDef tyVoid (AST.CDeclr (Just loopExit_i) decltrs mb_strLitr attrs N.undefNode) declrs loopExitStmt N.undefNode

                           
                           loop        = fname `app` iid loopName
                           loop_i      = loop `app` l_i
                           loopPush    = fname `app` iid loopPushName
                           lambdaloop  = fname `app` iid lambdaLoopName
                           loopStmt    = AST.CCompound [] 
                                        [ AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` loopPush)) [ (adr (cvar loopCond_i)), (adr (cvar loopVisit_i)), (adr (cvar loopExit_i)), (cvar $ iid ctxtParamName) ] N.undefNode)) N.undefNode)
                                        , (AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` lambdaloop)) [ (cvar $ iid ctxtParamName) ] N.undefNode)) N.undefNode))
                                        ] 
                                        N.undefNode
                           loop_iD = AST.CFunDef tyVoid (AST.CDeclr (Just loop_i) decltrs mb_strLitr attrs N.undefNode) declrs loopStmt N.undefNode
                       in ([loopCond_iD, loopVisit_iD, loopExit_iD, loop_iD] ++ d_j ++ d_k, cvar loop_i)

                  ; _ -> error "not possible"
                  }
             ; _ -> error "not possible"
             }            
           
    ; [ l_j, l_k ] | otherwise -> 
{-
-- in target langauge
CL_cps l_i kenv cfg 
   | \exists l_j, l_k: l_j \= l_k /\ (l_i,l_j) \in cfg /\ (l_i, l_k) \in cfg 
     /\ \not (\exists  l' : (l_j, l', l_i) \in cfg \/ (l_k, l', l_i) \in cfg)
   = (\overline{D_j} ++ \overline{D_k}, ifc( () => { return E}, E_j,E_k ))  
   where (\overline{\phi}, if e { goto l_j; } else { goto l_k; }) = kenv(l_i)
         (\overline{D_j}, E_j) = CL_cps l_j kenv cfg
         (\overline{D_k}, E_k) = CL_cps l_k kenv cfg
         E = CE_cps e
-- in c 
CL_cps l_i kenv cfg 
   | \exists l_j, l_k: l_j \= l_k /\ (l_i,l_j) \in cfg /\ (l_i, l_k) \in cfg 
     /\ \not (\exists  l' : (l_j, l', l_i) \in cfg \/ (l_k, l', l_i) \in cfg)
   = (Ifc_i::IfTr_i::IfFl_i::\overline{D_j} ++ \overline{D_k}, Ifc_i) 
   where (\overline{\phi}, if e { goto l_j; } else { goto l_k; }) = kenv(l_i)
         (\overline{D_j}, E_j) = CL_cps l_j kenv cfg
         (\overline{D_k}, E_k) = CL_cps l_k kenv cfg
         E = CE_cps e
         IfCond_i = int ifcond_i (ctxt* c) { return E; }
         IfTr_i = void iftr_i (ctxt* c) { (CF_cps l_i l_j); return E_j; }
         IfFl_i = void iffl_i (ctxt* c) { (CF_cps l_i l_k); return E_k; }
         Ifc_i = void ifc_i (ctxt* c) { ifc_push(&ifcond_i, &iftr_i, &iftf_i,c); lambda_ifc(c); }

-}
           case M.lookup l_i kenv of 
             { Just n -> case lb_stmts n of  
                  { [ AST.CBlockStmt 
                      (AST.CIf exp trueStmt@(AST.CGoto l_j _) (Just falseStmt@(AST.CGoto l_k _)) nodeInfo) ] -> 
                       let (d_j,e_j) = cps_trans_lb isReturnVoid localVars fargs ctxtName fname l_j kenv cfg
                           (d_k,e_k) = cps_trans_lb isReturnVoid localVars fargs ctxtName fname l_k kenv cfg
                           exp'      = cps_trans_exp localVars fargs ctxtName exp
                           tyVoid    = [AST.CTypeSpec (AST.CVoidType N.undefNode)]
                           tyInt     = [AST.CTypeSpec (AST.CIntType N.undefNode)]
                           declrs = []
                           paramCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)]  -- ctxt* ctxt
                                       [(Just (AST.CDeclr (Just (iid ctxtParamName)) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
                           paramDeclr = [paramCtxt]      
                           decltrs    = [AST.CFunDeclr (Right (paramDeclr,False)) [] N.undefNode] 
                           mb_strLitr = Nothing
                           attrs      =  []
                           
                           ifcCond    = fname `app` iid ifcCondName
                           ifcCond_i  = ifcCond `app` l_i 
                           ifcCondStmt = AST.CCompound [] [AST.CBlockStmt (AST.CReturn (Just exp') N.undefNode)] N.undefNode
                           ifcCond_iD = AST.CFunDef tyInt (AST.CDeclr (Just ifcCond_i) decltrs mb_strLitr attrs N.undefNode) declrs ifcCondStmt N.undefNode
                           
                           ifcTrue    = fname `app` iid ifcTrName
                           ifcTrue_i  = ifcTrue `app` l_i
                           
                           ifcTruePhis = case M.lookup l_j kenv of 
                             { Nothing -> [] 
                             ; Just n  -> cps_trans_phis ctxtName l_i l_j (lb_phis n)
                             }
                           ifcTrueStmt = AST.CCompound [] (ifcTruePhis ++ [AST.CBlockStmt (AST.CReturn (Just e_j) N.undefNode)]) N.undefNode 
                           ifcTrue_iD = AST.CFunDef tyVoid (AST.CDeclr (Just ifcTrue_i) decltrs mb_strLitr attrs N.undefNode) declrs ifcTrueStmt N.undefNode
                           
                           
                           ifcFalse    = fname `app` iid ifcFlName
                           ifcFalse_i  = ifcFalse `app` l_i
                           ifcFalsePhis = case M.lookup l_k kenv of 
                             { Nothing -> [] 
                             ; Just n  -> cps_trans_phis ctxtName l_i l_k (lb_phis n)
                             } 
                           ifcFalseStmt = AST.CCompound [] (ifcFalsePhis ++ [AST.CBlockStmt (AST.CReturn (Just e_k) N.undefNode)]) N.undefNode 
                           ifcFalse_iD = AST.CFunDef tyVoid (AST.CDeclr (Just ifcFalse_i) decltrs mb_strLitr attrs N.undefNode) declrs ifcFalseStmt N.undefNode

                           
                           ifc        = fname `app` iid ifcName
                           ifc_i      = ifc `app` l_i
                           ifcPush    = fname `app` iid ifcPushName
                           lambdaifc  = fname `app` iid lambdaIfcName
                           ifcStmt    = AST.CCompound [] 
                                        [ AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` ifcPush)) [ (adr (cvar ifcCond_i)), (adr (cvar ifcTrue_i)), (adr (cvar ifcFalse_i)), (cvar $ iid ctxtParamName) ] N.undefNode)) N.undefNode)
                                        , (AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` lambdaifc)) [ (cvar $ iid ctxtParamName) ] N.undefNode)) N.undefNode))
                                        ] 
                                        N.undefNode
                           ifc_iD = AST.CFunDef tyVoid (AST.CDeclr (Just ifc_i) decltrs mb_strLitr attrs N.undefNode) declrs ifcStmt N.undefNode
                       in ([ifcCond_iD, ifcTrue_iD, ifcFalse_iD, ifc_iD] ++ d_j ++ d_k, cvar ifc_i)

                  ; _ -> error "not possible"
                  }
             ; _ -> error "not possible"
             } 
    ; [] -> 
{-
-- in target langauge
CL_cps l_i kenv cfg 
   | \not (\exists l_j : (l_i,l_j) \in cfg) 
   = ([D], bind( f_i, () => { return ret()})  
   where (\overline{\phi}, s) = kenv(l_i)
         k is fresh
         S = CS_cps s kenv l_i k 
         D = (void => void) => void f_i = (void => void k) = { S }
-- in c 
CL_cps l_i kenv cfg 
   | \not (\exists l_j : (l_i,l_j) \in cfg) 
   = ([D], bind( f_i, () => { return ret()})  
   where (\overline{\phi}, s) = kenv(l_i)
         k is fresh
         S = CS_cps s kenv l_i k 
         D = void f_i = (ctxt* c) = { S }
         Bind_i = void bind_i (ctxt* c) { bind_push(&f_i, &ret, c); lambda_bind(c); }
-}           
           let f_iname = fname `app` l_i
               tyVoid = [AST.CTypeSpec (AST.CVoidType N.undefNode)]
               declrs = []
               paramCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)]  -- ctxt* ctxt
                           [(Just (AST.CDeclr (Just (iid ctxtParamName)) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
               paramDeclr = [paramCtxt]      
               decltrs = [AST.CFunDeclr (Right (paramDeclr,False)) [] N.undefNode] 
               mb_strLitr = Nothing
               attrs  =  []
               
               k      = iid kParamName

               lb     = fromJust (M.lookup l_i kenv)
               stmt' =  AST.CCompound [] ((cps_trans_stmts isReturnVoid localVars fargs ctxtName fname k kenv l_i (lb_stmts lb)) ) N.undefNode 
               f_iD   = AST.CFunDef tyVoid (AST.CDeclr (Just f_iname) decltrs mb_strLitr attrs N.undefNode) declrs stmt' N.undefNode
               
               bind  = fname `app` iid bindName -- todo need to add fname prefix?
               bindpush = fname `app`  iid bindPushName
               lambdabind = fname `app` iid lambdaBindName
               
               ret = fname `app` iid retName
               
               bind_i = bind `app` l_i
               bindPush = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` bindpush)) [ (adr $ cvar f_iname), (adr $ cvar ret), (cvar $ iid ctxtParamName)] N.undefNode)) N.undefNode) -- bind_push(&f_i, &e, ctxt)
               lambdaBindApp = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` lambdabind)) [(cvar $ iid ctxtParamName)] N.undefNode)) N.undefNode) -- lambdaBind(ctxt)
               bindStmt = AST.CCompound [] [ bindPush, lambdaBindApp] N.undefNode
               bind_iD = AST.CFunDef tyVoid (AST.CDeclr (Just bind_i) decltrs mb_strLitr attrs N.undefNode) declrs bindStmt N.undefNode
             
        in (f_iD:bind_iD:[], cvar bind_i) 
             
    }
     
     
cps_trans_stmts :: Bool -> -- ^ is return type void
                   S.Set Ident -> -- ^ local vars
                   S.Set Ident -> -- ^ formal args
                   ContextName -> 
                   Ident -> -- ^ fname 
                   Ident -> -- ^ K
                   -- ^ \bar{\Delta} become part of the labelled block flag (loop) 
                   M.Map Ident LabeledBlock ->  -- ^ \bar{b} kenv
                   Ident ->  -- ^ label for the current block
                   [AST.CCompoundBlockItem N.NodeInfo] ->  -- ^ stmts
                   [AST.CCompoundBlockItem N.NodeInfo]
-- cps_trans_stmts = undefined                   
cps_trans_stmts isReturnVoid localVars fargs ctxtName fname k kenv l_i stmts = -- todo: maybe pattern match the CCompound constructor here?
  concatMap (\stmt -> cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k kenv l_i stmt) stmts 



-- fn, K, \bar{\Delta}, \bar{b} |-_l s => S
cps_trans_stmt :: Bool -> -- ^ is return type void
                  S.Set Ident -> -- ^ local vars
                  S.Set Ident -> -- ^ formal args
                  ContextName -> 
                  Ident ->  -- ^ fname
                  Ident ->  -- ^ K
                   -- ^ \bar{\Delta} become part of the labelled block flag (loop)                   
                  M.Map Ident LabeledBlock -> -- ^ \bar{b} kenv
                  Ident ->  -- ^ label for the current block
                  AST.CCompoundBlockItem N.NodeInfo -> 
                  [AST.CCompoundBlockItem N.NodeInfo]
{-
-- in target language
CS_cps (goto l_i) kenv l_j k = \overline{X = E}; return k();
       where (\overline{\phi}, s) = kenv l_i
             \overline{(X,E)} = CF_cps \overline{\phi} l_j
-- in C
CS_cps (goto l_i) kenv l_j k = \overline{X = E}; ctxt_arr_void k = kpeek(ctxt); kpop(ctxt); return (*k)(ctxt);
       where (\overline{\phi}, s) = kenv l_i
             \overline{(X,E)} = CF_cps \overline{\phi} l_j

-}
-- note that our target is C, hence besides k, the function call include argument such as context
cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k kenv l_j (AST.CBlockStmt (AST.CGoto l_i nodeInfo)) = case M.lookup l_i kenv of 
  { Just lb -> 
       let 
           asgmts  = cps_trans_phis ctxtName l_j l_i (lb_phis lb)
           peek_ty = [AST.CTypeSpec (AST.CTypeDef (fname `app` (iid ctxt_arr_void)) N.undefNode)]
           peek_var = Just (AST.CDeclr (Just (fname `app` k)) [] Nothing [] N.undefNode)
           peek_rhs = Just (AST.CInitExpr (AST.CCall (cvar (fname `app` (iid kPeekName))) [cvar $ iid ctxtParamName] N.undefNode) N.undefNode) 
           peek   = AST.CBlockDecl (AST.CDecl peek_ty [(peek_var, peek_rhs, Nothing)] N.undefNode) 
           pop    = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` (iid kPopName))) [cvar $ iid ctxtParamName] N.undefNode)) N.undefNode)
           kApp   = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (ind $ cvar (fname `app` k)) [cvar $ iid ctxtParamName] N.undefNode)) N.undefNode) -- (*k)(ctxt)
       in asgmts ++ [ peek, pop, kApp ]
  ; Nothing -> error "cps_trans_stmt failed at a non existent label."
  }
                                                                                        
cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k kenv l_j (AST.CBlockStmt (AST.CExpr (Just e) nodeInfo)) =  -- exp
  let e' = cps_trans_exp localVars fargs ctxtName e
  in [AST.CBlockStmt (AST.CExpr (Just e') nodeInfo)]
{-
-- target language
CS_cps (return e) kenv l_j k = res = E; return k();
         where E = CE_cps e

-- c language
CS_cps (return e) kenv l_j k = res = E; ctxt_arr_void k = kpeek(ctxt); kpop(ctxt); return (*k)(ctxt);
-}

cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k kenv l_i  (AST.CBlockStmt (AST.CReturn Nothing nodeInfo)) = 
  let
    peek_ty = [AST.CTypeSpec (AST.CTypeDef (fname `app` (iid ctxt_arr_void)) N.undefNode)]
    peek_var = Just (AST.CDeclr (Just (fname `app` k)) [] Nothing [] N.undefNode)
    peek_rhs = Just (AST.CInitExpr (AST.CCall (cvar (fname `app` (iid kPeekName))) [cvar $ iid ctxtParamName] N.undefNode) N.undefNode) 
    peek   = AST.CBlockDecl (AST.CDecl peek_ty [(peek_var, peek_rhs, Nothing)] N.undefNode) 
    pop    = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` (iid kPopName))) [cvar $ iid ctxtParamName] N.undefNode)) N.undefNode)
    kApp   = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (ind $ cvar (fname `app` k)) [cvar $ iid ctxtParamName] N.undefNode)) N.undefNode) -- (*k)(ctxt)
  in [ peek, pop, kApp ]

cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k kenv l_i  (AST.CBlockStmt (AST.CReturn (Just e) nodeInfo)) = 
  let
    peek_ty = [AST.CTypeSpec (AST.CTypeDef (fname `app` (iid ctxt_arr_void)) N.undefNode)]
    peek_var = Just (AST.CDeclr (Just (fname `app` k)) [] Nothing [] N.undefNode)
    peek_rhs = Just (AST.CInitExpr (AST.CCall (cvar (fname `app` (iid kPeekName))) [cvar $ iid ctxtParamName] N.undefNode) N.undefNode) 
    peek   = AST.CBlockDecl (AST.CDecl peek_ty [(peek_var, peek_rhs, Nothing)] N.undefNode) 
    pop    = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (cvar (fname `app` (iid kPopName))) [cvar $ iid ctxtParamName] N.undefNode)) N.undefNode)
    kApp   = AST.CBlockStmt (AST.CExpr (Just (AST.CCall (ind $ cvar (fname `app` k)) [cvar $ iid ctxtParamName] N.undefNode)) N.undefNode) -- (*k)(ctxt)
    e' = cps_trans_exp localVars fargs ctxtName e
    assign_or_e | isReturnVoid = ((cvar (iid ctxtParamName)) .->. (iid "func_result")) .=. e' 
                 | otherwise = e' 
    stmt = AST.CBlockStmt (AST.CExpr (Just assign_or_e) N.undefNode)
  in [ stmt, peek, pop, kApp ]
  

cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k kenv l_i (AST.CBlockStmt (AST.CCompound ids stmts nodeInfo)) = 
  -- todo: do we need to do anything with the ids (local labels)?
  let stmts' = cps_trans_stmts isReturnVoid localVars fargs ctxtName fname k kenv l_i stmts
  in [AST.CBlockStmt (AST.CCompound ids stmts' nodeInfo)]
  

cps_trans_stmt isReturnVoid localVars fargs ctxtName fname k kenv l_i stmt = 
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
ssa2cps fundef ssa@(SSA scopedDecls labelledBlocks sdom local_decl_vars fargs) = 
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
      context         = mkContext ctxtStructName labelledBlocks formalArgDecls scopedDecls returnTy ptrArrs local_decl_vars fargs
      ctxtName        = map toLower ctxtStructName -- alias name is inlower case and will be used in the the rest of the code
            
      -- loop_cps, loop_lambda, id and pop and push
      loop_cps        = loopCPS ctxtName funName
      lambda_loop_cps = lambdaLoopCPS ctxtName funName
      loop_push_cps   = loopPushCPS ctxtName funName
      loop_pop_cps    = loopPopCPS ctxtName funName
      loop_peek_c_cps = loopPeekCCPS ctxtName funName
      loop_peek_v_cps = loopPeekVCPS ctxtName funName
      loop_peek_e_cps = loopPeekECPS ctxtName funName
      
      
      bind_cps = bindCPS ctxtName funName
      lambda_bind_cps = lambdaBindCPS ctxtName funName
      bind_push_cps   = bindPushCPS ctxtName funName 
      bind_pop_cps    = bindPopCPS ctxtName funName
      bind_peek_m_cps = bindPeekMCPS ctxtName funName
      bind_peek_f_cps = bindPeekFCPS ctxtName funName
      
      ifc_cps = ifcCPS ctxtName funName
      lambda_ifc_cps  = lambdaIfcCPS ctxtName funName
      ifc_push_cps    = ifcPushCPS ctxtName funName
      ifc_pop_cps     = ifcPopCPS ctxtName funName
      ifc_peek_c_cps  = ifcPeekCCPS ctxtName funName
      ifc_peek_tr_cps = ifcPeekTrCPS ctxtName funName
      ifc_peek_fl_cps = ifcPeekFlCPS ctxtName funName 
      
      id_cps          = idCPS ctxtName funName
      ret_cps         = retCPS ctxtName funName
      
      k_push_cps      = kPushCPS ctxtName funName
      k_peek_cps      = kPeekCPS ctxtName funName
      k_pop_cps       = kPopCPS ctxtName funName
      
      combinators     = [loop_cps, lambda_loop_cps, loop_push_cps, loop_pop_cps, loop_peek_c_cps, loop_peek_v_cps, loop_peek_e_cps, 
                         bind_cps, lambda_bind_cps, bind_push_cps, bind_pop_cps, bind_peek_m_cps, bind_peek_f_cps, 
                         ifc_cps, lambda_ifc_cps, ifc_push_cps,ifc_pop_cps, ifc_peek_c_cps, ifc_peek_tr_cps, ifc_peek_fl_cps, 
                         id_cps, ret_cps, k_push_cps, k_peek_cps,k_pop_cps]
      
      -- all the "nested/helper" function declarations 
      -- todo: packing all the variable into a record 
      l_0 = iid (labPref ++ "0" )
      (ps,entryExp)   = cps_trans_lb isReturnVoid local_decl_vars fargs ctxtName (iid funName) l_0 labelledBlocks ssa
      
      -- all function signatures
      funcSignatures  = map funSig (ps ++ combinators ++ [fundef]) -- include the source func, in case of recursion
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
        -- 5. calling entry expression
        [ AST.CBlockStmt (AST.CExpr (Just entryExp) N.undefNode)
        , if isReturnVoid 
          then AST.CBlockStmt (AST.CReturn Nothing N.undefNode) 
          else AST.CBlockStmt (AST.CReturn (Just $ (cvar (iid ctxtParamName)) .->. (iid "func_result")) N.undefNode)
        ]
        
      main_func = case fundef of 
        { AST.CFunDef tySpecfs declarator decls _ nodeInfo -> 
             AST.CFunDef tySpecfs declarator decls (AST.CCompound [] (main_decls ++ main_stmts) N.undefNode) nodeInfo 
        }
  in CPS main_decls main_stmts funcSignatures (ps ++ combinators)  context main_func 
     
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

-- ^ push

{-
void loop_push(ctxt_arr_bool cond,
	  ctxt_arr_void visitor,
	  ctxt_arr_void exit,
	  ctxt *ctxt) {
  ctxt->loop_c[ctxt->curr_stack_size] = cond;
  ctxt->loop_v[ctxt->curr_stack_size] = visitor;
  ctxt->loop_e[ctxt->curr_stack_size] = exit;
  ctxt->loop_stack_top = ctxt->curr_stack_size + 1;  
}
-}
                 
genPushCPS :: ContextName -> 
              String -> -- function name prefix
              String -> -- operator name (loop or ifc)
              String -> -- stackTop name
              String -> -- fst para name (cond)
              String -> -- snd para name (visitor or tr)
              String -> -- third para name (exit or fl)
              String -> -- stack fst name
              String -> -- stack snd name
              String -> -- stack third name
              AST.CFunctionDef N.NodeInfo 
genPushCPS ctxtName fname operatorName stackTop condParaName sndParamName thirdParamName stackCName stackSndName stackThirdName = 
  let cond          = iid condParaName
      formalArgCond = AST.CDecl [AST.CTypeSpec intTy] -- int (*cond)(ctxt *)
                      [(Just (AST.CDeclr (Just cond) 
                              [ AST.CPtrDeclr [] N.undefNode
                              , AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode) ] 
                                                       [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] 
                                                       N.undefNode], False)
                                              ) [] N.undefNode] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode 
      formalArgX x = AST.CDecl [AST.CTypeSpec voidTy] -- void (*x)(ctxt *)
                     [(Just (AST.CDeclr (Just x) 
                             [ AST.CPtrDeclr [] N.undefNode
                             , AST.CFunDeclr (Right ([AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode) ] 
                                                       [(Just (AST.CDeclr Nothing [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] 
                                                       N.undefNode], False)
                                              ) [] N.undefNode] Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode 
      second       = iid sndParamName                     
      formalArgSecond = formalArgX second
      third          = iid thirdParamName
      formalArgThird = formalArgX third
      ctxt          = iid ctxtParamName                 
      formalArgCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)]  -- ctxt* ctxt
                      [(Just (AST.CDeclr (Just ctxt) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
  in fun [AST.CTypeSpec voidTy] (iid fname `app` iid operatorName) [formalArgCond, formalArgSecond, formalArgThird, formalArgCtxt] 
     [ AST.CBlockStmt (AST.CExpr (Just (((cvar ctxt .->. (iid stackCName)) .!!. (cvar ctxt .->. (iid stackTop))) .=. (cvar cond))) N.undefNode)
     , AST.CBlockStmt (AST.CExpr (Just (((cvar ctxt .->. (iid stackSndName)) .!!. (cvar ctxt .->. (iid stackTop))) .=. (cvar second))) N.undefNode)
     , AST.CBlockStmt (AST.CExpr (Just (((cvar ctxt .->. (iid stackThirdName)) .!!. (cvar ctxt .->. (iid stackTop))) .=. (cvar third))) N.undefNode)
     , AST.CBlockStmt (AST.CExpr (Just ((cvar ctxt .->. (iid stackTop)) .=. (AST.CBinary AST.CAddOp (cvar ctxt .->. (iid stackTop)) (AST.CConst (AST.CIntConst (cInteger 1) N.undefNode)) N.undefNode))) N.undefNode)
     ]
                 
loopPushCPS :: ContextName -> 
              String -> -- function name prefix
              AST.CFunctionDef N.NodeInfo
loopPushCPS ctxtName fname = genPushCPS ctxtName fname loopPushName loopStackTop condParamName visitorParamName exitParamName loopStackCName loopStackVName loopStackEName


ifcPushCPS :: ContextName -> 
              String -> -- function name prefix
              AST.CFunctionDef N.NodeInfo
ifcPushCPS ctxtName fname = genPushCPS ctxtName fname ifcPushName ifcStackTop condParamName trueParamName falseParamName ifcStackCName ifcStackTrName ifcStackFlName


{-     
void pop(sortctxt *ctxt) {
  ctxt->curr_stack_size = ctxt->curr_stack_size - 1;
}
-}

genPopCPS :: ContextName -> 
             String -> -- ^ function name prefix
             String -> -- ^ operator name
             String -> -- ^ stackTop
             AST.CFunctionDef N.NodeInfo 
genPopCPS ctxtName fname operatorName stackTop = 
  let ctxt          = iid ctxtParamName                 
      formalArgCtxt = AST.CDecl [AST.CTypeSpec (AST.CTypeDef (iid ctxtName) N.undefNode)]  -- ctxt* ctxt
                      [(Just (AST.CDeclr (Just ctxt) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode
  in fun [AST.CTypeSpec voidTy] (iid fname `app` iid operatorName) [formalArgCtxt]
     [ AST.CBlockStmt (AST.CExpr (Just ((cvar ctxt .->. (iid stackTop)) .=. (AST.CBinary AST.CSubOp (cvar ctxt .->. (iid stackTop)) (AST.CConst (AST.CIntConst (cInteger 1) N.undefNode)) N.undefNode))) N.undefNode)
     ]

     
kPopCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo 
kPopCPS ctxtName fname = genPopCPS ctxtName fname kPopName kStackTop

loopPopCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo 
loopPopCPS ctxtName fname = genPopCPS ctxtName fname loopPopName loopStackTop

ifcPopCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo 
ifcPopCPS ctxtName fname = genPopCPS ctxtName fname ifcPopName ifcStackTop


bindPopCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo 
bindPopCPS ctxtName fname = genPopCPS ctxtName fname bindPopName bindStackTop


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
     

retCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo 
retCPS = undefined


loopPeekCCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo 
loopPeekCCPS = undefined


loopPeekVCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo 
loopPeekVCPS = undefined


loopPeekECPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo 
loopPeekECPS = undefined


-- loopPopCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo 
-- loopPopCPS = undefined



lambdaBindCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo
lambdaBindCPS = undefined


bindPushCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo
bindPushCPS = undefined

-- bindPopCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo
-- bindPopCPS = undefined


bindCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo
bindCPS = undefined


bindPeekMCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo
bindPeekMCPS = undefined

bindPeekFCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo
bindPeekFCPS = undefined


ifcCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo
ifcCPS = undefined

lambdaIfcCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo
lambdaIfcCPS = undefined


ifcPeekCCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo
ifcPeekCCPS = undefined

ifcPeekTrCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo
ifcPeekTrCPS = undefined


ifcPeekFlCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo
ifcPeekFlCPS = undefined

-- ifcPopCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo
-- ifcPopCPS = undefined

kPushCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo
kPushCPS = undefined


kPeekCPS :: ContextName -> String -> AST.CFunctionDef N.NodeInfo
kPeekCPS = undefined


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
  

