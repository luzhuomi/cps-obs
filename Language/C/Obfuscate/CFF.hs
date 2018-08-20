{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Language.C.Obfuscate.CFF
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

import Language.C.Obfuscate.CPS (cps_trans_phis, 
                                 cps_trans_exp, 
                                 ctxtParamName, 
                                 renameDeclWithLabeledBlocks, 
                                 declLHSEq, 
                                 cps_trans_declaration,
                                 dropConstTyQual,
                                 dropStorageQual
                                 )

-- import for testing
import Language.C (parseCFile, parseCFilePre)
import Language.C.System.GCC (newGCC)
import Language.C.Pretty (pretty)
import Text.PrettyPrint.HughesPJ (render, text, (<+>), hsep)

import System.IO.Unsafe (unsafePerformIO)

type ContextName = String 

swParamName = "swParam"

-- CL_cff
-- unlike the paper, we use switch instead of nested if
cff_trans_blks :: Bool -> -- ^ is return void
                  S.Set Ident -> -- ^ local vars
                  S.Set Ident -> -- ^ formal args
                  ContextName -> 
                  Ident ->  -- ^ top level function name
                  M.Map Ident LabeledBlock ->  -- ^ KEnv
                  [(Ident, LabeledBlock)] -> -- ^ [(label, { phi ; s })]
                  [AST.CCompoundBlockItem N.NodeInfo]
cff_trans_blks isVoid localVars formalArgs ctxtName fname kenv [] = []
cff_trans_blks isVoid localVars formalArgs ctxtName fname kenv ((l, blk):rest) = 
  let stmts   = lb_stmts blk
      sw       = iid swParamName
      stmts'  = cff_trans_stmts isVoid localVars formalArgs ctxtName fname sw kenv l stmts -- must be all Block Statement
      rest'   = cff_trans_blks isVoid localVars formalArgs ctxtName fname kenv rest
      e       =  AST.CConst (AST.CIntConst (cInteger (unLabPref l)) N.undefNode)
      --- stmts'' = (stmts'++ [AST.CBlockStmt (AST.CBreak N.undefNode)])
  in (cCase e stmts')++rest'


cff_trans_stmts :: Bool -> -- ^ is return type void
                   S.Set Ident -> -- ^ local vars
                   S.Set Ident -> -- ^ formal args
                   ContextName -> 
                   Ident -> -- ^ fname 
                   Ident -> -- ^ sw
                   M.Map Ident LabeledBlock ->  -- ^ \bar{b} kenv
                   Ident -> -- ^ label for the current block
                   [AST.CCompoundBlockItem N.NodeInfo] ->  -- ^ stmts
                   [AST.CCompoundBlockItem N.NodeInfo]
cff_trans_stmts isReturnVoid localVars fargs ctxtName fname sw kenv l stmts = 
  concatMap (\stmt -> cff_trans_stmt isReturnVoid localVars fargs ctxtName fname sw kenv l stmt) stmts 


cff_trans_stmt :: Bool -> -- ^ is return type void
                  S.Set Ident -> -- ^ local vars
                  S.Set Ident -> -- ^ formal args
                  ContextName -> 
                  Ident -> -- ^ fname 
                  Ident -> -- ^ sw
                  M.Map Ident LabeledBlock ->  -- ^ \bar{b} kenv
                  Ident -> -- ^ label for the current block
                  AST.CCompoundBlockItem N.NodeInfo ->  -- ^ stmt
                  [AST.CCompoundBlockItem N.NodeInfo]
{-                  
CS_cff (goto l_i) kenv lj = \overline{X = E}; sw = i; break;
where (\overline{\phi}, s) = kenv l_i
             \overline{(X,E)} = CF_cff \overline{\phi} l_j
-}
cff_trans_stmt isReturnVoid localVars fargs ctxtName fname sw kenv l_j (AST.CBlockStmt (AST.CGoto l_i nodeInfo)) = case M.lookup l_i kenv of 
  { Just lb -> 
       let 
           asgmts  = cff_trans_phis ctxtName l_j l_i (lb_phis lb)
           i = AST.CConst (AST.CIntConst (cInteger (unLabPref l_i)) N.undefNode)
           sw_assg = AST.CBlockStmt (AST.CExpr (Just ((cvar sw) .=. i)) N.undefNode)
           
       in asgmts ++ [ sw_assg, cBreak ]
  ; Nothing -> error "cff_trans_stmt failed at a non existent label."
  }

{-                  
CS_cff (return e) kenv lj = res = (CE_cff e); return;

-- in the actual C backend, we do use the go() sub proc. We can return

CS_cff (return e) kenv lj = return (CE_cff e);
-}
cff_trans_stmt isReturnVoid localVars fargs ctxtName fname sw kenv l_i  (AST.CBlockStmt (AST.CReturn Nothing nodeInfo)) = [ (AST.CBlockStmt (AST.CReturn Nothing nodeInfo)) ]   

cff_trans_stmt isReturnVoid localVars fargs ctxtName fname sw kenv l_i  (AST.CBlockStmt (AST.CReturn (Just e) nodeInfo)) = 
  let       
    e' = cff_trans_exp localVars fargs ctxtName e
  in [ (AST.CBlockStmt (AST.CReturn (Just e') nodeInfo)) ] 
{-     
CS_cff (x = e; \overline{ x = e }; goto l_i) kenv lj = X = E; S
  where X = x
        E = CE_cff e
        S = CS_cff (\overline{ x = e }; goto l_i) kenv lj
-}

cff_trans_stmt isReturnVoid localVars fargs ctxtName fname sw kenv l_i (AST.CBlockStmt (AST.CCompound ids stmts nodeInfo)) = 
  let stmts' = cff_trans_stmts isReturnVoid localVars fargs ctxtName fname sw kenv l_i stmts
  in [AST.CBlockStmt (AST.CCompound ids stmts' nodeInfo)]

cff_trans_stmt isReturnVoid localVars fargs ctxtName fname sw kenv l_j (AST.CBlockStmt (AST.CExpr (Just e) nodeInfo)) =  -- exp
  let e' = cff_trans_exp localVars fargs ctxtName e
  in [AST.CBlockStmt (AST.CExpr (Just e') nodeInfo)]                                                                                                                        
{-     
CS_cff (if e { goto l_i} else { goto l_j }) kenv l_k  = if (CE_cff e) {Si} else { Sj}
  where Si = CS_cff (goto l_i) kenv l_k
        Sj = CS_cff (goto l_i) kenv l_k
-}
cff_trans_stmt isReturnVoid localVars fargs ctxtName fname sw kenv l_k (AST.CBlockStmt (AST.CIf e trueStmt (Just falseStmt) nodeInfo)) =      
  let e' =  cff_trans_exp localVars fargs ctxtName e
      trueStmt' = cff_trans_stmt isReturnVoid localVars fargs ctxtName fname sw kenv l_k (AST.CBlockStmt trueStmt)
      falseStmt' = cff_trans_stmt isReturnVoid localVars fargs ctxtName fname sw kenv l_k (AST.CBlockStmt falseStmt)
  in [ AST.CBlockStmt (AST.CIf e' (AST.CCompound [] trueStmt' N.undefNode) (Just (AST.CCompound [] falseStmt' N.undefNode)) nodeInfo) ]
     
cff_trans_stmt isReturnVoid localVars fargs ctxtName fname sw kenv l_k b = error ("cff_trans_stmt : unsupported case " ++ (render $ pretty b))

     
cff_trans_phis = cps_trans_phis
cff_trans_exp = cps_trans_exp
cff_trans_declaration = cps_trans_declaration
{-
CP_cff (t' f (t x) {\overline{d}; \overline{b} }) = T' F (T X) {\overline{D'}; sw = entry; ign = go() ; return res; }
   where T' = t'   F = f    T = t    X = x
         \overline{D} = CD_cff \overline{d}
         (kenv, l_entry) = B_ssa \overline{b}
         S = CB_cff \overline{b} kenv
         D = void => void go() => { S } 
         \overline{D'} = \overline{D}; D; T res; int sw; void ign;
-}
ssa2cff :: (AST.CFunctionDef N.NodeInfo) -> SSA -> CFF 
ssa2cff fundef ssa@(SSA scopedDecls labelledBlocks sdom local_decl_vars fargs) = 
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
      
      sw        = iid swParamName
      
      cases = cff_trans_blks isReturnVoid local_decl_vars fargs ctxtName (iid funName) labelledBlocks (M.toList labelledBlocks)
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
        , AST.CBlockDecl (AST.CDecl
                          [AST.CTypeSpec intTy]
                          [(Just (AST.CDeclr (Just sw) [] Nothing [] N.undefNode), Just (AST.CInitExpr (AST.CConst (AST.CIntConst (cInteger 0) N.undefNode)) N.undefNode), Nothing)] N.undefNode)
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
        , let (Just (var,rhs)) = containerDeclToInit $ dropStorageQual $ dropConstTyQual scopedDecl ] ++ 
        
        -- 4. initialize the sw = 0;
        -- [ AST.CBlockStmt (AST.CExpr (Just ((cvar sw) .=. (AST.CConst (AST.CIntConst (cInteger 0) N.undefNode)))) N.undefNode) ] ++ 
        -- 5. constructing the while (1) { switch sw of { ... } }
        [ cWhile (AST.CConst (AST.CIntConst (cInteger 1) N.undefNode)) [cSwitch (cvar sw) cases] ]       
        
      main_func = case fundef of 
        { AST.CFunDef tySpecfs declarator decls _ nodeInfo -> 
             AST.CFunDef tySpecfs declarator decls (AST.CCompound [] (main_decls ++ main_stmts) N.undefNode) nodeInfo 
        }
                  
        
  in CFF main_decls main_stmts context main_func
     
     
     
prettyCFF :: CFF -> String 
prettyCFF cff = 
  let declExts = map (\d -> AST.CDeclExt d) [(cff_ctxt cff)]
      funDeclExts = map (\f -> AST.CFDefExt f) [cff_main cff]
  in render $ pretty (AST.CTranslUnit (declExts ++ funDeclExts) N.undefNode)


-- ^ a CFF function declaration AST
data CFF = CFF { cff_decls :: [AST.CCompoundBlockItem N.NodeInfo]  -- ^ main function decls
               , cff_stmts :: [AST.CCompoundBlockItem N.NodeInfo]  -- ^ main function stmts
               , cff_ctxt  :: AST.CDeclaration N.NodeInfo -- ^ the context for the closure
               , cff_main ::  AST.CFunctionDef N.NodeInfo -- ^ the main function
               } deriving Show     
                          
                          
                          
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
      
      isReturnVoid   = isVoidDeclSpec returnType
      
      funcResult | isReturnVoid = [] 
                 | otherwise    = [AST.CDecl returnType [(Just (AST.CDeclr (Just $ iid "func_result") ptrArrs Nothing [] N.undefNode), Nothing, Nothing)] N.undefNode]
      decls'        = -- formal_arg_decls ++ 
        -- note: we remove local decl duplicate, maybe we should let different label block to have different type decl in the ctxt, see test/scoped_dup_var.c
                      concatMap (\d -> renameDeclWithLabeledBlocks d labeledBlocks local_decl_vars fargs) (nubBy declLHSEq $ map (cff_trans_declaration . dropConstTyQual . dropStorageQual) (formal_arg_decls ++ local_var_decls)) ++ funcResult
      tyDef         = AST.CStorageSpec (AST.CTypedef N.undefNode)
      structDef     =
        AST.CTypeSpec (AST.CSUType
                       (AST.CStruct AST.CStructTag (Just structName) (Just decls') attrs N.undefNode) N.undefNode) 
  in AST.CDecl [tyDef, structDef] [(Just ctxtAlias, Nothing, Nothing)] N.undefNode
                          
     
     
     
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
     
                                                             
                                                             
                                                             