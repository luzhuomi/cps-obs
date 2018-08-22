module Language.C.Obfuscate.ASTUtils
       where
import Data.Char
import qualified Data.Map as M
import qualified Language.C.Syntax.AST as AST
import qualified Language.C.Data.Node as N 
import Language.C.Syntax.Constants
import Language.C.Data.Ident

-- import Language.C.Obfuscate.Var

-- some AST boilerplate to extract parts from the function declaration
getFunReturnTy :: AST.CFunctionDef N.NodeInfo -> 
                  ([AST.CDeclarationSpecifier N.NodeInfo], [AST.CDerivedDeclarator N.NodeInfo]) -- todo retrieve the pointers from declarator
getFunReturnTy (AST.CFunDef tySpecfs declarator decls stmt nodeInfo) = 
  let ty = filter (\tySpecf -> isTypeSpec tySpecf) tySpecfs 
      pointer_or_arrs = case declarator of 
        { AST.CDeclr mb_ident derivedDeclarators mb_strLit attrs nodeInfo' -> 
             filter (\derivedDeclarator -> not (isFunDeclr derivedDeclarator)) derivedDeclarators
        ; _ -> [] 
        }
  in (ty, pointer_or_arrs)
                 
isFunDeclr :: AST.CDerivedDeclarator N.NodeInfo -> Bool
isFunDeclr (AST.CFunDeclr _ _ _) = True
isFunDeclr _ = False

  -- todo retrieve the pointers from declarator

isTypeSpec :: AST.CDeclarationSpecifier N.NodeInfo -> Bool
isTypeSpec (AST.CTypeSpec _) = True
isTypeSpec _ = False


isStorageQual :: AST.CDeclarationSpecifier N.NodeInfo -> Bool
isStorageQual (AST.CStorageSpec _) = True
isStorageQual _ = False




isInlineFun :: AST.CFunctionDef N.NodeInfo -> Bool
isInlineFun (AST.CFunDef tySpecfs declarator decls stmt nodeInfo) = 
  any isInlineTyQual tySpecfs
  
isInlineTyQual :: AST.CDeclarationSpecifier N.NodeInfo -> Bool 
isInlineTyQual (AST.CFunSpec (AST.CInlineQual _)) = True
isInlineTyQual _ = False


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
getFormalArgsFromDerivedDeclarator (AST.CPtrDeclr typeQuals nodeInfo) = []
getFormalArgsFromDerivedDeclarator (AST.CArrDeclr typeQuals arraySize nodeInfo) = []
-- getFormalArgsFromDerivedDeclarator e = error $ "unhandled derived declarator " ++ show e  


getFormalArgIds :: (AST.CDeclaration N.NodeInfo) -> [Ident]
getFormalArgIds (AST.CDecl tySpecs trips nodeInfo) = concatMap (\(mb_decltr, mb_init, mb_exp) -> case mb_decltr of 
                                                                   { Nothing -> [] 
                                                                   ; Just (AST.CDeclr (Just id) derived mb_clit attrs _) -> [id]
                                                                   ; Just _ -> []
                                                                   }) trips


-- combinators for the ease of constructing the AST 

(.=.) :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.=.) lhs rhs = AST.CAssign AST.CAssignOp lhs rhs N.undefNode

(.==.) :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.==.) lhs rhs = AST.CBinary AST.CEqOp lhs rhs N.undefNode

-- ^ boolean or
(.||.) :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.||.) e1 e2 = AST.CBinary AST.CLorOp e1 e2 N.undefNode


(.->.) :: AST.CExpression N.NodeInfo -> Ident -> AST.CExpression N.NodeInfo
(.->.) struct member = AST.CMember struct member True N.undefNode

(...) :: AST.CExpression N.NodeInfo -> Ident -> AST.CExpression N.NodeInfo
(...) struct member = AST.CMember struct member False N.undefNode


(.+.) :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.+.) lhs rhs = AST.CBinary AST.CAddOp lhs rhs N.undefNode


(.-.) :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.-.) lhs rhs = AST.CBinary AST.CSubOp lhs rhs N.undefNode



(.*.) :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.*.) lhs rhs = AST.CBinary AST.CMulOp lhs rhs N.undefNode

(./.) :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(./.) lhs rhs = AST.CBinary AST.CDivOp lhs rhs N.undefNode


(.%.) :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.%.) lhs rhs = AST.CBinary AST.CRmdOp lhs rhs N.undefNode


(.&.) :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.&.) lhs rhs = AST.CBinary AST.CAndOp lhs rhs N.undefNode

(.^.) :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.^.) lhs rhs = AST.CBinary AST.CXorOp lhs rhs N.undefNode


(.|.) :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.|.) lhs rhs = AST.CBinary AST.COrOp lhs rhs N.undefNode


(.<<.) :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.<<.) lhs rhs = AST.CBinary AST.CShlOp lhs rhs N.undefNode

(.>>.) :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.>>.) lhs rhs = AST.CBinary AST.CShrOp lhs rhs N.undefNode


-- ^ array idex
(.!!.) :: AST.CExpression N.NodeInfo ->  AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
(.!!.) arr idx = AST.CIndex arr idx N.undefNode


-- ^ boolean not
cnot :: AST.CExpression N.NodeInfo -> AST.CExpression N.NodeInfo
cnot e = AST.CUnary AST.CNegOp e N.undefNode


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


isVoidDeclSpec :: [AST.CDeclarationSpecifier N.NodeInfo] -> Bool
isVoidDeclSpec [ AST.CTypeSpec (AST.CVoidType _) ] = True
isVoidDeclSpec _ = False


ptrTyArg :: [AST.CDeclarationSpecifier N.NodeInfo] -> -- ^ type decl
            Ident -> -- ^ arg name
            AST.CDeclaration N.NodeInfo
ptrTyArg ty arg = AST.CDecl ty [(Just (AST.CDeclr (Just arg) [AST.CPtrDeclr [] N.undefNode] Nothing [] N.undefNode),Nothing,Nothing)] N.undefNode              


(.::*.) :: Ident -> -- ^ arg name
           [AST.CDeclarationSpecifier N.NodeInfo] -> -- ^ type decl
           AST.CDeclaration N.NodeInfo
(.::*.) arg ty = ptrTyArg ty arg



isCaseStmt :: AST.CStatement N.NodeInfo -> Bool 
isCaseStmt (AST.CCase exp stmt nodeInfo) = True
isCaseStmt _ = False

isWhileStmt :: AST.CStatement N.NodeInfo -> Bool
isWhileStmt (AST.CWhile exp stmt False nodeInfo) = True
isWhileStmt _ = False

isForStmt :: AST.CStatement N.NodeInfo -> Bool
isForStmt (AST.CFor init test exp stmt nodeInfo) = True
isForStmt _ = False



isEmptyStmt :: AST.CStatement N.NodeInfo -> Bool
isEmptyStmt stmt = isEmptyCmpdStmt stmt || isEmptyExpStmt stmt

isEmptyCmpdStmt (AST.CCompound _ [] _) = True 
isEmptyCmpdStmt (AST.CCompound _ blks _) = all (\blk -> case blk of { AST.CBlockStmt (AST.CExpr Nothing _) -> True
                                                                    ; _ -> False }) blks
isEmptyCmpdStmt _ = False

isEmptyExpStmt (AST.CExpr Nothing _) = True
isEmptyExpStmt _ = False


cSwitch :: AST.CExpression N.NodeInfo -> [AST.CCompoundBlockItem N.NodeInfo] -> AST.CCompoundBlockItem N.NodeInfo
cSwitch e cases = 
  AST.CBlockStmt (AST.CSwitch e (AST.CCompound [] cases N.undefNode) N.undefNode)

cCase :: AST.CExpression N.NodeInfo -> [AST.CCompoundBlockItem N.NodeInfo] -> [AST.CCompoundBlockItem N.NodeInfo]
cCase e [] = [AST.CBlockStmt (AST.CCase e (AST.CExpr Nothing N.undefNode) N.undefNode)]
cCase e ((AST.CBlockStmt stmt):blkItems) = (AST.CBlockStmt (AST.CCase e stmt N.undefNode)):blkItems 

cBreak :: AST.CCompoundBlockItem N.NodeInfo 
cBreak = AST.CBlockStmt (AST.CBreak N.undefNode)


cWhile :: AST.CExpression N.NodeInfo -> [AST.CCompoundBlockItem N.NodeInfo] -> AST.CCompoundBlockItem N.NodeInfo
cWhile e stmts = 
  AST.CBlockStmt (AST.CWhile e (AST.CCompound [] stmts N.undefNode) False N.undefNode)




endsWithGoto :: [AST.CCompoundBlockItem N.NodeInfo] -> Bool 
endsWithGoto [] = False
endsWithGoto [AST.CBlockStmt (AST.CGoto succ _)] = True
endsWithGoto (_:xs) = endsWithGoto xs
      

endsWithBreak :: [AST.CCompoundBlockItem N.NodeInfo] -> Bool 
endsWithBreak [] = False
endsWithBreak [AST.CBlockStmt (AST.CBreak _)] = True
endsWithBreak (_:xs) = endsWithBreak xs

endsWithCont :: [AST.CCompoundBlockItem N.NodeInfo] -> Bool 
endsWithCont [] = False
endsWithCont [AST.CBlockStmt (AST.CCont _)] = True
endsWithCont (_:xs) = endsWithCont xs


endsWithReturn :: [AST.CCompoundBlockItem N.NodeInfo] -> Bool 
endsWithReturn [] = False
endsWithReturn [AST.CBlockStmt (AST.CReturn _ _)] = True
endsWithReturn (_:xs) = endsWithReturn xs
