module Language.C.Obfuscate.ASTUtils
       where
import Data.Char
import qualified Data.Map as M
import qualified Language.C.Syntax.AST as AST
import qualified Language.C.Data.Node as N 
import Language.C.Syntax.Constants
import Language.C.Data.Ident

import Language.C.Obfuscate.Var

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


isVoidDeclSpec :: [AST.CDeclarationSpecifier N.NodeInfo] -> Bool
isVoidDeclSpec [ AST.CTypeSpec (AST.CVoidType _) ] = True
isVoidDeclSpec _ = False