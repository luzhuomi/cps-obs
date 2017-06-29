{-# LANGUAGE FlexibleInstances #-} 

-- operations dealing with variables.

module Language.C.Obfuscate.Var where


import Control.Monad.State as MS
import qualified Data.Map as M

import qualified Language.C.Syntax.AST as AST
import qualified Language.C.Data.Node as N 
import Language.C.Syntax.Constants
import Language.C.Data.Ident

-- variable renaming
data RenameState = RSt { lbl    :: Ident 
                       , rn_env :: M.Map Ident Ident -- ^ variable -> renamed variable
                       }
                 deriving Show

class Renamable a where
  rename :: a -> MS.State RenameState a -- ^ just renaming based on what is in the r
  update :: a -> MS.State RenameState a -- ^ rename variable based on the label and update the copy of the variable in the map
  
  
instance Renamable (AST.CCompoundBlockItem N.NodeInfo) where
  rename (AST.CBlockStmt stmt) = do 
    { stmt' <- rename stmt
    ; return $ AST.CBlockStmt stmt' 
    }
  rename (AST.CBlockDecl decl) = error "todo"
  update (AST.CBlockStmt stmt) = error "todo"
  
instance Renamable (AST.CStatement N.NodeInfo) where
  rename stmt = case stmt of 
    { AST.CLabel lbl stmt _ _       -> error "can't rename label stmt"
    ; AST.CCase exp stmt _          -> error "can't rename case statement"
    ; AST.CCases lower upper stmt _ -> error "can't rename case statement"
    ; AST.CDefault stmt _           -> error "can't rename default statement"
    ; AST.CExpr exp nodeInfo        -> 
      case exp of 
        { Nothing -> return (AST.CExpr Nothing nodeInfo)
        ; Just exp -> do 
             { exp' <- rename exp
             ; return (AST.CExpr (Just exp') nodeInfo)
             }
        } 
    ; _ -> error "todo"
    }
  update stmt = error "todo"
  
  
instance Renamable (AST.CExpression N.NodeInfo) where
  rename (AST.CAssign op lval rval nodeInfo) = do 
    { rval' <- rename rval  
    ; lval' <- update lval
    ; return (AST.CAssign op lval' rval' nodeInfo)
    }
  update exp = error "todo"
  
  
                                 





-- todo get it wrap up and leave it first.