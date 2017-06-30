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
  rename (AST.CComma exps nodeInfo) = do 
    { exps' <- mapM rename exps
    ; return (AST.CComma exps' nodeInfo) 
    }

  rename (AST.CCond e1 mb_e2 e3 nodeInfo) = do 
    { e1' <- rename e1
    ; mb_e2' <- case mb_e2 of 
      { Nothing -> return Nothing
      ; Just e2 -> do { e2' <- rename e2
                      ; return $ Just e2' 
                      }
      }
    ; e3' <- rename e3
    ; return (AST.CCond e1' mb_e2' e3' nodeInfo)
    }
  rename (AST.CBinary op e1 e2 nodeInfo) = do 
    { e1' <- rename e1
    ; e2' <- rename e2
    ; return (AST.CBinary op e1' e2' nodeInfo)
    }
  rename (AST.CCast ty e nodeInfo) = do 
    { e' <- rename e
    ; return (AST.CCast ty e' nodeInfo)
    }
  rename (AST.CUnary op e nodeInfo) = do 
    { e' <- rename e
    ; return (AST.CUnary op e' nodeInfo) 
    }
  rename (AST.CSizeofExpr e nodeInfo) = do 
    { e' <- rename e
    ; return (AST.CSizeofExpr e' nodeInfo) 
    }
  rename (AST.CSizeofType t nodeInfo) = return (AST.CSizeofType t nodeInfo)
  rename (AST.CAlignofExpr e nodeInfo) = do 
    { e' <- rename e
    ; return (AST.CAlignofExpr e' nodeInfo)
    }
  rename (AST.CAlignofType t nodeInfo) = return (AST.CAlignofType t nodeInfo)
  rename (AST.CComplexReal e nodeInfo) = do 
    { e' <- rename e
    ; return (AST.CComplexReal e' nodeInfo)
    }
  rename (AST.CComplexImag e nodeInfo) = do 
    { e' <- rename e
    ; return (AST.CComplexImag e' nodeInfo)
    }
  rename (AST.CIndex a e nodeInfo) = do 
    { a' <- rename a
    ; e' <- rename e
    ; return (AST.CIndex a' e' nodeInfo)
    }
  rename (AST.CCall f args nodeInfo) = do 
    { f' <- rename f
    ; args' <- mapM rename args
    ; return (AST.CCall f' args' nodeInfo)
    }
  rename (AST.CMember e ident isDeRef nodeInfo) = do 
    { e' <- rename e
    ; return (AST.CMember e' ident isDeRef nodeInfo) 
    }
  rename (AST.CVar ident nodeInfo) = do 
    { RSt lbl env <- get
    ; case M.lookup ident env of 
      { Nothing      -> return (AST.CVar ident nodeInfo) -- unbound means as formal arg
      ; Just renamed -> return (AST.CVar renamed nodeInfo) 
      }
    }      
  rename (AST.CConst const) = return (AST.CConst const)
  {-
  rename (AST.CGenericSelection e declExps nodeInfo) = do 
    { e' <- rename e
    ; declExps' <- mapM (\(mb_decl, exp) -> case mb_decl of 
                            { Nothing -> do 
                                 { exp' <- rename exp
                                 ; return (Nothing, exp') 
                                 }
                            ; Just decl -> do  -- todo rename decl?
                                 { exp' <- rename exp
                                 ; return (Just decl, exp')
                                 }
                            }) declExps
    ; return (AST.CGenericSelection e' declExps' nodeInfo)
    }
  -}
  rename (AST.CStatExpr stmt nodeInfo) = do 
    { stmt' <- rename stmt
    ; return (AST.CStatExpr stmt' nodeInfo)
    }
  rename (AST.CLabAddrExpr ident nodeInfo) = return (AST.CLabAddrExpr ident nodeInfo) -- todo : check
  rename (AST.CBuiltinExpr buildin) = return (AST.CBuiltinExpr buildin) -- todo : check
  
  update (AST.CVar ident nodeInfo) = do 
    { RSt lbl env <- get
    ; let renamed = ident `app` lbl
          env'    = upsert ident renamed env
    ; put (RSt lbl env')
    ; return (AST.CVar renamed nodeInfo)
    }
  update (AST.CComma exps nodeInfo) = error "can't update comma expression"
  update (AST.CAssign op lval rval nodeInfo) = error "can't update assignment expression"
  update exp = error "can't update expression"
  
  
                                 


app :: Ident -> Ident -> Ident
app (Ident s1 hash1 nodeInfo1) (Ident s2 hash2 nodeInfo2) = internalIdent $ s1 ++ "_" ++  s2



upsert :: Ord a =>  a -> b -> M.Map a b -> M.Map a b
upsert k v m = case M.lookup k m of
  { Nothing -> M.insert k v m 
  ; Just u  -> M.update (\_ -> Just v) k m
  }

-- todo get it wrap up and leave it first.