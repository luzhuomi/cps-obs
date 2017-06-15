module Language.C.Obfuscate.Desugar
       where

import qualified Data.Map as Map
import qualified Language.C.Syntax.AST as AST
import qualified Language.C.Data.Node as N 
import Language.C.Data.Ident
import Control.Monad.State 

type FunDef = AST.CFunctionDef N.NodeInfo
type Stmt   = AST.CStatement N.NodeInfo

type Label = Ident

type LoopEnv = Map.Map Label (Label, Label)

type DesState = State LoopEnv

-- | desguaring, rewriting for and while loop into if with goto

class Desugar a where
  desugar :: a -> DesState a
  
instance Desugar a => Desugar [a] where
  desugar = mapM desugar 
  
instance Desugar a => Desugar (Maybe a) where
  desugar Nothing = return Nothing
  desugar (Just x) = do 
    { y <- desugar x
    ; return $ Just y 
    }
    

instance Desugar (AST.CFunctionDef a) where  
  desugar (AST.CFunDef tySpecfs declarator decls stmt a) = do {- stmt must be compound stmt -} 
    { stmt' <- desugar stmt
    ; return $ AST.CFunDef tySpecfs declarator decls stmt' a 
    }

instance Desugar (AST.CStatement a) where
  desugar (AST.CLabel label stmt attrs a) = do 
    { stmt' <- desugar stmt
    ; return $ AST.CLabel label stmt' attrs a 
    }
  desugar (AST.CCase exp stmt a) = do 
    { stmt' <- desugar stmt
    ; return $ AST.CCase exp stmt' a -- todo: desugar it to if? 
    }
  desugar (AST.CCases lower upper stmt a) = do 
    { stmt' <- desugar stmt
    ; return $ AST.CCases lower upper stmt' a -- todo: desugar it to if? 
    }
  desugar (AST.CDefault stmt a) = do 
    { stmt' <- desugar stmt
    ; return $ AST.CDefault stmt' a  -- todo: desugar it to if? 
    } 
  desugar (AST.CExpr mbExp a) = return $ AST.CExpr mbExp a
  desugar (AST.CCompound localLabels blockItems a) = do 
    { blockItems' <- mapM desugar blockItems
    ; return $ AST.CCompound localLabels blockItems' a
    }
  desugar (AST.CIf exp trueStmt mbFalseStmt a) = do 
    { trueStmt' <- desugar trueStmt
    ; mbFalseStmt' <- desugar mbFalseStmt
    ; return $ AST.CIf exp trueStmt' mbFalseStmt' a
    }
  desugar (AST.CSwitch exp swStmt a) = do 
    { swStmt' <- desugar swStmt
    ; return $ AST.CSwitch exp swStmt' a  -- todo: desugar it to if? 
    }
  desugar (AST.CWhile exp stmt False nodeInfo) = do   -- while loop
    {-
    ------------------------------------------
      while (exp) { stmt } => l1: if (exp) { goto l2; } else {goto l3}; l2: { stmt; goto l1 }; l3:
    -}
    { 
      
    }
    


instance Desugar (AST.CCompoundBlockItem a) where
  desugar = undefined