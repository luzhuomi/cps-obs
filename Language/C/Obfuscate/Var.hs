{-# LANGUAGE FlexibleInstances #-} 

-- operations dealing with variables.

module Language.C.Obfuscate.Var where


import Control.Monad.State as MS
import qualified Data.Map as M

import qualified Language.C.Syntax.AST as AST
import qualified Language.C.Data.Node as N 
import Language.C.Syntax.Constants
import Language.C.Data.Ident

-- ^ top level function
-- rename variables in block items and generate function scope declarations
renameBlkItemsGenDecls :: RenameState -> [AST.CCompoundBlockItem N.NodeInfo] ->  ([AST.CCompoundBlockItem N.NodeInfo], [AST.CDeclaration N.NodeInfo])
renameBlkItemsGenDecls rstate blkItems = case MS.runState (rename blkItems) rstate of 
  { (renamedBlkItems, rstate') -> (renamedBlkItems, local_decls rstate') }


-- variable renaming
data RenameState = RSt { lbl         :: Ident 
                       , rn_env      :: M.Map Ident Ident -- ^ variable -> renamed variable
                       , local_decls :: [AST.CDeclaration N.NodeInfo] -- ^ inner declaralation to be made to be function scope after renaming
                       }
                 deriving Show

class Renamable a where
  rename :: a -> MS.State RenameState a -- ^ just renaming based on what is in the r
  update :: a -> MS.State RenameState a -- ^ rename variable based on the label and update the copy of the variable in the map
  
  
instance Renamable a => Renamable [a] where 
  rename xs = mapM rename xs
  update xs = mapM update xs
  
instance (Renamable a, Renamable b) => Renamable (a,b) where  
  rename (x,y) = do { x' <- rename x
                    ; y' <- rename y
                    ; return (x',y')
                    }
  update (x,y) = do { x' <- update x
                    ; y' <- update y
                    ; return (x',y')
                    }
instance Renamable a => Renamable (Maybe a) where
  rename ma = case ma of 
    { Nothing -> return Nothing 
    ; Just a  -> do 
      { a' <- rename a
      ; return (Just a')
      }
    }
  update ma = case ma of
    { Nothing -> return Nothing 
    ; Just a  -> do 
      { a' <- rename a
      ; return (Just a')
      }
    }
  
instance (Renamable a, Renamable b) => Renamable (Either a b) where  
  rename (Left a) = do 
    { a' <- rename a
    ; return (Left a')
    }
  rename (Right b) = do 
    { b' <- rename b
    ; return (Right b')
    }
  update (Left a) = do 
    { a' <- update a
    ; return (Left a')
    }
  update (Right b) = do 
    { b' <- update b
    ; return (Right b')
    }
  

instance Renamable (AST.CCompoundBlockItem N.NodeInfo) where
  rename (AST.CBlockStmt stmt) = do 
    { stmt' <- rename stmt
    ; return $ AST.CBlockStmt stmt' 
    }
  rename (AST.CBlockDecl decl) = do 
    -- turn it into a renamed assignment statement 
    -- and move the declaration to the global level.
    { decl' <- rename decl
    ; let (decl'', stmt) = splitDecl decl'
    ; RSt lbl rn_env local_decls <- get
    ; put (RSt lbl rn_env (local_decls ++ [decl'']))
    ; return (AST.CBlockStmt stmt)
    } 
  update (AST.CBlockStmt stmt) = error "todo:update blockstmt"
  
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
    ; AST.CCompound localLabels blockItems nodeInfo -> do 
        { blockItems' <- rename blockItems
        ; return (AST.CCompound localLabels blockItems' nodeInfo)
        }
    ; AST.CIf cond iftrue mb_iffalse nodeInfo -> do 
        { cond'       <- rename cond
        ; iftrue'     <- rename iftrue
        ; mb_iffalse' <- rename mb_iffalse 
        ; return (AST.CIf cond' iftrue' mb_iffalse' nodeInfo)
        }
    ; AST.CSwitch exp stmt nodeInfo -> do 
        { exp'  <- rename exp
        ; stmt' <- rename stmt
        ; return (AST.CSwitch exp' stmt' nodeInfo)
        }
    ; AST.CWhile exp stmt isDoWhile nodeInfo -> do 
        { exp'  <- rename exp
        ; stmt' <- rename stmt
        ; return (AST.CWhile exp' stmt' isDoWhile nodeInfo) 
        }
    ; AST.CFor exp_or_decl mb_term_cond mb_incr stmt nodeInfo -> do
        { exp_or_decl'  <- rename exp_or_decl
        ; mb_term_cond' <- rename mb_term_cond
        ; mb_incr'      <- rename mb_incr
        ; stmt'         <- rename stmt
        ; return (AST.CFor exp_or_decl' mb_term_cond' mb_incr' stmt' nodeInfo)
        }
    ; AST.CGoto id nodeInfo -> return $ AST.CGoto id nodeInfo
    ; AST.CGotoPtr exp nodeInfo -> do 
      { exp' <- rename exp
      ; return $ AST.CGotoPtr exp' nodeInfo
      }
    ; AST.CCont nodeInfo -> return $ AST.CCont nodeInfo
    ; AST.CBreak nodeInfo -> return $ AST.CBreak nodeInfo 
    ; AST.CReturn mb_exp nodeInfo -> do 
      { mb_exp' <- rename mb_exp
      ; return $ AST.CReturn mb_exp' nodeInfo 
      }
    ; AST.CAsm asm_stmt nodeInfo -> return $ AST.CAsm asm_stmt nodeInfo                   
    -- ; _ -> error "todo:rename stmt"
    }
  update stmt = error "todo:update stmt"
  
instance Renamable (AST.CDeclaration N.NodeInfo) where  
  rename decl = case decl of 
    { AST.CDecl tyspec tripls nodeInfo -> do 
         { tripls' <- mapM (\(mb_decltr, mb_init, mb_size) -> do 
                               { mb_decltr' <- case mb_decltr of 
                                    { Nothing -> return Nothing
                                    ; Just decltr -> do 
                                      { decltr' <- rename decltr
                                      ; return $ Just decltr'
                                      }
                                    }
                               ; mb_init'   <- case mb_init of 
                                    { Nothing -> return Nothing
                                    ; Just init -> do 
                                      { init' <- rename init
                                      ; return $ Just init'
                                      }
                                    }
                               ; mb_size'   <- case mb_size of
                                    { Nothing   -> return Nothing
                                    ; Just size -> do 
                                      { size' <- rename size
                                      ; return $ Just size'
                                      }
                                    }
                               ; return (mb_decltr', mb_init', mb_size')
                               }) tripls
         ; return (AST.CDecl tyspec tripls' nodeInfo)
         }
    -- ; AST.CStaticAssert e lit nodeInfo -> undefined
    }
  update decl = error "todo:update declaration"

instance Renamable (AST.CDeclarator N.NodeInfo) where 
  rename (AST.CDeclr mb_ident derivs mb_lit attrs nodeInfo) = do 
    { rst <- get
    ; let env = rn_env rst
    ; mb_ident' <- case mb_ident of 
      { Nothing    -> return Nothing 
      ; Just ident -> case M.lookup ident env of  -- todo : please check again, maybe we should call update ident?
        { Nothing -> -- it is possible that it is not in the env
             let ident' = ident `app` (lbl rst)
                 env'   = M.insert ident ident' env
             in do 
               { put rst{rn_env=env'} 
               ; return $ Just ident'
               }
        ; Just ident'' -> -- it is possible that there is another local var with the same name defined somewhere else.
               let ident' = ident `app` (lbl rst)
                   env'   = M.update (\_ -> Just ident') ident env
               in do 
                 { put rst{rn_env=env'}
                 ; return $ Just ident'
                 }
        }
      }
    ; derivs' <- mapM (\deriv -> case deriv of 
                          { AST.CPtrDeclr ty nodeInfo1 -> return deriv
                          ; AST.CArrDeclr ty size nodeInfo1 -> do 
                            { size' <- rename size
                            ; return (AST.CArrDeclr ty size' nodeInfo1)
                            }
                          ; AST.CFunDeclr (Left idents) attrs nodeInfo1 -> return deriv -- todo : check what to do with idents (args?)
                          ; AST.CFunDeclr (Right (declrs, flag)) attrs nodeInfo1 -> return deriv -- todo: check what to do with the declrs (args?)
                          }) derivs
    ; mb_lit' <- return mb_lit
    ; attrs' <- rename attrs
    ; return (AST.CDeclr mb_ident' derivs' mb_lit' attrs' nodeInfo) 
    }
    
  update (AST.CDeclr mb_ident deriveds mb_lit attrs nodeInfo) = error "todo:update declarator"
  
instance Renamable (AST.CInitializer N.NodeInfo) where
  rename (AST.CInitExpr exp nodeInfo) = do 
    { exp' <- rename exp
    ; return (AST.CInitExpr exp' nodeInfo)
    }
  rename (AST.CInitList initList nodeInfo) = do 
    { initList' <- rename initList
    ; return (AST.CInitList initList' nodeInfo) 
    }
  update initalizer = error "todo:update initializer"
  
instance Renamable (AST.CPartDesignator N.NodeInfo) where
  rename (AST.CArrDesig exp nodeInfo) = do 
    { exp' <- rename exp
    ; return (AST.CArrDesig exp' nodeInfo) 
    }
  rename (AST.CMemberDesig ident nodeInfo) = do 
    { ident' <- rename ident
    ; return (AST.CMemberDesig ident' nodeInfo)
    }
  rename (AST.CRangeDesig low upp nodeInfo) = do 
    { low' <- rename low
    ; upp' <- rename upp
    ; return (AST.CRangeDesig low' upp' nodeInfo)
    } 
  update partDesign = error "todo:update PartDesignator"
  
instance Renamable (AST.CAttribute N.NodeInfo) where
  rename (AST.CAttr ident exps nodeInfo) = do 
    { ident' <- rename ident
    ; exps'  <- rename exps
    ; return (AST.CAttr ident' exps' nodeInfo) 
    }
  update attr = error "todo:update attr"
  
  
instance Renamable Ident where  
  rename ident = do  
    { st <- get
    ; let env = rn_env st 
    ; case M.lookup ident env of 
      { Nothing      -> return ident -- unbound means as formal arg
      ; Just renamed -> return renamed
      }
    }      
  update ident = do 
    { RSt lbl env decls <- get
    ; let renamed = ident `app` lbl
          env'    = upsert ident renamed env
    ; put (RSt lbl env' decls)
    ; return renamed
    }


                                        
instance Renamable (AST.CArraySize N.NodeInfo)   where
  rename (AST.CNoArrSize flag) = return (AST.CNoArrSize flag)
  rename (AST.CArrSize flag exp) = do 
    { exp' <- rename exp
    ; return (AST.CArrSize flag exp')
    }
  update arraySize = error "todo: update arraySize"
  
  
  
instance Renamable (AST.CExpression N.NodeInfo) where
  rename (AST.CAssign op lval rval nodeInfo) = do 
    { rval' <- rename rval  
    ; lval' <- update lval
    ; return (AST.CAssign op lval' rval' nodeInfo)
    }
  rename (AST.CComma exps nodeInfo) = do 
    { exps' <- rename exps
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
    ; args' <- rename args
    ; return (AST.CCall f' args' nodeInfo)
    }
  rename (AST.CMember e ident isDeRef nodeInfo) = do 
    { e' <- rename e
    ; return (AST.CMember e' ident isDeRef nodeInfo) 
    }
  rename (AST.CVar ident nodeInfo) = do 
    { ident' <- rename ident
    ; return (AST.CVar ident' nodeInfo)
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
    { ident' <- update ident
    ; return (AST.CVar ident' nodeInfo)
    }
  update (AST.CComma exps nodeInfo) = error "can't update comma expression"
  update (AST.CAssign op lval rval nodeInfo) = error "can't update assignment expression"
  update exp = error "can't update expression"
  
  
                                 

-- append two identifiers with _
app :: Ident -> Ident -> Ident
app (Ident s1 hash1 nodeInfo1) (Ident s2 hash2 nodeInfo2) = internalIdent $ s1 ++ "_" ++  s2

-- split a declaration into the declaration and a assignment statement
splitDecl :: AST.CDeclaration a -> (AST.CDeclaration a, AST.CStatement a)
splitDecl decl = case decl of 
    { AST.CDecl tyspec tripls nodeInfo -> 
         let getLVal mb_decltr = case mb_decltr of 
               { Nothing -> Nothing 
               ; Just (AST.CDeclr (Just id) derivs mb_lit attrs nodeInf2) -> 
                 Just (AST.CVar id nodeInf2) -- todo: what to do with the derivs, lit and attrs?
               }
             (tripls', stmts) = 
               foldl (\(ts, ss) (mb_decltr, mb_init, mb_size) -> 
                       let new_tripl = (mb_decltr, Nothing, mb_size) -- todo: what is size?
                           new_stmts = 
                             case mb_init of 
                               { Nothing   -> []
                               ; Just (AST.CInitExpr exp nodeInfo1) -> 
                                    case getLVal mb_decltr of 
                                      { Nothing   -> []
                                      ; Just lval -> 
                                           let op   = AST.CAssignOp 
                                               -- get the lval from the decltr
                                               rval = exp -- get the rval from the exp
                                               assignmt = AST.CAssign op lval rval nodeInfo1
                                           in [ AST.CExpr (Just assignmt) nodeInfo ] 
                                      }
                               ; Just (AST.CInitList iList nodeInfo1) -> error "todo: splitDecl init list is not supported" -- todo: can we do this in an assignment?
                               }
                       in (ts ++ [new_tripl], ss ++ new_stmts )
                     ) ([],[]) tripls 
         in case stmts of 
           { [ stmt ] -> (AST.CDecl tyspec tripls' nodeInfo, stmt)
           ; _        -> (AST.CDecl tyspec tripls' nodeInfo, AST.CCompound [] (map AST.CBlockStmt stmts) nodeInfo)
           }
                                      
    }

upsert :: Ord a =>  a -> b -> M.Map a b -> M.Map a b
upsert k v m = case M.lookup k m of
  { Nothing -> M.insert k v m 
  ; Just u  -> M.update (\_ -> Just v) k m
  }

