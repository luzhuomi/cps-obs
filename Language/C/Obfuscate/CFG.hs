{-# LANGUAGE FlexibleInstances #-} 

-- constructing a control flow graph from a C source file

module Language.C.Obfuscate.CFG 
       where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.C.Syntax.AST as AST
import qualified Language.C.Data.Node as N 
import Language.C.Syntax.Constants
import Language.C.Data.Ident

import Control.Applicative
import Control.Monad.State as M hiding (State)
-- the data type of the control flow graph

-- import for testing
import Language.C (parseCFile, parseCFilePre)
import Language.C.System.GCC (newGCC)
import Language.C.Pretty (pretty)
import Text.PrettyPrint.HughesPJ (render, text, (<+>), hsep)

testCFG = do 
  { let opts = []
  ; ast <- errorOnLeftM "Parse Error" $ parseCFile (newGCC "gcc") Nothing opts "test/fibiter.c"
  ; case ast of 
    { AST.CTranslUnit (AST.CFDefExt fundef:_) nodeInfo -> 
         case runCFG fundef of
           { CFGOk (_, state) -> putStrLn $ show (cfg state)
           ; CFGError s       -> error s
           }
    ; _ -> error "not fundec"
    }
  }

errorOnLeft :: (Show a) => String -> (Either a b) -> IO b
errorOnLeft msg = either (error . ((msg ++ ": ")++).show) return

errorOnLeftM :: (Show a) => String -> IO (Either a b) -> IO b
errorOnLeftM msg action = action >>= errorOnLeft msg




type FunDef   = AST.CFunctionDef N.NodeInfo
type Stmt     = AST.CStatement N.NodeInfo
type CFG      = M.Map NodeId Node                   


lookupCFG :: NodeId -> CFG -> Maybe Node 
lookupCFG id cfg = M.lookup id cfg
                          
data Node = Node { stmts   :: [AST.CCompoundBlockItem N.NodeInfo] -- ^ a compound stmt
                 , lhsVars :: [Ident] -- ^ all the variables appearing on the LHS of the assignment stmts
                 , rhsVars :: [Ident] -- ^ all the variables appearing on the RHS of the assignment stmts
                 , localDecls :: [Ident] -- ^ all the variables that are declared locally in this node.
                 , preds   :: [NodeId]
                 , succs   :: [NodeId]
                 , isLoop  :: Bool     -- ^ to indicate whether a if node is desugared from a loop (while / for loop)
                 } 
                   

instance Show Node where
  show (Node stmts lhs rhs local_decls preds succs loop) = 
    "\n Node = (stmts: " ++ (show (map (render . pretty) stmts)) ++ "\n succs: " ++ (show succs) ++  "\n lhsVars: " ++ (show lhs) ++ "\n localDecls: " ++ (show local_decls) ++ ")\n"

type NodeId = Ident



-- we use state monad for the ease of reporting error and keep track of the environment passing
data StateInfo = StateInfo { currId :: Int
                           , cfg :: CFG
                           , currPreds ::[NodeId]
                           , continuable :: Bool
                           -- , stmtUpdate :: M.Map NodeId (Ident -> Node -> Node) -- ^ callback to update the stmt in predecessor, used in case of while, if and goto
                           } deriving Show
                 
-- instance Show StateInfo where                 
--   show (StateInfo cId cfg currPreds continuable) = show cId ++ "\t" ++ show cfg

labPref :: String
labPref = "myLabel"


initStateInfo = StateInfo 0 M.empty [] False 



data CFGResult a  = CFGError String
                  | CFGOk a
                  deriving Show
                   
instance Functor CFGResult where
  -- fmap  :: (a -> b) -> CFGResult a -> CFGResult b
  fmap f ma = case ma of
    { CFGOk a -> CFGOk (f a)
    ; CFGError s -> CFGError s
    }
  
instance Applicative CFGResult where
  pure x = CFGOk x
  -- (<*>) :: CFGResult (a -> b) -> CFGResult a -> CFGResult b
  mf <*> ma = case mf of  
    { CFGOk f -> case ma of 
         { CFGOk a -> CFGOk (f a)
         ; CFGError s -> CFGError s
         }
    ; CFGError s -> CFGError s
    }
              
instance Alternative CFGResult where
  empty = CFGError "error"
  p <|> q = case p of 
    { CFGOk x -> CFGOk x
    ; CFGError s -> q 
    }
              

instance Monad CFGResult where
  return x = CFGOk x
  p >>= q  = case p of 
    { CFGOk a -> q a
    ; CFGError s -> CFGError s
    }
  fail mesg = CFGError mesg

instance MonadPlus CFGResult where 
  mzero = CFGError "error"
  p `mplus` q = case p of 
    { CFGOk a    -> CFGOk a
    ; CFGError _ ->  q
    }


type State s = M.StateT s CFGResult


runCFG :: AST.CFunctionDef N.NodeInfo -> CFGResult ((), StateInfo)
runCFG fundef = 
  runStateT (buildCFG fundef) initStateInfo
          

class CProg a  where
  buildCFG :: a -> State StateInfo ()
  
instance CProg (AST.CFunctionDef N.NodeInfo)  where
  buildCFG (AST.CFunDef tySpecfs declarator decls stmt nodeInfo)  = {- stmt must be compound -} 
    buildCFG stmt >> return ()
  

-- CFG, newNodeId, predIds, continuable |- stmt => CFG', newNodeId', predIds', continuable'

instance CProg (AST.CStatement N.NodeInfo) where
{-
CFG1 = CFG \update { pred : { succ = l } } \union { l : { stmts = goto max; } 
// todo maybe we shall add this goto statement later
CFG1, max, {l}, false |- stmt => CFG2, max2, preds, continuable
------------------------------------------------------------------------------
CFG, max, preds, _ |- l: stmt => CFG2, max2, preds, continuable
-}
  buildCFG (AST.CLabel label stmt attrs nodeInfo) = do 
    { st <- get 
    ; let max        = currId st
          currNodeId = internalIdent (labPref++show max)
          cfg0       = cfg st 
          preds0     = currPreds st
          cfgNode    = Node [AST.CBlockStmt $ 
                             AST.CLabel label (AST.CGoto currNodeId nodeInfo) attrs nodeInfo] [] [] [] preds0 [] False
          cfg1       = M.insert label cfgNode $ 
                       foldl (\g pred -> M.update (\n -> Just n{succs = [label]}) pred g) cfg0 preds0
    ; put st{cfg = cfg1, currPreds=[label], continuable = False}
    ; buildCFG stmt 
    }
  buildCFG (AST.CCase exp stmt nodeInfo) = 
    fail $ (posFromNodeInfo nodeInfo) ++ " case stmt not supported."
  buildCFG (AST.CCases lower upper stmt nodeInfo) = 
    fail $ (posFromNodeInfo nodeInfo) ++ " range case stmt not supported."
  buildCFG (AST.CDefault stmt nodeInfo) = 
    fail $ (posFromNodeInfo nodeInfo) ++ " default case stmt not supported."
{-
CFG1 = CFG \update { pred : {stmts = stmts ++ [x = exp], lhsVars = lhsVars ++ [x] } }  
x \not in {v | v \in lhsVars pred, pred \in preds }
--------------------------------------------------------
CFG, max, preds, true |-  x = exp => CFG1, max, [] , true 

max1 = max + 1
CFG1 = CFG \update { pred : {succ = max} |  pred <- preds } \union { max : {x = exp} } 
--------------------------------------------------------
CFG, max, preds, false |- x = exp => CFG1, max1, [], false 
-}
  
  buildCFG (AST.CExpr (Just (AST.CAssign op lval rval nodeInfo1)) nodeInfo) = do  
    { st <- get
    ; let lhs      = getLHSVarFromExp lval
          cfg0     = cfg st
          preds0   = currPreds st
          lhsPreds = S.fromList $ concatMap (\pred -> case (M.lookup pred cfg0) of 
                                                { Just n -> lhsVars n 
                                                ; Nothing -> [] 
                                                }) preds0
          s        = AST.CBlockStmt $ AST.CExpr (Just (AST.CAssign op lval rval nodeInfo1) ) nodeInfo
    ; if (continuable st && not (any (\x -> x `S.member` lhsPreds) lhs))
      then 
        let cfg1       = foldl (\g pred -> M.update (\n -> Just n{stmts=(stmts n) ++ [ s ], lhsVars=(lhsVars n)++lhs}) pred g) cfg0 preds0
        in do { put st{cfg = cfg1, continuable = True} }   

      else 
        let max        = currId st
            currNodeId = internalIdent (labPref++show max)          
            max1       = max + 1
            cfgNode    = Node [s] lhs [] [] preds0 [] False
            cfg1'      = foldl (\g pred -> M.update (\n -> Just n{succs = (succs n)++[currNodeId]} ) pred g) cfg0 preds0
            cfg1       = M.insert currNodeId cfgNode cfg1'
        in do { put st{cfg = cfg1, currId=max1, currPreds=[currNodeId], continuable = True} }
    }
  -- not assigment, pretty much the same as the assignment expression except that we don't care about the lhs vars
  buildCFG (AST.CExpr (Just exp) nodeInfo) =  do  
    { st <- get
    ; let cfg0     = cfg st
          preds0   = currPreds st
          s        = AST.CBlockStmt $ AST.CExpr (Just exp) nodeInfo
    ; if (continuable st)
      then 
        let cfg1       = foldl (\g pred -> M.update (\n -> Just n{stmts=(stmts n) ++ [ s ]}) pred g) cfg0 preds0
        in do { put st{cfg = cfg1, continuable = True} }   

      else 
        let max        = currId st
            currNodeId = internalIdent (labPref++show max)          
            max1       = max + 1
            cfgNode    = Node [s] [] [] [] preds0 [] False
            cfg1'      = foldl (\g pred -> M.update (\n -> Just n{succs = (succs n)++[currNodeId]} ) pred g) cfg0 preds0
            cfg1       = M.insert currNodeId cfgNode cfg1'
        in do { put st{cfg = cfg1, currId=max1, currPreds=[currNodeId], continuable = True} }
    }
    
  buildCFG (AST.CExpr Nothing nodeInfo) = do 
    fail $ (posFromNodeInfo nodeInfo) ++ " empty expression stmt not supported." 
  
{-  
CFG, max, preds, continuable |- stmt1 => CFG1, max1, preds1, continuable1
...
CFGn-1, maxn-1, predsn-1, continuablen-1 |- stmtn => CFGn, maxn, predsn, continuablen

---------------------------------------------------------------------------
CFG, max, preds, continuable |- { stmt1, ..., stmtN } => CFG', max', preds', continuable' 
-}
  buildCFG (AST.CCompound localLabels blockItems nodeInfo) = do 
    { mapM_ buildCFG blockItems
    ; return ()
    }

  buildCFG (AST.CIf exp trueStmt mbFalseStmt nodeInfo) = 
    case mbFalseStmt of 
{-  
max1 = max + 1
CFG1 = CFG \update { pred : {succ = max} |  pred <- preds } \union { max : { stmts =  [ if exp { goto max1 } else { goto max2 } ], succ = [], preds = preds} }
CFG1, max1, {max}, false |-n trueStmt => CFG2, max2, preds1, _ 
CFG2, max2, {max}, false |-n falseStmt => CFG3, max3, preds2, _
-------------------------------------------------------------------------------------------------------------
CFG, max, preds, _ |- if exp { trueStmt } else { falseStmt }  => CFG3, max3, preds1 U preds2, false
-}      
      { Just falseStmt -> do 
           { st <- get 
           ; let max        = currId st
                 currNodeId = internalIdent (labPref++show max)          
                 max1       = max + 1
                 cfg0       = cfg st 
                 preds0     = currPreds st
                 -- note: we dont have max2 until we have CFG1
                 -- CFG1, max1, {max}, false |-n trueStmt => CFG2, max2, preds1, _ 
                 -- we can give an empty statement to the new CFG node in CFG1 first and update it
                 -- after we have max2,
                 cfgNode    = Node [] [] [] [] preds0 [] False
                 cfg1'      = foldl (\g pred -> M.update (\n -> Just n{succs = (succs n) ++ [currNodeId]}) pred g) cfg0 preds0
                 cfg1       = M.insert currNodeId cfgNode cfg1'
                              
           ; put st{cfg = cfg1, currId=max1, currPreds=[currNodeId], continuable = False}
           ; buildCFG trueStmt
           ; st1 <- get
           ; let max2      = currId st1
                 preds1    = currPreds st1
                 s         = AST.CBlockStmt $ AST.CIf exp  
                             (AST.CGoto (internalIdent (labPref ++ show max1)) nodeInfo) 
                             (Just (AST.CGoto (internalIdent (labPref ++ show max2)) nodeInfo)) nodeInfo
                 cfg2      = cfg st1
                 -- add the stmt back to the curr node (If statement)
                 cfg2'     = M.update (\n -> Just n{stmts=[s]}) currNodeId cfg2
           ; put st1{cfg = cfg2', currId=max2, currPreds=[currNodeId], continuable = False}
           ; buildCFG falseStmt
           ; st2 <- get
           ; let max3      = currId st2
                 preds2    = currPreds st2
                 cfg3      = cfg st2
           ; put st{cfg = cfg3, currId=max3, currPreds=preds1 ++ preds2, continuable = False}
           }
{-  
CFG, max, preds, continuable |- if exp { trueStmt } else { nop } => CFG', max', preds', continuable' 
-------------------------------------------------------------------------------------------------------------
CFG, max, preds, continuable |- if exp { trueStmt }   => CFG', max', preds', continuable' 
-}                                
      ; Nothing -> buildCFG (AST.CIf exp trueStmt (Just $ AST.CCompound [] [] nodeInfo) nodeInfo) 
      }
  buildCFG (AST.CSwitch exp swStmt nodeInfo) = 
    fail $ (posFromNodeInfo nodeInfo) ++ "switch stmt not supported."
  -- | switch statement @CSwitch selectorExpr switchStmt@, where
  -- @switchStmt@ usually includes /case/, /break/ and /default/
  -- statements
{-  
max1 = max + 1
CFG1 = CFG \update { pred : {succ = max} |  pred <- preds ++ preds1 } \union { max: { stmts = [ if exp { goto max1 } else { goto max2 } ]
CFG1, max1, {max}, false |-n stmt => CFG2, max2, preds1, _ 
--------------------------------------------------------------------------------------
CFG, max, preds, _ |- while (exp) { stmt } => CFG2, max2, {max}, false
-}
  buildCFG (AST.CWhile exp stmt False nodeInfo) = do -- while
    { st <- get 
    ; let max        = currId st
          currNodeId = internalIdent (labPref++show max)          
          max1       = max + 1
          cfg0       = cfg st 
          preds0     = currPreds st
          -- note: we dont have max2 and preds1 until we have CFG1
          -- CFG1, max1, {max}, false |-n trueStmt => CFG2, max2, preds1, _ 
          -- we can give an empty statement to the new CFG node in CFG1 first and update it
          -- after we have max2,
          cfgNode    = Node [] [] [] [] preds0 [] True
          cfg1'      = foldl (\g pred -> M.update (\n -> Just n{succs = (succs n) ++ [currNodeId]}) pred g) cfg0 preds0
          cfg1       = M.insert currNodeId cfgNode cfg1'
    ; put st{cfg = cfg1, currId=max1, currPreds=[currNodeId], continuable = False}
    ; buildCFG stmt 
    ; st1 <- get
    ; let max2      = currId st1
          preds1    = currPreds st1
          s         = AST.CBlockStmt $ AST.CIf exp  
                      (AST.CGoto (internalIdent (labPref ++ show max1)) nodeInfo) 
                      (Just (AST.CGoto (internalIdent (labPref ++ show max2)) nodeInfo)) nodeInfo
          cfg2      = cfg st1
          cfg2'     = foldl (\g pred -> M.update (\n -> Just n{succs = (succs n) ++ [currNodeId]}) pred g) cfg2 preds1
          -- add the stmt back to the curr node (If statement)
          cfg2''     = M.update (\n -> Just n{stmts=[s], preds=(preds n)++preds1}) currNodeId cfg2'
    ; put st1{cfg = cfg2'', currId=max2, currPreds=[currNodeId], continuable = False}
    }
{-  
CFG, max, preds, continuable |- stmt => CFG1, max1, preds1, false 
CFG1, max1, preds1, false    |- while (exp) { stmt } => CFG2, max2, preds2, continuable
--------------------------------------------------------------------------------------
CFG, max, preds, continuable |- do  { stmt } while (exp) => CFG2, max2, {max}, false
-}
                                                  
  buildCFG (AST.CWhile exp stmt True nodeInfo) = do   -- do ... while
    { buildCFG stmt
    ; buildCFG (AST.CWhile exp stmt False nodeInfo)
    }
                                                                                                  
                                                 
                                                 
{-  

CFG, max, preds, continuable |- init => CFG1, max1, preds1, continuable1 
CFG1, max1, preds1, continuable1 |- while (exp2) { stmt; exp3 } => CFG2, max2, preds2, continuabl2
---------------------------------------------------------------------------------------    
CFG, max, preds, true |- for (init; exp2; exp3) { stmt }  => CFG2, max', preds', continuable

-}


  buildCFG (AST.CFor init exp2 exp3 stmt nodeInfo) = do 
    { _ <- case init of 
         { Right decl   -> buildCFG decl
         ; Left Nothing -> return ()
         ; Left exp     -> buildCFG (AST.CExpr exp nodeInfo)
         } 
    ; let exp2'      = case exp2 of 
            { Nothing -> AST.CConst (AST.CIntConst (cInteger 1) nodeInfo) -- true
            ; Just exp -> exp
            }
          stmt'      = case exp3 of 
            { Nothing -> stmt
            ; Just exp -> appStmt stmt (AST.CExpr exp3 nodeInfo)
            }
    ; buildCFG (AST.CWhile exp2' stmt' False nodeInfo)
    }
    where appStmt stmt1 stmt2 = case stmt1 of
            { AST.CCompound localLabels blockItems nodeInfo1 -> AST.CCompound localLabels (blockItems ++ [AST.CBlockStmt stmt2]) nodeInfo1
            ; _ -> AST.CCompound [] [AST.CBlockStmt stmt1, AST.CBlockStmt stmt2] nodeInfo 
            }

{-
  buildCFG (AST.CFor init exp2 exp3 stmt nodeInfo) = do 
    { st <- get
    ; if not (continuable st) 
      then
        let cfg0       = cfg st 
            preds0     = currPreds st
            (ss,lhs)   = case init of 
              { Right decl   -> [AST.CBlockDecl decl]
              ; Left Nothing -> []
              ; Left exp     -> [AST.CBlockStmt (AST.CExpr exp nodeInfo)]
              }
            cfg1       = foldl (\g pred -> M.update (\n -> Just n{stmts = (stmts n)++ss}) pred g) cfg0 preds0
            exp2'      = case exp2 of 
              { Nothing -> AST.CConst (AST.CIntConst (cInteger 1) nodeInfo) -- true
              ; Just exp -> exp
              }
            stmt'      = case exp3 of 
              { Nothing -> stmt
              ; Just exp -> appStmt stmt (AST.CExpr exp3 nodeInfo)
              }
        in do  
          { put st{cfg = cfg1, continuable = False}
          ; buildCFG (AST.CWhile exp2' stmt' False nodeInfo)
          }
      else 
        let max        = currId st
            currNodeId = internalIdent (labPref++show max)          
            max1       = max + 1
            cfg0       = cfg st 
            preds0     = currPreds st
            ss         = case init of 
              { Right decl   -> [AST.CBlockDecl decl]
              ; Left Nothing -> []
              ; Left exp     -> [AST.CBlockStmt (AST.CExpr exp nodeInfo)]
              }
            cfg1'      = foldl (\g pred -> M.update (\n -> Just n{succs = [currNodeId]}) pred g) cfg0 preds0
            cfgNode    = Node ss [] [] [] preds0 [] False
            cfg1       = M.insert currNodeId cfgNode cfg1'
            exp2'      = case exp2 of 
              { Nothing -> AST.CConst (AST.CIntConst (cInteger 1) nodeInfo) -- true
              ; Just exp -> exp
              }
            stmt'      = case exp3 of 
              { Nothing -> stmt
              ; Just exp -> appStmt stmt (AST.CExpr exp3 nodeInfo)
              }            
        in do 
          { put st{cfg = cfg1, currId=max1, currPreds=[currNodeId], continuable = False}
          ; buildCFG (AST.CWhile (exp2') stmt' False nodeInfo)
          }
    }
    where appStmt stmt1 stmt2 = case stmt1 of
            { AST.CCompound localLabels blockItems nodeInfo1 -> AST.CCompound localLabels (blockItems ++ [AST.CBlockStmt stmt2]) nodeInfo1
            ; _ -> AST.CCompound [] [AST.CBlockStmt stmt1, AST.CBlockStmt stmt2] nodeInfo 
            }
  -- | for statement @CFor init expr-2 expr-3 stmt@, where @init@ is
  -- either a declaration or initializing expression
-}  
{-
CFG1 = CFG \update { pred : {stmts = stmts ++ [goto L] } } 
--------------------------------------------------------
CFG, max, preds, true |- goto L => CFG1, max, [] , false 

max1 = max + 1
CFG1 = CFG \update { pred : {succ = max} |  pred <- preds } \union { max : {stmts = goto L } } 
--------------------------------------------------------
CFG, max, preds, false |- goto L => CFG1, max1, [], false 
-}

  buildCFG (AST.CGoto ident nodeInfo) = do 
    { st <- get 
    ; if (continuable st) 
      then 
        let cfg0       = cfg st 
            preds0     = currPreds st
            s          = AST.CBlockStmt $ AST.CGoto ident nodeInfo
            cfg1      = foldl (\g pred -> M.update (\n -> Just n{stmts=(stmts n) ++ [ s ]}) pred g) cfg0 preds0
        in do 
          { put st{cfg = cfg1, currPreds=[], continuable = False} }
      else 
        let max        = currId st
            currNodeId = internalIdent (labPref++show max)          
            max1       = max + 1
            cfg0       = cfg st 
            preds0     = currPreds st
            s          = AST.CBlockStmt $ AST.CGoto ident nodeInfo
            cfgNode    = Node [s] [] [] [] preds0 [] False
            cfg1'      = foldl (\g pred -> M.update (\n -> Just n{succs = (succs n)++[currNodeId]} ) pred g) cfg0 preds0
            cfg1       = M.insert currNodeId cfgNode cfg1'
        in do 
          {  put st{cfg = cfg1, currId=max1, currPreds=[], continuable = False} }
    }
  buildCFG (AST.CGotoPtr exp nodeInfo) =  
    fail $ (posFromNodeInfo nodeInfo) ++ "goto pointer stmt not supported."
  buildCFG (AST.CCont nodeInfo) = 
    fail $ (posFromNodeInfo nodeInfo) ++ "continue stmt not supported."
  buildCFG (AST.CBreak nodeInfo) =   
    fail $ (posFromNodeInfo nodeInfo) ++ "break stmt not supported."
{-  
CFG1 = CFG \update { pred : {stmts = stmts ++ [ return exp ] } } 
--------------------------------------------------------
CFG, max, preds, true |- return exp  => CFG1, max, [] , false

max1 = max + 1
CFG1 = CFG \update { pred : {succ = max} |  pred <- preds } \union { max : {stmts = return exp } } 
--------------------------------------------------------
CFG, max, preds, false |- return exp => CFG1, max, [], false 
-}
  buildCFG (AST.CReturn mb_expression modeInfo) = do 
    { st <- get 
    ; if (continuable st) 
      then  
        let cfg0       = cfg st 
            preds0     = currPreds st
            s          = AST.CBlockStmt $ AST.CReturn mb_expression modeInfo
            cfg1       = foldl (\g pred -> M.update (\n -> Just n{stmts=(stmts n) ++ [ s ]}) pred g) cfg0 preds0
        in do 
          { put st{cfg = cfg1, currPreds=[], continuable = False} }        
      else 
        let max        = currId st
            currNodeId = internalIdent (labPref++show max)          
            max1       = max + 1
            cfg0       = cfg st 
            preds0     = currPreds st
            s          = AST.CBlockStmt  $ AST.CReturn mb_expression modeInfo
            cfgNode    = Node [s] [] [] [] preds0 [] False
            cfg1'      = foldl (\g pred -> M.update (\n -> Just n{succs = (succs n)++[currNodeId]} ) pred g) cfg0 preds0
            cfg1       = M.insert currNodeId cfgNode cfg1'
        in do 
          {  put st{cfg = cfg1, currId=max1, currPreds=[], continuable = False} }
    }
  -- | return statement @CReturn returnExpr@
  buildCFG (AST.CAsm asmb_stmt nodeInfo) = 
    fail $ (posFromNodeInfo nodeInfo) ++  "asmbly statement not supported." 

instance CProg (AST.CCompoundBlockItem N.NodeInfo) where
  buildCFG (AST.CBlockStmt stmt) = buildCFG stmt
  buildCFG (AST.CBlockDecl decl) = buildCFG decl
  buildCFG (AST.CNestedFunDef fundec) = error "nested function not supported"
  
  
  
instance CProg (AST.CDeclaration N.NodeInfo) where
{-
CFG1 = CFG \update { pred : {stmts = stmts ++ [ty x = exp[]], lhsVars = lhsVars ++ [x] } } 
--------------------------------------------------------
CFG, max, preds, true |- ty x = exp[] => CFG1, max, [] , false 

max1 = max + 1
CFG1 = CFG \update { pred : {succ = max} |  pred <- preds } \union { max : {ty x = exp[] } } 
--------------------------------------------------------
CFG, max, preds, false |- ty x = exp[] => CFG1, max1, [], false 
-}
  
  buildCFG (AST.CDecl specs divs nodeInfo) = do 
    { st <- get
    ; if (continuable st) 
      then         
        let cfg0       = cfg st 
            preds0     = currPreds st
            s          = AST.CBlockDecl (AST.CDecl specs divs nodeInfo) 
            lvars      = getLHSVarsFromDecl divs
            cfg1       = foldl (\g pred -> 
                                 M.update (\n -> Just n{ stmts=(stmts n) ++ [ s ]
                                                       , localDecls = (localDecls n) ++ lvars
                                                       , lhsVars = (lhsVars n) ++ lvars }) pred g) cfg0 preds0
        in do 
          { put st{cfg = cfg1} }        
      else 
        let max        = currId st
            currNodeId = internalIdent (labPref++show max)          
            max1       = max + 1
            cfg0       = cfg st 
            preds0     = currPreds st
            s          = AST.CBlockDecl (AST.CDecl specs divs nodeInfo) 
            lvars      = getLHSVarsFromDecl divs
            cfgNode    = Node [s] lvars [] lvars preds0 [] False
            cfg1'      = foldl (\g pred -> 
                                 M.update (\n -> Just n{succs = (succs n)++[currNodeId]} ) pred g) cfg0 preds0
            cfg1       = M.insert currNodeId cfgNode cfg1'
        in do 
          {  put st{cfg = cfg1, currId=max1, currPreds=[currNodeId], continuable = True} }
    }
  {-
  buildCFG (AST.CStaticAssert expr str nodeInfo) = 
    fail $ (posFromNodeInfo nodeInfo) ++ "static assert decl not supported."
-}
  
-- print position info given the NodeInfo  
posFromNodeInfo :: N.NodeInfo -> String
posFromNodeInfo (N.OnlyPos pos posLen) = show pos ++ ": \n"
posFromNodeInfo (N.NodeInfo pos posLen name) = show pos ++ ": \n"



-- todo: move these to Var.hs?
-- aux functions retrieving LHS variables
getLHSVarsFromDecl :: [(Maybe (AST.CDeclarator a),  -- declarator (may be omitted)
                        Maybe (AST.CInitializer a), -- optional initialize
                        Maybe (AST.CExpression a))] -- optional size (const expr)
                      -> [Ident]
getLHSVarsFromDecl divs = 
  concatMap (\(mb_decl, mb_init, mb_ce) ->
              case mb_decl of 
                { Just (AST.CDeclr (Just ident) derivedDecl mb_strLit attrs nodeInfo) -> [ident]
                ; _                                                                   -> [] 
                }
            ) divs


getLHSVarFromExp :: AST.CExpression a -> [Ident]
getLHSVarFromExp (AST.CComma exps _)          = concatMap getLHSVarFromExp exps -- todo: fix me
getLHSVarFromExp (AST.CAssign op lval rval _) = getLHSVarFromExp lval
getLHSVarFromExp (AST.CVar ident _)           = [ident]
getLHSVarFromExp (AST.CIndex arr idx _ )      = getLHSVarFromExp arr
getLHSVarFromExp _                            = [] -- todo to check whether we miss any other cases




