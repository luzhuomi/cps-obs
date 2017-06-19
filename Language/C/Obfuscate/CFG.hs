{-# LANGUAGE FlexibleInstances #-} 
module Language.C.Obfuscate.CFG 
       where

import qualified Data.Map as M
import qualified Language.C.Syntax.AST as AST
import qualified Language.C.Data.Node as N 

import Language.C.Data.Ident

import Control.Applicative
import Control.Monad.State as M hiding (State)
-- the data type of the control flow graph

type FunDef   = AST.CFunctionDef N.NodeInfo
type Stmt     = AST.CStatement N.NodeInfo
type CFG      = M.Map NodeId Node                   
                          
data Node = Node { stmts  :: [AST.CCompoundBlockItem N.NodeInfo] -- ^ a compound stmt
                 , lhsVars :: [Ident] -- ^ all the variables appearing on the LHS of the assignment stmts
                 , rhsVars :: [Ident] -- ^ all the variables appearing on the RHS of the assignment stmts
                 , preds :: [NodeId]
                 , succs :: [NodeId] 
                 } deriving (Show)
                   

type NodeId = Ident

-- we use state monad for the ease of reporting error and keep track of the environment passing
data StateInfo = StateInfo { currId :: Int
                           , cfg :: CFG
                           , currPreds ::[NodeId]
                           , continuable :: Bool
                           , stmtUpdates :: M.Map NodeId (Ident -> Node -> Node) -- ^ callback to update the stmt in predecessor, used in case of while, if and goto
                           }




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


class CProg a  where
  buildCFG :: a -> State StateInfo ()
  
instance CProg (AST.CFunctionDef N.NodeInfo)  where
  buildCFG (AST.CFunDef tySpecfs declarator decls stmt nodeInfo)  = {- stmt must be compound -} 
    buildCFG stmt >> return ()
  

-- CFG, newNodeId, predIds, continuable |- stmt => CFG', newNodeId', predIds', continuable'

instance CProg (AST.CStatement N.NodeInfo) where
{-
CFG1 = CFG \update { pred : { succ = l } } \union { l : { stmts = goto max; } // goto max is not safe, the goto statement should be updated by the successor 
CFG1, max, {l}, false |- stmt => CFG2, max2, preds, continuable
------------------------------------------------------------------------------
CFG, max, preds, _ |- l: stmt => CFG2, max2, preds, continuable
-}
  buildCFG (AST.CLabel label stmt attrs nodeInfo) = do 
    { st <- get 
    ; let max        = currId st
          currNodeId = internalIdent ("obfs"++show max)
          cfg0       = cfg st 
          preds      = currPreds st
          stmtUps    = stmtUpdates st
          cfgNode    = Node [AST.CBlockStmt $ 
                             AST.CLabel label (AST.CGoto currNodeId nodeInfo) attrs nodeInfo] [] [] preds []
          cfg1'      = foldl (\g pred -> M.update (\n -> Just n{succs = [label]}) pred g) cfg0 preds
          cfg1       = M.insert label cfgNode cfg1

    ; put st{cfg = cfg1, currPreds=[label], continuable = False}
    ; buildCFG stmt 
    }
  buildCFG (AST.CCase exp stmt nodeInfo) = 
    fail $ (posFromNodeInfo nodeInfo) ++ " case stmt not supported."
  buildCFG (AST.CCases lower upper stmt nodeInfo) = 
    fail $ (posFromNodeInfo nodeInfo) ++ " range case stmt not supported."
  buildCFG (AST.CDefault stmt nodeInfo) = 
    fail $ (posFromNodeInfo nodeInfo) ++ " default case stmt not supported."
  buildCFG (AST.CExpr mbExp nodeInfo) = 
    fail $ (posFromNodeInfo nodeInfo) ++ " expression stmt not supported." 
  
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
                 currNodeId = internalIdent ("obfs"++show max)          
                 max1       = max + 1
                 cfg0       = cfg st 
                 preds      = currPreds st
                 stmtUps    = stmtUpdates st
                 -- note: we dont have max2 until we have CFG1
                 -- CFG1, max1, {max}, false |-n trueStmt => CFG2, max2, preds1, _ 
                 -- we can give an empty statement to the new CFG node in CFG1 first and update it
                 -- after we have max2,
                 cfgNode    = Node [] [] [] preds []
                 cfg1'      = foldl (\g pred -> 
                                      let stmtUp = case M.lookup pred stmtUps of 
                                            { Nothing -> \x -> id
                                            ; Just f  -> f
                                            }
                                      in M.update (\n -> Just $ stmtUp currNodeId n) pred g) cfg0 preds
                 cfg1       = M.insert currNodeId cfgNode cfg1'
                 stmtUps1   = M.insert currNodeId (\succNodeId -> \n -> n{succs = [succNodeId]}) stmtUps 
           ; put st{cfg = cfg1, currId=max1, currPreds=[currNodeId], continuable = False, stmtUpdates = stmtUps1}
           ; buildCFG trueStmt
           ; st1 <- get
           ; let max2      = currId st1
                 preds1    = currPreds st1
                 s         = AST.CBlockStmt $ AST.CIf exp  
                             (AST.CGoto (internalIdent ("obfs" ++ show max1)) nodeInfo) 
                             (Just (AST.CGoto (internalIdent ("obfs" ++ show max2)) nodeInfo)) nodeInfo
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
max1 = max + 1
CFG1 = CFG \update { pred : {succ = max} |  pred <- preds } \union { max : { stmts =  [ if exp { goto max1 } else { goto max2 } ], succ = [], preds = preds} } 
CFG1, max1, {max}, false |-n trueStmt => CFG2, max2, preds1, _ 


-- todo: max2 might not be the continuation. for instance, if {cond} { if {cond1} { stmt1 } else {stmt2} } else { stmt3 }; stmt4, stmt2 is not aware the next max id goes to stmt3 not the stmt4.
-- fixed: we make the \union { ... } operation as a call-back whenever 

-------------------------------------------------------------------------------------------------------------
CFG, max, preds, _ |- if exp { trueStmt }   => CFG2, max2, preds1 , false
-}                                
      ; Nothing -> undefined
      }
  buildCFG (AST.CSwitch exp swStmt nodeInfo) = 
    fail $ (posFromNodeInfo nodeInfo) ++ "switch stmt not supported."
  -- | switch statement @CSwitch selectorExpr switchStmt@, where
  -- @switchStmt@ usually includes /case/, /break/ and /default/
  -- statements
{-  
max1 = max + 1
CFG1 = CFG \update { pred : {succ = max} |  pred <- preds } \union { max: { stmts = [ if exp { goto max1 } else { goto max2 } ]
CFG1, max1, {max}, false |-n stmt => CFG2, max2, preds1, _ 
--------------------------------------------------------------------------------------
CFG, max, preds, _ |- while (exp) { stmt } => CFG2, max2, {max}, false
-}
  buildCFG (AST.CWhile exp stmt isDoWhile nodeInfo) =   
    undefined
  
{-  
CFG1 = CFG \update { pred : {stmts = stmt ++ init } } 
CFG1, max, preds, false |- while (exp2) { stmt; exp3 } => CFG2, max', preds', continuable
---------------------------------------------------------------------------------------    
CFG, max, preds, true |- for (init; exp2; exp3) { stmt }  => CFG2, max', preds', continuable


CFG1 = CFG \update { pred : {succ = max} |  pred <- preds }  \union { max : {stmts = init } } 
max1 = max + 1
CFG1, max1, {max}, false |- while (exp2) { stmt; exp3 } => CFG2, max2, preds2, continuable
---------------------------------------------------------------------------------------    
CFG, max, preds, false |- for (init; exp2; exp3) { stmt }  => CFG2, max2, preds2, continuable
-}


  buildCFG (AST.CFor init exp2 exp3 stmt nodeInfo) =   
    undefined
  -- | for statement @CFor init expr-2 expr-3 stmt@, where @init@ is
  -- either a declaration or initializing expression
  
{-
CFG1 = CFG \update { pred : {stmts = stmts ++ goto L } } 
--------------------------------------------------------
CFG, max, preds, true |- goto L => CFG1, max, preds, false 

max1 = max + 1
CFG1 = CFG \update { pred : {succ = max} |  pred <- preds } \union { max : {stmts = goto L } } 
--------------------------------------------------------
CFG, max, preds, false |- goto L => CFG1, max, preds, false 
-}

  buildCFG (AST.CGoto ident nodeInfo) =   
    undefined
  
  buildCFG (AST.CGotoPtr exp nodeInfo) =   
    undefined
  -- | computed goto @CGotoPtr labelExpr@
  buildCFG (AST.CCont nodeInfo) = 
    undefined
  -- | continue statement
  buildCFG (AST.CBreak nodeInfo) =   
    undefined
  -- | break statement
{-  
CFG1 = CFG \update { pred : {stmts = stmts ++ return exp } } 
--------------------------------------------------------
CFG, max, preds, true |- return exp  => CFG1, max, preds, false

max1 = max + 1
CFG1 = CFG \update { pred : {succ = max} |  pred <- preds } \union { max : {stmts = return exp } } 
--------------------------------------------------------
CFG, max, preds, false |- return exp => CFG1, max, preds, false 
-}
  buildCFG (AST.CReturn mb_expression modeInfo) = 
    undefined
  -- | return statement @CReturn returnExpr@
  buildCFG (AST.CAsm asmb_stmt nodeInfo) = 
    fail $ (posFromNodeInfo nodeInfo) ++  "asmbly statement not supported." 

instance CProg (AST.CCompoundBlockItem N.NodeInfo) where
  buildCFG (AST.CBlockStmt stmt) = buildCFG stmt
  buildCFG (AST.CBlockDecl decl) = error "decl not supported"
  buildCFG (AST.CNestedFunDef fundec) = error "nested function not supported"
  

  
posFromNodeInfo :: N.NodeInfo -> String
posFromNodeInfo (N.OnlyPos pos posLen) = show pos ++ ": \n"
posFromNodeInfo (N.NodeInfo pos posLen name) = show pos ++ ": \n"



{-
stmts ::= stmt | stmt; stmts 
stmt ::= x = stmt | if (exp) { stmts } else {stmts} | 
         return exp | exp | while (exp) { stmts } 
exp ::= exp op exp | (exp) | id | exp op | op exp | id (exp, ... exp) 
-}


{-
buildCFG :: [NodeId]           -- ^ I
            -> IntMap [NodeId] -- ^ S
            -> IntMap [Stmt]   -- ^ N
            -> [Stmt]          -- ^ stmts
            -> ([NodeId], IntMap [NodeId], IntMap [Stmt])

buildCFG i s n [] = (i,s,n)
-}
-- x must not be in the lhs of the existing CFG node, we don't need to create new CFG node.
{-
x  \not\in lhs(stmts') this is definitely
{i}, S, N \cup { i->stmts' ++ {T x=exp} } |- stmts : I, S', N'
------------------------------------------------------------- (Decl)
{i}, S, N\cup{i->stmts'} |- T x=exp; stmts : I, S', N'
-}
{-
buildCFG i s n (stmt@(AST.CBlockDecl (AST.CDecl 
                                      declSpecifiers -- e.g. type specifiers
                                      declInitCExps  -- e.g. [lhs = rhs, ... , ]
                                      nodeInfo)):stmts) 
  | (n!!i) `lhsContains` decInitExps = (i,s, update (\stmts -> Just (stmts++[stmt])) i n)  
  | otherwise                        = error "variable reinitialized."
  where lhsContains :: [Stmt] -> [(Maybe (CDeclarator N.NodeInfo), Maybe (CInitializer N.NodeInfo), Maybe (CExpression N.NodeInfo))] -> Bool 
        lhsContains 
-}
-- x is not in the lhs of the existing CFG node, we don't need to create new CFG node.
{-  
x \not\in lhs(stmts')
{i}, S, N \cup { i->stmts' ++ {x=exp} } |- stmts : I, S', N'
------------------------------------------------------------- (Assign1)
{i}, S, N\cup{i->stmts'} |- x=exp; stmts : I, S', N'
-}




{-
x \in lhs(stmts')  
j is fresh
{j}, S \pcup { i-> {j} }, N \cup { i->stmts', j->{x=exp}} |- stmst: I, S',N' 
-----------------------------------------------------------------------------(Assign2)
{i}, S, N\cup{i->stmts'} |- x=exp; stmts : I, S', N'

where
S \pcup { i->{j} } := S\i \cup { i -> S(i) \cup {j} } 
-}
       

{-
j, k, l are fresh
S' = S \cup{i->{j,k}}
N' = N \cup { i->stmts' \cup { if (cond) {stmts1} else {stmts2} } }
{j}, S', N' \cup {j->{}} |- stmts1, I1, S1, N1
{k}, S', N' \cup {k->{}} |- stmts2, I2, S2, N2
{l}, S1 \cup S2 \cup { n -> {m} | n \in I1\cup I2 }, N1 \cup N2 \cup {m->{}} |- stmts: I',S',N'
-------------------------------------------------------------------------------------------------- (If)
I,S,N\cup { i->stmts'} |- if (cond) { stmts1 } else { stmts2 }; stmts: I',S',N'
-}

{-
j, k are fresh
N' = N\cup{i->stmts'\cup{while(cond) {stmts1}}}
{j}, S, N' \cup {j -> {}} |- stmts1, I, S',N''
{k}, S',N''\cup {k -> {}} |- stmts, I', S'', N'''
-------------------------------------------------------------------------------------------------- (While)
{i}, S, N\cup{i->stmts'} |- while (cond) {stmts1}; stmts: I', S'', N'''

-}


{-
-------------------------------------------------------------------------------------------------- (Return)
{i}, S, N\cup{i->stmts'} |- return exp; stmts: {i},S, N\cup{i->stmts'\cup {return exp}} 

-}