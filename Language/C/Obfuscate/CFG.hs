module Language.C.Obfuscate.CFG 
       where

import Data.Map
import qualified Language.C.Syntax.AST as AST
import qualified Language.C.Data.Node as N 
import Control.Monad.State as M
-- the data type of the control flow graph

type FunDef = AST.CFunctionDef N.NodeInfo
type Stmt   = AST.CStatement N.NodeInfo
type CFG = Map NodeId Node                          
                          
data Node = Node { stmts  :: [Stmt]
                 , preds :: [NodeId]
                 , succs :: [NodeId] 
                 } deriving (Show)
                   

type NodeId = Int

data StateInfo = StateInfo { currMaxNodeId :: NodeId
                           , cfg :: CFG
                           , currPreds ::[NodeId]
                           }
                 deriving (Show)

type CFGState = M.State StateInfo



buildCFGfrFunDec :: FunDef -> CFGState ()
buildCFGfrFunDec (AST.CFunDef tySpecfs declarator decls stmt nodeInfo) = 
  buildCFGfrStmt stmt {- stmt must be compound -}
  
buildCFGfrStmt :: Stmt -> CFGState () 
buildCFGfrStmt (AST.CLabel label stmt attrs nodeInfo) = 
  error "labelled stmt not supported."
buildCFGfrStmt (AST.CCase exp stmt nodeInfo) = 
  error "case stmt not supported."
buildCFGfrStmt (AST.CCases lower upper stmt nodeInfo) = 
  error "range case stmt not supported."
buildCFGfrStmt (AST.CDefault stmt nodeInfo) = 
  error "default case stmt not supported."
buildCFGfrStmt (AST.CExpr mbExp nodeInfo) = 
  error "expression stmt not supported."
  
{-  
CFG, max, preds |- stmt => CFG', max', preds'
----------------------------------------------
CFG, max, preds |- { stmt } => CFG', max', preds'  
-}
buildCFGfrStmt (AST.CCompound localLabels blockItems nodeInfo) =   
  undefined

{-  
max1 = max + 1
CFG1 = CFG \update { pred{succ = max1} |  pred <- preds } \union { max1{ stmts =  [ if exp { trueStmt } else { falseStmt } ], succ = [], preds = preds} }
CFG1, max1, {max1} |- trueStmt => CFG2, max2, preds1
CFG2, max2, {max1} |- falseStmt => CFG3, max3, preds2
-------------------------------------------------------------------------------------------
CFG, max, preds |- if exp { trueStmt } else { falseStmt }  => CFG3, max3, preds1 U preds2  
-}
buildCFGfrStmt (AST.CIf exp trueStmt mbFalseStmt nodeInfo) =
  undefined
buildCFGfrStmt (AST.CSwitch exp swStmt nodeInfo) = 
  error "switch statmt not supported."
  -- | switch statement @CSwitch selectorExpr switchStmt@, where
  -- @switchStmt@ usually includes /case/, /break/ and /default/
  -- statements
{-  

--------------------------------------------------------------------

-}
buildCFGfrStmt (AST.CWhile exp stmt isDoWhile nodeInfo) =   
  undefined
buildCFGfrStmt (AST.CFor init exp2 exp3 stmt nodeInfo) =   
  undefined
  -- | for statement @CFor init expr-2 expr-3 stmt@, where @init@ is
  -- either a declaration or initializing expression
buildCFGfrStmt (AST.CGoto ident nodeInfo) =   
  undefined
buildCFGfrStmt (AST.CGotoPtr exp nodeInfo) =   
  undefined
  -- | computed goto @CGotoPtr labelExpr@
buildCFGfrStmt (AST.CCont nodeInfo) = 
  undefined
  -- | continue statement
buildCFGfrStmt (AST.CBreak nodeInfo) =   
  undefined
  -- | break statement
buildCFGfrStmt (AST.CReturn mb_expression modeInfo) = 
  undefined
  -- | return statement @CReturn returnExpr@
buildCFGfrStmt (AST.CAsm asmb_stmt nodeInfo) = 
  error "asmbly statement not supported."


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