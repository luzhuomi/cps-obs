module Language.C.Obfuscate.CFG 
       where

import Data.IntMap
import qualified Language.C.Syntax.AST as AST
import qualified Language.C.Data.Node as N 

-- the data type of the control flow graph

type Stmt = AST.CStatement N.NodeInfo

data CFG = CFG { nodes :: IntMap Node -- ^ NodeID -> Node
               , precs :: IntMap [NodeId] -- ^ Succ Node ID -> Prec Node IDs
               , succs :: IntMap [NodeId] -- ^ Prec Node Id -> Succ Node IDs
               } deriving (Show)
                 
data Node = Node { nodeID :: NodeId
                 , stmts  :: [Stmt]
                 } deriving (Show)
                   


{-
stmts ::= stmt | stmt; stmts 
stmt ::= x = stmt | if (exp) { stmts } else {stmts} | 
         return exp | exp | while (exp) { stmts } 
exp ::= exp op exp | (exp) | id | exp op | op exp | id (exp, ... exp) 
-}

type NodeId = Int

buildCFG :: [NodeId]           -- ^ I
            -> IntMap [NodeId] -- ^ S
            -> IntMap [Stmt]   -- ^ N
            -> [Stmt]          -- ^ stmts
            -> ([NodeId], IntMap [NodeId], IntMap [Stmt])

buildCFG i s n [] = (i,s,n)

{-
x \not\in lhs(stmts')
{i}, S, N \cup { i->stmts' ++ {x=exp} } |- stmts : I, S', N'
------------------------------------------------------------- (Assign1)
{i}, S, N\cup{i->stmts'} |- x=exp; stmts : I, S', N'
-}

buildCFG i s n (


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