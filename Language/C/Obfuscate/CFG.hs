module Language.C.Obfuscate.CFG 
       where

import Data.IntMap
import qualified Language.C.Syntax.AST as AST
import qualified Language.C.Data.Node as N 

-- the data type of the control flow graph

type Stmt = AST.CStatement N.NodeInfo

data CFG = CFG { nodes :: IntMap Node -- ^ NodeID -> Node
               , precs :: IntMap [Int] -- ^ Succ Node ID -> Prec Node IDs
               , succs :: IntMap [Int] -- ^ Prec Node Id -> Succ Node IDs
               } deriving (Show)
                 
data Node = Node { nodeID :: Int
                 , stmts  :: [Stmt]
                 } deriving (Show)
                   


{-
stmts ::= stmt | stmt; stmts 
stmt ::= x = stmt | if (exp) { stmts } else {stmts} | 
         return exp | exp | while (exp) { stmts } 
exp ::= exp op exp | (exp) | id | exp op | op exp | id (exp, ... exp) 



-}
       