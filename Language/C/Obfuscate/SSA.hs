

module Language.C.Obfuscate.SSA
       where


import qualified Data.Set as S

import Language.C.Obfuscate.CFG 
import Language.C.Data.Ident

data DomTree a = DTNode a [a] 
               deriving Show
                        
                        
-- reachbility
-- given a CFG, a set of nodes to be excluded, compute the set of nodes that are reachable from the source node, by following the path
-- the context keep tracks of nodes that have been 
reach :: Ident   -> -- ^ source node
         S.Set Ident -> -- ^ nodes already in the context
         Ident -> -- ^ a node to exclude
         CFG     ->
         [Ident] 
reach src context exclude cfg
  | src `S.member` context = []
  | src == exclude         = []
  | otherwise              = 
    case lookupCFG src cfg of 
      { Nothing   -> [] 
      ; Just node -> src:((concatMap (\succ -> reach succ (S.insert src context) exclude cfg)) (succs node))
      }
         
         