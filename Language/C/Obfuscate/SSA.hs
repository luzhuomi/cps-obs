

module Language.C.Obfuscate.SSA
       where

import qualified Data.Map as M
import qualified Data.Set as S

import Language.C.Obfuscate.CFG 
import Language.C.Data.Ident

                        
-- ^ reachbility function
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
         
    
type SDom = M.Map Ident (S.Set Ident)

                     
-- ^ build a sdom relation
-- a sdom b means for all path p in cfg starting from 0 to b, a is in p and a != b
-- in otherwrods b \in sdom(a)
-- sdom(a) = all_nodes - {a} - reach(0,{},a,cfg)
buildSDom :: CFG -> SDom
buildSDom cfg = 
  let idents = M.keys cfg
      allIds = S.fromList idents
  in foldl (\sdom ident -> 
             let 
               zero = internalIdent (labPref++"0") 
               notDominatees = S.fromList $ reach zero S.empty ident cfg
             in M.insert ident (allIds S.\\ (S.singleton ident) S.\\ notDominatees) sdom) M.empty idents


-- ^ dominace tree
data DTree = DTNode Ident [Ident] 
           deriving Show
                        
buildDTree :: CFG -> DTree
buildDTree 
         