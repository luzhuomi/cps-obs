

module Language.C.Obfuscate.SSA
       where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

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


-- ^ dominance tree
data DTree = DTBranch Ident [DTree] 
           | DTLeaf Ident
           deriving Show
                        
buildDTree :: CFG -> DTree
buildDTree cfg = 
  let sdom = buildSDom cfg
      -- sort the idents based on the size of the their sdom size, small to large
      ssdom = sortBy (\(i,doms) (j,doms') -> compare (S.size doms) (S.size doms')) $ M.toList sdom
      -- start from the nodes has no dominatees. build a parent-children map
      -- 
      buildCPM []           = []
      buildCPM [(i,_)]      = []
      buildCPM ((i,_):rest) = 
        -- since the list is sorted from least to largest, we find the first j in rest
        -- such that j `sdom` i
        case find (\(j,doms) -> i `S.member` doms) rest of 
          { Nothing -> error $ "Fatal: something is wrong, can't find the parent of a node." ++ show i 
          ; Just (j,doms) -> (i,j):(buildCPM rest)
          }
      
  in undefined
         