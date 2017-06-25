

module Language.C.Obfuscate.SSA
       where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

import Language.C.Obfuscate.CFG 
import Language.C.Data.Ident
import qualified Language.C.Data.Node as N 



-- import for testing
import Language.C (parseCFile, parseCFilePre)
import Language.C.System.GCC (newGCC)
import Language.C.Pretty (pretty)
import Text.PrettyPrint.HughesPJ (render, text, (<+>), hsep)
import qualified Language.C.Syntax.AST as AST



testSSA = do 
  { let opts = []
  ; ast <- errorOnLeftM "Parse Error" $ parseCFile (newGCC "gcc") Nothing opts "test/fibiter.c"
  ; case ast of 
    { AST.CTranslUnit (AST.CFDefExt fundef:_) nodeInfo -> 
         case runCFG fundef of
           { CFGOk (_, state) -> do 
                { putStrLn $ show $ buildDTree (cfg state)
                ; putStrLn $ show $ buildDF (cfg state)
                }
           ; CFGError s       -> error s
           }
    ; _ -> error "not fundec"
    }
  }


                        
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
data DTree = DTNode Ident [DTree] 
           deriving Show
-- ^ another representation in hash table                    
type DTreeMap = M.Map Ident [Ident]
                        
buildDTree :: CFG -> Maybe (DTree, DTreeMap, SDom)
buildDTree cfg = 
  let sdom = buildSDom cfg
      -- sort the idents based on the size of the their sdom size, small to large
      ssdom = sortBy (\(i,doms) (j,doms') -> compare (S.size doms) (S.size doms')) $ M.toList sdom
      -- start from the nodes has no dominatees (mostly leaf node eventually). 
      -- build a parent-child list
      -- since the list ssdom is sorted from least to largest, we find the first j in rest
      -- such that j `sdom` i
      buildPCL []           = []
      buildPCL [(i,_)]      = []
      buildPCL ((i,_):rest) = 
        case find (\(j,doms) -> i `S.member` doms) rest of 
          { Nothing       ->
               error $ "Fatal in buildDTree. can't find the parent of a node." ++ show i 
          ; Just (j,doms) -> (j,i):(buildPCL rest)
          }
      -- turn the parent child list into a lookup table Parent -> [Child]
      pcm :: DTreeMap
      pcm = foldl (\m (p,c) -> case M.lookup p m of  
                      { Just children -> M.update (\_ -> Just (children ++ [c])) p m
                      ; Nothing       -> M.insert p [c] m
                      }) M.empty (buildPCL ssdom)
      buildTree pid pcm = 
        case M.lookup pid pcm of 
          { Nothing -> DTNode pid []
          ; Just children -> DTNode pid (map (\c -> buildTree c pcm) children)
          }
  in if not (null ssdom)   
     then Just $ (buildTree ((fst . last) ssdom) pcm, pcm, sdom)
     else Nothing
         
-- ^ dominace frontier
data DFTable = DFTable { idom :: M.Map Ident Ident -- ^ child to idom 
                       , df1  :: M.Map Ident [Ident] 
                       , dfup :: M.Map Ident [Ident]
                       , df   :: M.Map Ident [Ident]
                       }
               deriving Show


buildDF :: CFG -> Maybe DFTable 
buildDF cfg = 
  case buildDTree cfg of 
    { Nothing                 -> Nothing
    ; Just (dtree, pcm, sdom) ->  
      -- traversing the dtree in post order
      -- build the df1 and df, for leaf nodes df == df1
      -- for non-leaf nodes, df = df1 \union dfup(c) for all child node c.
      
      -- DF1(a) is the local dominance frontier for node a. 
      -- DF1(a) = { b | b \in succ(a), not (a sdom b) }
      -- e.g. DF1(3) = { 2 } because 2 \in succ(3), but not (3 sdom 2)

      -- Lemma: Let b \in succ(a), a = idom(b) iff a sdom b
      -- Hence DF1 can be computed easily via the following
      --  DF1(a) = { b | b \in succ(a), a != idom(b) }

      -- DFup(a) is the upwards dominance frontier for node a.
      -- DFup(a) = { b | b \in DF(a), not (idom(a) sdom b) }

      -- Similar to DF1
      -- DFup can be computed easily via the following
      -- DFup(a) = { b | b \in DF(a), idom(a) = c, idom(b) != c }

      -- DF(a) is the dominance frontier for node a.
      -- DF(a) = DF1(a) \union { b | c \in child(a), b \in DF(c), not (a sdom b) }
      -- Lemma: Let a be a leaf node in dom tree, then DF(a) == DF1(a)


      
      let 
        po :: [Ident]
        po = postOrder dtree
        
        idoms :: M.Map Ident Ident
        idoms = foldl (\m (p,cs) -> foldl (\m' c -> M.insert c p m') m cs) M.empty (M.toList pcm)
        
        findDF1 :: Ident -> DFTable -> [Ident]
        findDF1 a dft = case M.lookup a cfg of
          { Nothing -> [] 
          ; Just n  -> 
               let bs    = succs n 
                   idoms = idom dft
               in concatMap (\b -> case M.lookup b idoms of 
                                { Nothing               -> [] 
                                ; Just idom | a == idom -> []
                                            | otherwise -> [b] 
                                }) bs
          }                     
                     
        findDFup :: Ident -> DFTable -> [Ident]
        findDFup a dftable = case M.lookup a (df dftable) of 
          { Nothing -> []
          ; Just bs -> 
               concatMap (\b -> 
                           case (M.lookup b idoms, M.lookup a idoms) of
                             { (Just idb, Just ida) | idb /= ida -> [b]
                             ; _                                 -> [] 
                             }) bs
          }
                             
        findDF :: Ident -> [Ident] -> DFTable -> [Ident]
        findDF a a_df1 dftable  = 
          let {- a_df1 = case M.lookup a (df1 dftable) of
                { Nothing -> []
                ; Just bs -> bs } -} -- a_df1 needs to be passed in because dftable is partial
              a_sdom b = case M.lookup a sdom of
                { Nothing -> False
                ; Just doms -> b `S.member` doms
                }
              cs    = case M.lookup a pcm of 
                { Nothing -> [] 
                ; Just xs -> xs 
                }
              bs    = concatMap (\c -> case M.lookup c (df dftable) of 
                                    { Nothing -> []
                                    ; Just ys -> [ y | y <- ys, not (a_sdom y)]
                                    }
                                ) cs
          in a_df1 ++ bs
        empDFT = DFTable idoms M.empty M.empty M.empty

      in Just $ foldl (\dft id -> 
                        let df1'  = findDF1  id dft
                            dfup' = findDFup id dft
                            df'   = findDF id df1' dft
                            (DFTable idoms df1s dfups dfs) = dft
                        in dft{ df1  = (M.insert id df1' df1s)
                              , dfup = (M.insert id dfup' dfups)
                              , df   = (M.insert id df' dfs)
                              }
                      ) empDFT po
    }
  

lookupDF :: Ident -> DFTable -> [Ident]  
lookupDF id dft = case M.lookup id (df dft) of 
  { Nothing  -> []
  ; Just dfs -> dfs 
  }


-- def: we extend the definition of DF to set of nodes.
-- DF(A) = { b | a \in A, b \in DF(a) }

lookupDFs :: [Ident] -> DFTable -> [Ident]  
lookupDFs ids dft = concatMap (\id -> lookupDF id dft) ids

  
postOrder :: DTree -> [Ident]  
postOrder (DTNode ident dts) =  
  (concatMap postOrder (reverse dts)) ++ [ident]


{-
def: DF(1)(A) = DF(A)

def: DF(n)(A) = DF(n-1)(A) \union DF(DF(n-1)(A))

def: DF+(A) = DF(1)(A) \union DF(2)(A) \union ... 

Lemma: DF+(A) is finite. i.e the above reach a fix point.
-}

dfplus :: S.Set Ident -> DFTable -> S.Set Ident
dfplus ids dft = 
  let ids' = dfp ids dft
  in if (ids' `S.isSubsetOf` ids) && (ids `S.isSubsetOf` ids') 
     then ids
     else dfp (ids `S.union` ids') dft
       where dfp :: S.Set Ident -> DFTable -> S.Set Ident
             dfp s dft = S.fromList (lookupDFs (S.toList s) dft)
             
             
             
             
-- ^ phiLoc given a variable, identify the list of labelled block in SSA to insert the phi
phiLoc :: Ident -> CFG -> DFTable -> [Ident]             
phiLoc var cfg dft = 
  let modded = modLoc var cfg
  in S.toList (dfplus (S.fromList modded) dft)
      
                     
-- ^ retrieve the blocks from a CFG where a variable is being modified.
modLoc :: Ident -> CFG -> [Ident]                      
modLoc var cfg = map fst (filter (\(label, node) ->  (var `elem` lhsVars node)) (M.toList cfg))
          

data LabeledBlock = LB { phi :: [( Ident -- ^ var being redefined 
                                 , [(Ident, Ident)])] -- ^ incoming block x renamed variables
                       , stmts :: [AST.CCompoundBlockItem N.NodeInfo] -- ^ a compound stmt
                       , nexts :: [NodeId]
                       , loop  :: Bool
                       }
                    deriving Show
                             
data SSA = SSA { decls  :: [AST.CDeclaration N.NodeInfo]  -- ^ function wide global declaration
               , blocks :: M.Map Ident LabeledBlock       -- ^ translated labelled block
               }
           deriving Show
{-  The SSA Language
                  ___    _ _
(Prog)  p::= t x (t x) { d b }

(Decl)  d::= t x
                        _
(Block) b::= l:{s} | l:{i,s}

(Stmts) s::= x = e; s | goto l; | return e; | e; s | if e { s } else { s }
                     _
(Phi)   i::= x = phi(g)
                   _
(Exp)   e::= v | e(e)

(Labelled args) g::= l:v

(Labels) l::= l0 | l1 | ...

(Values) v::= x | c

(Types) t::= int | bool | t* | t[] | void

(Loop Envs) \delta = (l_if, e, l_t, l_f)
-}


buildSSA :: CFG -> SSA
buildSSA cfg = 
  case buildDF cfg of 
    { Nothing  -> error "failed to build dominance frontier table"
    ; Just dft -> 
      let localVars = allVars cfg
          -- build a mapping from label to the 
          -- set of variables that need to be merged
          phiLocMap :: M.Map Ident [Ident]
          phiLocMap = undefined
          
          
      in undefined
    }


insertGotos :: CFG -> CFG
insertGotos cfg = undefined

allVars :: CFG -> [Ident]
allVars cfg = S.toList (S.fromList (concatMap (\(i,n) -> lhsVars n) (M.toList cfg)))
  