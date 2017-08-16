

module Language.C.Obfuscate.SSA
       where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe (isJust)

import Language.C.Obfuscate.CFG 
import Language.C.Obfuscate.Var
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
  ; ast <- errorOnLeftM "Parse Error" $ parseCFile (newGCC "gcc") Nothing opts {- "test/sort.c" -- -} "test/fibiter.c"
  ; case ast of 
    { AST.CTranslUnit (AST.CFDefExt fundef:_) nodeInfo -> 
         case runCFG fundef of
           { CFGOk (_, state) -> do 
                { putStrLn $ show $ buildDTree (cfg state)
                ; putStrLn $ show $ buildDF (cfg state)
                ; putStrLn $ show $ buildSSA (cfg state)
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
          
          
parentOf :: Ident -> DTree -> Maybe Ident
parentOf lbl (DTNode lbl' dts) = 
  if lbl `elem` (map (\(DTNode lbl'' _) -> lbl'') dts)
  then Just lbl'
  else case filter (\r -> isJust r) $ map (\dt -> parentOf lbl dt) dts of
    { [] -> Nothing
    ; (Just p):_ -> Just p
    }
            
         
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
    ; Just (dtree, pcm, sdom) -> Just $ buildDF' (dtree, pcm, sdom, cfg) 
    }

buildDF' :: (DTree, DTreeMap, SDom, CFG) -> DFTable 
buildDF' (dtree, pcm, sdom, cfg) = 
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

  let po :: [Ident]
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

  in foldl (\dft id -> 
             let df1'  = findDF1  id dft
                 dfup' = findDFup id dft
                 df'   = findDF id df1' dft
                 (DFTable idoms df1s dfups dfs) = dft
             in dft{ df1  = (M.insert id df1' df1s)
                   , dfup = (M.insert id dfup' dfups)
                   , df   = (M.insert id df' dfs)
                   }
           ) empDFT po
    
  

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
          

data LabeledBlock = LB { phis :: [( Ident -- ^ var being redefined 
                                  , [(Ident, Ident)])] -- ^ incoming block x renamed variables
                       , lb_stmts :: [AST.CCompoundBlockItem N.NodeInfo] -- ^ a compound stmt
                       , lb_preds :: [NodeId]
                       , lb_succs :: [NodeId]
                       , lb_loop  :: Bool
                       }
                    deriving Show
                             
-- ^ a SSA function declaration AST. We only care about the body of the function. We 
-- apply translation on individual function.
data SSA = SSA { scoped_decls  :: [AST.CDeclaration N.NodeInfo]  -- ^ function wide global declaration
               , labelled_blocks :: M.Map Ident LabeledBlock       -- ^ translated labelled block
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
  case buildDTree cfg of 
    { Nothing  -> error "failed to build dominance frontier table"
    ; Just (dtree, pcm, sdom) -> 
      let dft = buildDF' (dtree, pcm, sdom, cfg) 
          localVars = allVars cfg
          -- build a mapping from node label to a 
          -- set of variables that need to be merged via phi func 
          -- (note: variables are not yet renamed)
          phiLocMap :: M.Map Ident -- node label
                       [Ident]     -- variable idents
          phiLocMap = foldl (\m (label,v) -> case M.lookup label m of 
                                { Nothing -> M.insert label [v] m 
                                ; Just vs -> M.update (\_ -> Just $ vs ++ [v]) label m 
                                }) M.empty $ do 
            { var <- localVars
            ; let phiLocs = phiLoc var cfg dft
            ; phiLoc <- phiLocs 
            ; return (phiLoc, var)
            }
                      
          -- given a node's label (as the starting point of the search)
          -- a variable (which is not yet renamed), the dom tree
          -- to find the label of the node which contains preceding definition of the variable 
          precDef :: Ident -> Ident -> DTree -> CFG -> Ident
          precDef currLbl var dt cfg = 
            case M.lookup currLbl phiLocMap of
              { Just vs | var `elem` vs -> currLbl  -- the var is reassigned through a phi function in the current block
              ; _                       ->          -- the var could be reassigned locally
                   case M.lookup currLbl cfg of
                     { Nothing   -> error $ "Fatal: Label " ++ show currLbl ++ " is not found in the CFG"
                     ; Just node | var `elem` (lhsVars node) ->  currLbl
                     ; Just _ | otherwise -> 
                       case parentOf currLbl dt of 
                         { Nothing -> -- no parent: this could be due to a nested scope var in some inner block of if else or while. 
                              -- we will move it to block 0 (the init block) 
                              -- e.g. refer to the fibiter.c, the variable t
                              currLbl
                         ; Just parentLbl -> precDef parentLbl var dt cfg
                         }
                     }
              }
                    
          -- renaming all the variables based on the individual labelled blocks
          -- and move all the declaration out.
          
          eachNode :: SSA -> (Ident, Node) -> SSA
          eachNode ssa (currLbl, node) = case node of 
            { Node statements lvars rvars local_decls precLbls succLbls isloop -> 
                 let phis_ :: [( Ident -- ^ var being redefined (not yet renamed)
                              , [(Ident, Ident)])] -- ^ (incoming block lbl, lbl of preceding blk in which var is redefined)
                     phis_ = case M.lookup currLbl phiLocMap of 
                       { Nothing      -> []
                       ; Just phiVars -> map (\v -> 
                                               let lastDefs = 
                                                     map (\precLbl -> 
                                                           let lblVarDefined =  precDef precLbl v dtree cfg
                                                           in (precLbl, lblVarDefined)) precLbls
                                               in (v, lastDefs)
                                                  -- find the preceding node and the renamed variable
                                             ) [ v |  v <- phiVars, not (v `elem` local_decls) ]
                       }
                     -- build the renaming state from the rhs vars with the precDef
                     -- or from phis_
                     rnState :: RenameState
                     rnState = let -- todo, the local_decls should be filtered away from the rhsVars and local_decls should be appended with the current node label
                                   rnEnvLocal = M.fromList $  map (\var -> (var, var `app` currLbl)) local_decls
                                   rvarsNotLocal = filter (\var -> not (var `M.member` rnEnvLocal)) rvars
                                   rnEnv = case precLbls of  
                                     { [] -> -- entry block 
                                          M.fromList (map (\var -> (var, var `app` currLbl)) rvarsNotLocal)
                                     ; [precLbl] -> -- non-phi block
                                          M.fromList (map (\var -> (var, var `app` (precDef precLbl var dtree cfg))) rvarsNotLocal)
                                     ; _ -> -- phi block
                                          M.fromList (map (\var -> (var, var `app` currLbl)) rvarsNotLocal)
                                     }
                               in RSt currLbl (rnEnvLocal `M.union` rnEnv) []
                             
                     renamedBlkItems_n_decls :: ([AST.CCompoundBlockItem N.NodeInfo], [AST.CDeclaration N.NodeInfo])
                     renamedBlkItems_n_decls = renamePure rnState statements 
                     (renamedBlkItems, new_decls) = renamedBlkItems_n_decls
                              
                     labelled_block = LB phis_ renamedBlkItems precLbls succLbls isloop
                 in ssa{ labelled_blocks = M.insert currLbl labelled_block (labelled_blocks ssa)
                       , scoped_decls    = (scoped_decls ssa) ++ new_decls }
            } 
          ssa =  foldl eachNode (SSA [] M.empty) $ M.toList cfg
      in ssa{scoped_decls = map (\decl -> renameLabel0 decl) (scoped_decls ssa)}  -- the scoped_delcs were not yet renamed.
    }



                 
renameLabel0 :: AST.CDeclaration N.NodeInfo ->  AST.CDeclaration N.NodeInfo
renameLabel0 decl = 
  let label0 = internalIdent $ labPref ++ "0" 
      rnState = RSt label0 M.empty [] 
  in case renamePure rnState decl of 
    { (decl', rstate') -> decl' }
      



allVars :: CFG -> [Ident]
allVars cfg = S.toList (S.fromList (concatMap (\(i,n) -> lhsVars n) (M.toList cfg)))
  