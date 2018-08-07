

module Language.C.Obfuscate.SSA
       where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe (isJust)

import System.IO.Unsafe (unsafePerformIO)

import Language.C.Obfuscate.CFG
import Language.C.Obfuscate.Var
import Language.C.Obfuscate.ASTUtils
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
  ; ast <- errorOnLeftM "Parse Error" $ parseCFile (newGCC "gcc") Nothing opts "test/sort.c" -- -} "test/fibiter.c"
  ; case ast of
    { AST.CTranslUnit (AST.CFDefExt fundef:_) nodeInfo ->
         case runCFG fundef of
           { CFGOk (_, state) -> do
                { putStrLn $ show $ buildDTree (cfg state)
                ; putStrLn $ show $ buildDF (cfg state)
                ; putStrLn $ show $ buildSSA (cfg state) (formalArgs state)
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


domBy :: SDom ->
         Ident -> -- ^ dominator
         Ident -> -- ^ dominatee
         Bool
domBy sdom dtor dtee = (dtor == dtee) || (sdomBy sdom dtor dtee)

sdomBy :: SDom ->
          Ident -> -- ^ dominator
          Ident -> -- ^ dominatee
          Bool
sdomBy sdom dtor dtee = case M.lookup dtor sdom of
  { Nothing -> False
  ; Just dominatees -> dtee `S.member` dominatees
  }


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
  --
  -- WAIT, the reference says

  -- DF(a) = DF1(a) \union { b | c \in child(a), b \in DFup(c) }
  -- They are they same!
  -- by definition of DFup(c)
  --       = DF1(a) \union { b | c \in child(a), b \in DF(c), not (idom(c) sdom b) }
  --       = DF1(a) \union { b | c \in child(a), b \in DF(c), not (a sdom b) }
  -- we don't need to compute DFup -- todo, remove DFup or use it in computing DF(a)
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
                 df1 = concatMap (\b -> case M.lookup b idoms of
                              { Nothing               -> [b] -- shouldn't be [], b has no idoms, means it is 0
                              ; Just idom | a == idom -> []
                                          | otherwise -> [b]
                              }) bs
                 -- io = unsafePerformIO $ print a >> print idoms >> print bs >> print df1                       
             in df1
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
      -- io = unsafePerformIO $ print po >> print cfg
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
{-
-- this does not work, once a phi is created, a new modded loc is also introduced
phiLoc var cfg dft =
  let modded = modLoc var cfg
  in S.toList (dfplus (S.fromList modded) dft)
-}
phiLoc var cfg dft =
  let modded = modLoc var cfg
      go :: S.Set Ident -> S.Set Ident -> DFTable -> S.Set Ident
      go modded curr_phiLocs dft =
        let next_phiLocs = dfplus modded dft
        in if next_phiLocs `S.isSubsetOf` curr_phiLocs
           then curr_phiLocs
           else go (modded `S.union` next_phiLocs) (curr_phiLocs `S.union` next_phiLocs) dft
  in S.toList (go (S.fromList modded) S.empty dft)



-- ^ retrieve the blocks from a CFG where a variable is being modified.
modLoc :: Ident -> CFG -> [Ident]
modLoc var cfg = map fst (filter (\(label, node) ->  (var `elem` lVars node)) (M.toList cfg)) 


data LabeledBlock = LB { lb_phis :: [( Ident -- ^ var being redefined
                                     , [(Ident, Maybe Ident)])] -- ^ incoming block x last_defined block (it is maybe because it could be formal arg)
                       , lb_stmts :: [AST.CCompoundBlockItem N.NodeInfo] -- ^ a compound stmt
                       , lb_preds :: [NodeId]
                       , lb_succs :: [NodeId]
                       , lb_lvars :: [Ident]
                       , lb_rvars :: [Ident]
                       , lb_containers :: [Ident]
                       , lb_loop  :: Bool
                       }
                    deriving Show

-- ^ a SSA function declaration AST. We only care about the body of the function. We
-- apply translation on individual function.
data SSA = SSA { scoped_decls  :: [AST.CDeclaration N.NodeInfo]  -- ^ function wide global declaration (not yet renamed)
               , labelled_blocks :: M.Map Ident LabeledBlock     -- ^ translated labelled block (renamed)
               , sdom_ssa :: SDom                                -- ^ we need the sdom for SSA to CPS translation
               , ssa_local_vars :: S.Set Ident                   -- ^ function level local decl'ed vars
               , ssa_formal_args :: S.Set Ident                  -- ^ function formal args
               }
           deriving Show
                    
                    

-- combinators needed for CPS translation
-- ^ returns the adjacent nodes given a node
adjacent :: SSA -> NodeId -> [NodeId] 
adjacent ssa l = case M.lookup l (labelled_blocks ssa) of 
  { Nothing -> []
  ; Just n -> lb_succs n
  }
                 
                 
-- ^ check whether a non-trivial path exists from i to j                 
pathExists :: SSA -> NodeId -> NodeId -> Bool
pathExists ssa li lj = 
  lj `S.member` descendant ssa li
                 

-- ^ compute all the descendants of a node
descendant :: SSA -> NodeId -> S.Set NodeId
descendant ssa l = go ssa S.empty [l]
  where go :: SSA -> S.Set NodeId -> [NodeId] -> S.Set NodeId
        go ssa acc [] = acc
        go ssa acc (l:ls) = 
          let acc' = S.insert l acc
              adjs = adjacent ssa l 
          in if (all (\x -> x `S.member` acc') adjs) 
             then go ssa acc' ls
             else let new = filter (\x -> x `S.notMember` acc) adjs
                  in go ssa acc' (nub (ls ++ new))

-- ^ pre condition, a path exists
-- ^ compute the shortest path between nodes excluding the starting and ending nodes
-- ^ result is in reversed order        
-- ^ a typical shortest path via bfs         
path :: SSA -> NodeId -> NodeId -> Maybe [NodeId]
path ssa li lj = go ssa S.empty [[li]]
  where go :: SSA -> S.Set NodeId -> [[NodeId]] -> Maybe [NodeId]
        go ssa visited [] = Nothing
        go ssa visited ps = 
          let visited' = foldl (\v x -> S.insert x v) visited (map head (filter null ps))
              new_paths = concatMap (\p -> case p of 
                                        [] -> []
                                        (x:xs) -> 
                                          let nexts = filter (\y -> not (y `S.member` visited')) (adjacent ssa x)
                                          in [ (n:x:xs) | n <- nexts ] ) ps
              reached = filter (\p -> case p of 
                                   { (x:xs) | x == lj -> True
                                   ; _ -> False }) new_paths
          in if not (null reached) 
             then Just $ head reached
             else go ssa visited' new_paths
-- ^ return the last node before the destination if exists                  
lastNodeInPath :: SSA -> NodeId -> NodeId -> Maybe NodeId
lastNodeInPath ssa li lj = case path ssa li lj of             
  { Nothing -> Nothing
  ; Just (x:y:xs) -> Just y
  ; _ -> Nothing
  }

-- ^ remove an edge from SSA                    
removeEdge :: SSA -> (NodeId, NodeId) -> SSA
removeEdge ssa (li,lj) = 
  let lbs = labelled_blocks ssa
  in case M.lookup li lbs of 
    { Just n -> let succs = filter (\l -> not (l == lj)) (lb_succs n)
                    n' = n{lb_succs=succs}
                    lbs' = M.update (\_ -> Just n') li lbs
                in ssa{labelled_blocks=lbs'}  
    ; _ -> ssa }


                    
removeEdges :: SSA -> [(NodeId,NodeId)] -> SSA
removeEdges ssa edges = foldl removeEdge ssa edges


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

{-
prettyLB :: LabeledBlock -> String
prettyLB lb = "LB {" ++ "phis:" ++ (show (phis lb)) ++ ", stmts:" ++ (show (lb_stmts lb)) ++ "}"

prettySSA :: SSA -> String
prettySSA ssa = "scope_decls :" ++ (render $ pretty (scoped_decls ssa)) ++ "\n" ++
                "labelled_blocks :" ++ (concatMap (\(label, lb) ->  (render $ pretty label) ++ ":" ++ (prettyLB lb) ++ ",\n") (M.toList $ labelled_blocks ssa))
-}

-- TODO : incorporating the formal args as allVars

buildSSA :: CFG -> [Ident] -> SSA
buildSSA cfg fargs =
  case buildDTree cfg of
    { Nothing  -> error "failed to build dominance frontier table"
    ; Just (dtree, pcm, sdom) ->
      let dft = buildDF' (dtree, pcm, sdom, cfg)
          allVarsUsed = allVars cfg -- all vars used in this function, local declared, formal args and global 
          allLocalVars = S.fromList (localDeclaredVars cfg) -- all locally declared vars in this function
          -- build a mapping from node label to a
          -- set of variables that need to be merged via phi func
          -- (note: variables are not yet renamed)
          phiLocMap :: M.Map Ident -- node label
                       [Ident]     -- variable idents
          phiLocMap = foldl (\m (label,v) -> case M.lookup label m of
                                { Nothing -> M.insert label [v] m
                                ; Just vs -> M.update (\_ -> Just $ vs ++ [v]) label m
                                }) M.empty $ do
            { var <- allVarsUsed -- todo: shall we exclude global vars?
            ; let phiLocs = phiLoc var cfg dft
            ; phiLoc <- phiLocs
            ; return (phiLoc, var)
            }

          -- given a node's label (as the starting point of the search)
          -- a variable (which is not yet renamed), the dom tree
          -- to find the label of the node which contains preceding definition of the variable
          precDef :: Ident -> Ident -> DTree -> CFG -> Maybe Ident
          precDef currLbl var dt cfg =
            case M.lookup currLbl phiLocMap of
              { Just vs | var `elem` vs -> Just currLbl  -- the var is reassigned through a phi function in the current block
              ; _                       ->               -- the var could be reassigned locally
                   case M.lookup currLbl cfg of
                     { Nothing   -> error $ "Fatal: Label " ++ show currLbl ++ " is not found in the CFG"
                     ; Just node | var `elem` (lVars node) ->  Just currLbl
                     ; Just _ | otherwise ->
                       case parentOf currLbl dt of
                         { Nothing | (var `S.member` formalArguments) ||  
                                     (var `S.member` allLocalVars)
                                     -> -- no parent: this is block 0
                                       Just currLbl
                                       -- this could be due to a nested scope var in some inner block of if else or while.
                                       -- we will move it to block 0 (the init block)
                                       -- e.g. refer to the fibiter.c, the variable t
                                       -- this make sense for block 0, all local variables should be lifted and declared in block 0
                                   | otherwise -> Nothing -- as this is to be decided later, it could be a global var
                         ; Just parentLbl -> precDef parentLbl var dt cfg
                         }
                     }
              }

          formalArguments = S.fromList fargs

          -- renaming all the variables based on the individual labelled blocks
          -- and move all the declaration out.

          eachNode :: SSA -> (Ident, Node) -> SSA
          eachNode ssa (currLbl, node) = case node of
            { Node statements lvars rvars local_decls precLbls succLbls switchOrLoop ->
                 let phis_ :: [( Ident -- ^ var being redefined (not yet renamed)
                              , [(Ident, Maybe Ident)])] -- ^ (incoming block lbl, lbl of preceding blk in which var is redefined)
                     phis_ = case M.lookup currLbl phiLocMap of
                       { Nothing      -> []
                       ; Just phiVars -> map (\v ->
                                               let lastDefs =
                                                     map (\precLbl ->
                                                           let mb_lblVarDefined =  precDef precLbl v dtree cfg
                                                           in (precLbl, mb_lblVarDefined)) precLbls
                                               in (v, lastDefs)
                                                  -- find the preceding node and the renamed variable
                                             ) [ v |  v <- phiVars, not (v `elem` local_decls) ]
                       }
                     -- build the renaming state from the rhs vars with the precDef
                     -- or from phis_
                     rnState :: RenameState
                     rnState = let -- todo, the local_decls should be filtered away from the rVars and local_decls should be appended with the current node label
                                   rnEnvLocal = M.fromList $  map (\var -> (var, var `app` currLbl)) local_decls
                                   rvarsNotLocal = filter (\var -> not (var `M.member` rnEnvLocal)) rvars
                                   rnEnv = case precLbls of
                                     { [] -> -- entry block -- todo: check, shouldn't be the formal arg?
                                          M.fromList (map (\var ->
                                                            if (var `S.member` formalArguments) ||  
                                                               (var `S.member` allLocalVars)
                                                            then (var, var `app` currLbl)
                                                            else (var, var) -- global
                                                          ) rvarsNotLocal)
                                     ; [precLbl] -> -- non-phi block
                                            M.fromList (map (\var ->
                                                              case precDef precLbl var dtree cfg of
                                                                { Just def_lbl -> (var, var `app` def_lbl)
                                                                ; Nothing      -> -- it is a formal arg and should be redefined in block 0
                                                                     if (var `S.member` formalArguments) ||  
                                                                        (var `S.member` allLocalVars)
                                                                     then (var, var `app` (iid (labPref ++ "0")))
                                                                     else (var, var) -- global
                                                                }) rvarsNotLocal)
                                     ; precLbls -> -- phi block
                                          M.fromList (map (\var -> case lookup var phis_ of
                                                              { Just _ -> (var, var `app` currLbl)
                                                              ; Nothing ->
                                                                   -- not in phi, it's a formal arg  and should be redefined in block 0
                                                                   -- (var, var `app` (iid (labPref ++ "0")))

                                                                   -- updated: the above is not true, look at sort example, var j in node 7. (7 has preds 4 and 6, both do not redefine j,
                                                                   -- node 4 & 6 share a common ancestor 3 where j is redefined
                                                                   case filter isJust $ map (\precLbl -> precDef precLbl var dtree cfg) precLbls of
                                                                     { [] -> -- var is global
                                                                          (var, var)
                                                                     ; (def:defs) | all (\d -> d == def) defs ->
                                                                            let (Just def_lbl) = def
                                                                            in (var, var `app` def_lbl)
                                                                                  | otherwise -> error $ "buildSSA: rnState variable " ++ (show var) ++ " has a conflicting preceding definition " ++ (show (def:defs)) ++ "\n current Label " ++ (show currLbl) ++ "\n CFG" ++ (show cfg) ++ "\n Phi loca Map" ++ (show phiLocMap) ++ "\n DFT" ++ (show dft)
                                                                     }
                                                              }) rvarsNotLocal)
                                     }
                               in RSt currLbl (rnEnvLocal `M.union` rnEnv) [] [] allLocalVars formalArguments

                     renamedBlkItems_decls_containers :: ([AST.CCompoundBlockItem N.NodeInfo], [AST.CDeclaration N.NodeInfo], [Ident])
                     renamedBlkItems_decls_containers = renamePure rnState statements
                     (renamedBlkItems, new_decls, containers) = renamedBlkItems_decls_containers
                     -- scalar copy for container variables eg. say a[] then a[i] = a[j] --> a_2 = a_1; a_2[i_2] = a_1[j_2];
                     -- and insert them to the begining of the labeled block
                     scalar_copy :: [Ident] -> [AST.CCompoundBlockItem N.NodeInfo]
                     scalar_copy containers =
                       concatMap (\container -> -- lookup the last def of container variable, similar to building rnState
                                   let containers' = case precLbls of
                                         { [] -> [] -- entry block
                                         ; [precLbl] -> case precDef precLbl container dtree cfg of
                                              { Just def_lbl -> [container `app` def_lbl]
                                              ; Nothing      -> [container `app` (iid (labPref ++ "0"))]
                                              }
                                         ; precLbls -> case lookup container phis_ of  -- phi block
                                              { Just _  -> [] -- should be covered by the phi translations
                                              ; Nothing ->
                                                   case filter isJust $ map (\precLbl -> precDef precLbl container dtree cfg) precLbls of
                                                     { [] -> error "impossible"
                                                     ; (def:defs) | (all (\d -> d == def) defs) ->
                                                             let (Just def_lbl) = def
                                                             in [container `app` def_lbl]
                                                                  | otherwise -> error $ "buildSSA: scalar copy variable " ++ (show container) ++ " has a conflicting preceding definition"
                                                     }
                                              }
                                         }
                                   in map (\container' -> AST.CBlockStmt (AST.CExpr (Just ((cvar (container `app` currLbl)) .=. (cvar container'))) N.undefNode)) containers'
                                 ) containers
                     isLoop x = case x of { (IsLoop _ _) -> True ; _ -> False }
                     labelled_block = LB phis_ ((scalar_copy containers) ++ renamedBlkItems) precLbls succLbls (lVars node) (rVars node) containers (isLoop switchOrLoop)
                 in ssa{ labelled_blocks = M.insert currLbl labelled_block (labelled_blocks ssa)
                       , scoped_decls    = (scoped_decls ssa) ++ new_decls }
            }
          ssa = foldl eachNode (SSA [] M.empty sdom allLocalVars formalArguments) $ M.toList cfg
          io = unsafePerformIO $ print cfg >> print ssa  >> print dft >> print pcm >> print dtree -- (map (\var -> (var, modLoc var cfg)) allVarsUsed) >> print dtree >> print dft >> print pcm >> print sdom
          
      in ssa   -- the scoped_decls were not yet renamed which will be renamed in SSA to CPS convertion
    }








allVars :: CFG -> [Ident]
allVars cfg = S.toList (S.fromList (concatMap (\(i,n) -> lVars n ++ rVars n) (M.toList cfg)))


localDeclaredVars :: CFG -> [Ident]
localDeclaredVars cfg =  
  S.toList (S.fromList (concatMap (\(i,n) -> localDecls n) (M.toList cfg)))
