import System.Environment
import qualified Language.C.Syntax.AST as AST

import Language.C.Obfuscate.CFG 
import Language.C.Obfuscate.SSA
import Language.C.Obfuscate.CPS

import Language.C (parseCFile, parseCFilePre)
import Language.C.System.GCC (newGCC)
import Language.C.Pretty (pretty)
import Text.PrettyPrint.HughesPJ (render, text, (<+>), hsep)

main :: IO ()
main = do 
  { let opts = []
  ; args <- getArgs
  ; let (src:dest:_) = args
  ; ast <- errorOnLeftM "Parse Error" $ parseCFile (newGCC "gcc") Nothing opts src
  ; case ast of 
    { AST.CTranslUnit (AST.CFDefExt fundef:_) nodeInfo -> 
         case runCFG fundef of
           { CFGOk (_, state) -> do 
                { let (SSA scopedDecls labelledBlocks sdom) = buildSSA (cfg state)
                      visitors = allVisitors sdom labelledBlocks
                      exits    = allExits labelledBlocks
                      cps = ssa2cps fundef (buildSSA (cfg state))
                ; writeFile dest (prettyCPS cps) 
                }
           ; CFGError s       -> error s
           }    
    ; _ -> error "not fundec"
    }
  }