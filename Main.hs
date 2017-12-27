import System.Environment
import qualified Language.C.Syntax.AST as AST

import Language.C.Obfuscate.CFG 
import Language.C.Obfuscate.SSA
import Language.C.Obfuscate.CPS
import Language.C.Obfuscate.ASTUtils

import Language.C (parseCFile, parseCFilePre)
import Language.C.System.GCC (newGCC)
import Language.C.Pretty (pretty)
import Text.PrettyPrint.HughesPJ (render, text, (<+>), hsep)


isMain fundef = case getFunName fundef of 
  { Just "main" -> True
  ; _ -> False
  }


main :: IO ()
main = do 
  { let opts = []
  ; args <- getArgs
  ; let (src:dest:opts) = args
  ; ast <- errorOnLeftM "Parse Error" $ parseCFile (newGCC "gcc") Nothing opts src
  ; case ast of 
    { AST.CTranslUnit defs nodeInfo -> do 
         { writeFile dest ""
         ; mapM_ (\def -> case def of 
                     { AST.CFDefExt fundef | not (isMain fundef) && not (isInlineFun fundef) -> 
                          case runCFG fundef of
                            { CFGOk (_, state) -> do 
                                 { let ssa@(SSA scopedDecls labelledBlocks sdom) = buildSSA (cfg state) (formalArgs state)
                                       visitors = allVisitors sdom labelledBlocks
                                       exits    = allExits labelledBlocks
                                       cps = ssa2cps fundef ssa 
                                 ; appendFile dest (prettyCPS cps) 
                                 ; appendFile dest "\n"
                                 }
                            ; CFGError s       -> error s
                            }
                     ; other_def -> do 
                          { appendFile dest (render $ pretty other_def)
                          ; appendFile dest "\n"
                          }
                     }
                 ) defs
         }
    ; _ -> error "not fundec"
    }
  }
