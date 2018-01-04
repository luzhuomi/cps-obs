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


import Data.Text (Text, unpack, pack)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Data.ByteString (ByteString, readFile)
import Control.Applicative
import Prelude hiding (readFile) -- Ensure Applicative is in scope and we have no warnings, before/after AMP.
import qualified Data.Set as S

isMain fundef = case getFunName fundef of 
  { Just "main" -> True
  ; _ -> False
  }


data Config =
  Config {
    gcc :: Text
  , whitelist :: [Text]
  , blacklist :: [Text]
  } deriving (Eq, Show)


instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .:   (pack "gcc")       <*>
    v .:   (pack "whitelist") <*>
    v .:   (pack "blacklist")
  parseJSON _ = fail "Expected Object for Config value"

-- gcc = "gcc" -- for linux
-- gcc = "/usr/local/bin/gcc-7" -- for mac with homebrew

main :: IO ()
main = do 
  { let opts = []
  ; args <- getArgs
  ; let (src:dest:opts) = args
  ; configFile <- readFile "./config.yaml"
  ; let mb_config :: Maybe Config 
        mb_config = Y.decode configFile
  ; case mb_config of 
    { Nothing -> error "Error: Invalid config.yaml."
    ; Just config -> do 
      { ast <- errorOnLeftM "Parse Error" $ parseCFile (newGCC (unpack $ gcc config)) Nothing opts src
      ; case ast of 
        { AST.CTranslUnit defs nodeInfo -> do 
             { writeFile dest ""
             ; let wl = S.fromList (map unpack (whitelist config))
                   isWhiteListed fundef 
                     | S.null wl = True  -- if the white list is empty, everything is in the white list
                     | otherwise = case getFunName fundef of 
                       { Nothing -> False
                       ; Just n  -> n `S.member` wl}
             ; mapM_ (\def -> case def of 
                         { AST.CFDefExt fundef | not (isMain fundef) && 
                                                 not (isInlineFun fundef) && 
                                                 isWhiteListed fundef -> 
                              case runCFG fundef of
                                { CFGOk (_, state) -> do 
                                     { let ssa = buildSSA (cfg state) (formalArgs state)
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
        ; _ -> error "Parse Failed" 
        }
      }
    }
  }
