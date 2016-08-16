import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import Text.PrettyPrint.HughesPJ (render, text, (<+>), hsep)

import Language.C (parseCFile)
import Language.C.System.GCC (newGCC)
import Language.C.Pretty (pretty)

usageMsg :: String -> String
usageMsg prg = render $ text "Usage:" <+> text prg <+> hsep (map text ["CPP_OPTIONS","input_file.c"])

main :: IO ()
main = do
  let usageErr = (hPutStrLn stderr (usageMsg "./ParseAndPrint") >> exitWith (ExitFailure 1))
  args <- getArgs
  when (length args < 1) usageErr
  let (opts,input_file) = (init args, last args)
  ast <- errorOnLeftM "Parse Error" $ parseCFile (newGCC "gcc") Nothing opts input_file
  putStrLn (show $ pretty ast)

errorOnLeft :: (Show a) => String -> (Either a b) -> IO b
errorOnLeft msg = either (error . ((msg ++ ": ")++).show) return

errorOnLeftM :: (Show a) => String -> IO (Either a b) -> IO b
errorOnLeftM msg action = action >>= errorOnLeft msg
