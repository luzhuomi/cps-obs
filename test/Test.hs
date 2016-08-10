{-# LANGUAGE FlexibleContexts #-}
import System.Environment(getArgs)
import Control.Monad.IO.Class(liftIO, MonadIO)
import Clang.String(unpack)
import Clang.TranslationUnit(getDiagnosticSet, {-getSpelling,-} getCursor)
import Clang(parseSourceFile)
import Clang
import Clang.Cursor 
import qualified Clang.Diagnostic as Diagnostic(format, getElements)
import Control.Monad.Trans.Control(MonadBaseControl)
import Control.Monad.Catch(MonadThrow)
import qualified Data.Vector.Storable as DVS


test :: (MonadIO m, 
         MonadBaseControl IO m,
         MonadThrow m) => 
        Clang.TranslationUnit s' -> ClangT s'' m ()

test tu = 
  do getDiagnosticSet tu >>= Diagnostic.getElements >>= mapM_ printDiag
    where printDiag d = Diagnostic.format Nothing d >>=
                        unpack >>=
                        (liftIO . putStrLn . ("Diag:" ++))
{-
test2 tu = do 
  { clangString <- getSpelling tu
  ; string <- unpack clangString
  ; (liftIO $ putStrLn string)
  }
-}         
  

test3 tu = do 
  { cursor <- getCursor tu
  ; mains <- getMain cursor
  ; if null mains
    then return ()
    else do 
      { let main = head mains
      ; traverseCursor main
      }
  }
           
getMain cursor = do 
  { displayName <- getDisplayName cursor
  ; unpackedName <- unpack displayName
  ; if (unpackedName == "main()") 
    then return [cursor]
    else do 
      { cursors <- getChildren cursor
      ; rs <- mapM getMain (DVS.toList cursors)
      ; return (concat rs) 
      }
  }


traverseCursor cursor = do 
  { printCursorInfo cursor
  ; cursors <- getChildren cursor
  ; DVS.mapM_ traverseCursor cursors
  }
                   
        


printCursorInfo cursor = do 
  { displayName <- getDisplayName cursor 
  ; unpackedName <- unpack displayName
  ; spelling <- getSpelling cursor  
  ; unpackedSpelling <- unpack spelling
  ; usr <- getUSR cursor
  ; unpackedUSR <- unpack usr
  ; liftIO $ putStrLn ( (show (getKind cursor)) ++ "\t" ++ unpackedName ++ "\t" ++ unpackedSpelling ++ "\t" ++ unpackedUSR )
  }


main = do
  (arg:args) <- getArgs
  parseSourceFile arg args test3
  
{-  
./Test helloworld.cpp -I/usr/lib/llvm-3.8/lib/clang/3.8.0/include/ 
-}