{-#LANGUAGE ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module CountParseErrors where
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Transform
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint
import Control.Parallel.Strategies
import Control.Exception
import ParHackageSearch
import qualified GHC as GHC


countErrors :: FilePath -> IO ()
countErrors fp = do
  packages <- getAllFileNames fp
  counts <- sequence (parMap rseq (\(pName, fs) -> do {putStrLn $ "Searching: " ++ pName;
                                                       countParseErrors fs}) packages)
  let (totalFail, totalCount) = foldl (\(x,y) (m,n)-> (x+m,y+n)) (0,0) counts
  putStrLn $ (show totalCount) ++ " files have been processed in total."
  putStrLn $ (show totalFail) ++ " files failed to parse."

countParseErrors :: [FilePath] -> IO (Int,Int)
countParseErrors files = do
  res <- mapM attemptParse files
  let totalFiles = length files
      failures = foldl (+) 0 res
  return (failures, totalFiles)

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

attemptParse :: FilePath -> IO Int
attemptParse fp = do
  parse <- catchAny (parseModule fp) $ \e -> do
    return (Left (GHC.noSrcSpan , (show e)))
  case parse of
    Left (_,err) -> do
      appendFile "errors.txt" ("Fatal parse exception in " ++ fp)
      appendFile "errors.txt" (err ++ "\n===================\n")
      putStrLn fp
      appendFile "failedParses.txt" fp
      return 1
    Right _ -> do
      return 0


