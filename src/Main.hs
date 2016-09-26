module Main where
import Control.Parallel.Strategies
import ParHackageSearch
import System.FilePath.Posix
import qualified Data.ByteString.Char8 as B
import System.Environment
import StateFileProcessing
  
main :: IO ()
main = do
  (fp:_) <- getArgs
  packageContents <- readAllPackages fp
  let res = parFileSearch2 packageContents
  showSearchResults res

type DirContents = (FilePath,[(FilePath, B.ByteString)])
type SearchRes = (FilePath,[(FilePath, [Match])])

regex = B.pack $ "instance Monad"


parFileSearch2 :: [DirContents] -> [SearchRes]
parFileSearch2 = parMap rseq fun
  where fun (dir, files) = let res = searchListOfFiles appInstancePred files in
          (dir, res)
  

searchFiles :: [DirContents] -> Eval [SearchRes]
searchFiles fs = do
  res <- parFileSearch fs
--  rseq res
  return res

parFileSearch :: [DirContents] -> Eval [SearchRes]
parFileSearch [] = return []
parFileSearch ((dir, files):rst) = do
  b <- rpar $ searchListOfFiles appInstancePred files
  bs <- parFileSearch rst
  return ((dir,b):bs)

showSearchResults :: [SearchRes] -> IO ()
showSearchResults [] = return ()
showSearchResults ((dir,searchReses):rst) = do
  let matches = showRes searchReses
  if not (null matches)
    then do
    putStrLn $ "===== Search results found in: " ++ dir
    mapM_ B.putStrLn matches
    else (return ())
  showSearchResults rst
    where showRes [] = []
          showRes ((file, lines):rst) =
            if not (null lines)
            then let fStr = B.pack $ "Match found in: " ++ takeFileName file in
              fStr:(showMatches 1 lines)
            else showRes rst
          showMatches _ [] = []
          showMatches n (m:mts) = let s = B.pack $ "MATCH " ++ (show n) ++ ":"
                                      rst = showMatches (n+1) mts in
            (s:m) ++ rst
                                            
