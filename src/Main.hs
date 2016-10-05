module Main where
import Control.Parallel.Strategies
import ParHackageSearch
import System.FilePath.Posix
import qualified Data.ByteString.Char8 as B
import System.Environment
import StateFileProcessing
import Control.Exception.Base
import Data.Time.Clock
import Text.Printf
import FindTypes

main :: IO ()
main = do
  (fp:_) <- getArgs
  t0 <- getCurrentTime
  packageContents <- readAllPackages fp
  t1 <- getCurrentTime
  res <- evaluate $ findMonadInstances packageContents
  t2 <- getCurrentTime
  showSearchResults res
  showSearchResStats res
  t3 <- getCurrentTime
  --Line taken from: https://github.com/simonmar/parconc-examples/blob/master/kmeans/kmeans.hs#L70
  printf "Total time on IO: %.2f\n" (realToFrac (diffUTCTime t1 t0) :: Double)
  printf "Total time on search: %.2f\n" (realToFrac (diffUTCTime t2 t1) :: Double)
  printf "Total time on print: %.2f\n" (realToFrac (diffUTCTime t3 t2) :: Double)
  

type DirContents = (FilePath,[(FilePath, B.ByteString)])
type SearchRes = (FilePath,[(FilePath, [Match])])

showSearchResStats :: [SearchRes] -> IO ()
showSearchResStats srs = do
  let (fc, mc) = foldl (\(a1,b1) (a2,b2) -> (a1+a2, b1+b2)) (0,0) (map fun srs)
  putStrLn $ "A total of " ++ (show mc) ++ " matches were found in " ++ (show fc) ++ " files."
  where    
    fun (dirP, lst) = let mCount = foldl (\n (_, l) -> n + length l) 0 lst in
          (length lst, mCount)

regex = B.pack $ "instance Monad"

findAppInstances :: [DirContents] -> [SearchRes]
findAppInstances = parMap rseq fun
  where fun (dir, files) = let res = searchListOfFiles appInstancePred files in
          (dir, res)

findMonadInstances :: [DirContents] -> [SearchRes]
findMonadInstances = parMap rseq fun
  where fun (dir, files) = let res = searchListOfFiles monadInstancePred files in
          (dir, res)

appPureIsAp :: [DirContents] -> [SearchRes]
appPureIsAp = parMap rseq fun
  where fun (dir, files) = let res = searchListOfFiles pureIsAp files in
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
                                            
