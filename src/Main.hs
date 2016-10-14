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
import qualified Data.Map as Map
import Control.DeepSeq
import System.IO
import ExactPrintSearch

main :: IO ()
main = do
  (fp:_) <- getArgs
  t0 <- getCurrentTime
  res <- exactPrintSearch fp
  mapM_ showDeclMp res
{-  packageContents <- readAllPackages fp
  putStrLn "Done reading packages"
  t1 <- getCurrentTime
  res <- evaluate $ getTypeSums packageContents
  putStrLn "Gotten sums attempting to output"
  t2 <- getCurrentTime
  showTypeSums res
  outputResults res
  t3 <- getCurrentTime
  --Line taken from: https://github.com/simonmar/parconc-examples/blob/master/kmeans/kmeans.hs#L70
  printf "Total time on IO: %.2f\n" (realToFrac (diffUTCTime t1 t0) :: Double)
  printf "Total time on search: %.2f\n" (realToFrac (diffUTCTime t2 t1) :: Double)
  printf "Total time on print: %.2f\n" (realToFrac (diffUTCTime t3 t2) :: Double)
-}

type DirContents = (FilePath,[(FilePath, B.ByteString)])
type SearchRes = (FilePath,[(FilePath, [Match])])

showTypeSums :: [(FilePath, [SearchMap])] -> IO ()
showTypeSums res = mapM_ showRes res

showResWHandle :: Handle -> (FilePath, [SearchMap]) -> IO ()
showResWHandle h (pName, maps) = do
  let sumNum = length allTySums
      allTySums = (foldr (++) [] (map Map.elems maps))
  if sumNum == 0
    then return ()
    else do
       hPutStrLn h $ "Results from the package: " ++ pName
       hPutStrLn h $ (show $ length maps) ++ " files were searched."
       hPutStrLn h $ (show sumNum) ++ " summaries were found."
       mapM_ (hPrint h) allTySums

showRes :: (FilePath, [SearchMap]) -> IO ()
showRes = showResWHandle stdout

writeRes :: FilePath -> (FilePath, [SearchMap]) -> IO ()
writeRes fp res = do
  handle <- openFile fp AppendMode
  showResWHandle handle res
  hClose handle

outputResults :: [(FilePath, [SearchMap])] -> IO ()
outputResults res = do
  let bothInstances = filterHasBoth res
      onlyM = filterOnlyMonad res
      onlyA = filterOnlyApp res
      bp = "hasBoth.txt"
      mp = "hasMonad.txt"
      ap = "hasApp.txt"
  mapM_ (writeRes bp) bothInstances
  mapM_ (writeRes mp) onlyM
  mapM_ (writeRes ap) onlyA
  putStrLn $ (show (countInstances bothInstances)) ++ " types were found which defined both instances."
  putStrLn $ (show (countInstances onlyM)) ++ " types were found which only defined the monadic instance."
  putStrLn $ (show (countInstances onlyA)) ++ " types were found which only defined the applicative instance."

showSearchResStats :: [SearchRes] -> IO ()
showSearchResStats srs = do
  let (fc, mc) = foldl (\(a1,b1) (a2,b2) -> (a1+a2, b1+b2)) (0,0) (map fun srs)
  putStrLn $ "A total of " ++ (show mc) ++ " matches were found in " ++ (show fc) ++ " files."
  where    
    fun (dirP, lst) = let mCount = foldl (\n (_, l) -> n + length l) 0 lst in
          (length lst, mCount)

regex = B.pack $ "instance Monad"

exactPrintSearch :: FilePath -> IO [(FilePath,DeclMap)]
exactPrintSearch fp = do
  files <- getAllFileNames fp
--  print files
  maps <- mapM (\(pName, fs) -> do{mp <- searchPackage fs; return (pName, mp)}) files
  return maps

findAppInstances :: [DirContents] -> [SearchRes]
findAppInstances = parMap rseq fun
  where fun (dir, files) = let res = searchListOfFiles appInstancePred files in
          (dir, res)

findMonadInstances :: [DirContents] -> [SearchRes]
findMonadInstances = parMap rseq fun
  where fun (dir, files) = let res = searchListOfFiles monadInstancePred files in
          (dir, res)

getTypeSums :: [DirContents] -> [(FilePath,[SearchMap])]
getTypeSums = parMap rseq fun
  where fun (dir, files) = let maps = genMaps files in
          (dir, maps)

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
                                            
