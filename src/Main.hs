module Main where
import Control.Parallel.Strategies
import ParHackageSearch
import System.FilePath.Posix
import qualified Data.ByteString.Char8 as B
import System.Environment
  
main :: IO ()
main = do
  (fp:_) <- getArgs
  packageContents <- readAllPackages fp
  let res = runEval $ searchFiles packageContents
  showSearchResults res

type DirContents = (FilePath,[(FilePath, B.ByteString)])
type SearchRes = (FilePath,[(FilePath, [B.ByteString])])

regex = B.pack $ "instance Monad"

searchFiles :: [DirContents] -> Eval [SearchRes]
searchFiles fs = do
  res <- parFileSearch fs
  rseq res
  return res

parFileSearch :: [DirContents] -> Eval [SearchRes]
parFileSearch [] = return []
parFileSearch ((dir, files):rst) = do
  b <- rpar $ (regexSearchFile regex 0) files
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
              fStr:lines
            else showRes rst
