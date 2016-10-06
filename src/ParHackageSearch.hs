module ParHackageSearch where
import Search
import System.Directory
import System.FilePath.Posix
import Text.Regex.Posix
import Control.Parallel.Strategies
import qualified Data.ByteString.Char8 as B
import Data.Char
import System.Mem
import Control.DeepSeq
import Control.Monad

packages = "/home/stephen/Projects/ParRegexSearch/hackage/package/"

readAllPackages :: FilePath -> IO [(FilePath,[(FilePath, B.ByteString)])]
readAllPackages fp = do
  dirs <- listDirectory fp
  let fullPaths = map (\pn -> fp ++ "/" ++ pn) dirs
  onlyDirs <- filterM doesDirectoryExist fullPaths
  res <- mapM readDir onlyDirs
  let fr = force res
  performGC
  return $ fr
    where
      searchPred fp = let ext = takeExtension fp in
        return $ ext == ".hs" || ext == ".lhs"
      readDir fp = do
        contents <- getDirContentsByPred fp searchPred
        return (fp, contents)


