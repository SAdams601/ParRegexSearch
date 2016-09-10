module ParHackageSearch where
import Search
import System.Directory
import System.FilePath.Posix
import Control.Parallel.Strategies
import qualified Data.ByteString as B

packages = "/home/stephen/Projects/ParRegexSearch/hackage/package/"

readAllPackages :: IO [(FilePath,[(FilePath, B.ByteString)])]
readAllPackages = do
  dirs <- listDirectory packages
  let fullPaths = map (\pn -> packages ++ pn) dirs
      pat = "instance Applicative"
  unsafeInterleaveMapIO readDir fullPaths
    where
      searchPred fp = let ext = takeExtension fp in
        return $ ext == ".hs" || ext == ".lhs"
      readDir fp = do
        contents <- getDirContentsByPred fp searchPred
        return (fp, contents)


searchPackages :: IO ()
searchPackages = do
  dirs <- listDirectory packages
  let fullPaths = map (\pn -> packages ++ pn) dirs
      pat = "instance Applicative"
      searchPred = (\fp -> let ext = takeExtension fp in
                       return $ ext == ".hs" || ext == ".lhs")
  res <- mapM (\fp -> searchDirWPred fp pat 3 searchPred) fullPaths
  return ()

{-
parSearchDir :: FilePath -> String -> Eval [FileMatches]
parSearchDir dir pat = do
  let searchPred = (\fp -> let ext = takeExtension fp in
                       return $ ext == ".hs" || ext == ".lhs")
  lst <- rpar $ searchDirWPred dir pat 3 searchPred
  lift lst
-}
