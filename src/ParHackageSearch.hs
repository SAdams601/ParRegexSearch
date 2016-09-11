module ParHackageSearch where
import Search
import System.Directory
import System.FilePath.Posix
import Text.Regex.Posix
import Control.Parallel.Strategies
--import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Data.Char

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

searchFileRead :: B.ByteString -> Int -> [(FilePath, B.ByteString)] -> [(FilePath, [B.ByteString])]
searchFileRead _ _ [] = []
searchFileRead pat extraLines ((fp,conts):rst) = (fp,matches): (searchFileRead pat extraLines rst)
  where matches = formatMatches $ searchLines (lns conts)
        lns l = zip [1..] $ B.lines l
        searchLines :: [(Int,B.ByteString)] -> [(Int,B.ByteString)]
        searchLines [] = []
        searchLines (fst@(_, line):rst) =
          if line =~ pat
          then let (match, toSearch) = splitAt extraLines rst in
            (fst:(match ++ (searchLines toSearch)))
          else searchLines rst
        formatMatches :: [(Int, B.ByteString)] -> [B.ByteString]
        formatMatches [] = []
        formatMatches ((linNum, match):rst) = let s = B.pack $ (show linNum) ++ ": "
                                                  r = formatMatches rst
                                              in
          (B.append s (B.dropWhile isSpace match): r)         


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
