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

readAllPackages :: FilePath -> IO [(FilePath,[(FilePath, B.ByteString)])]
readAllPackages fp = do
  dirs <- listDirectory fp
  let fullPaths = map (\pn -> fp ++ "/" ++ pn) dirs
  unsafeInterleaveMapIO readDir fullPaths
    where
      searchPred fp = let ext = takeExtension fp in
        return $ ext == ".hs" || ext == ".lhs"
      readDir fp = do
        contents <- getDirContentsByPred fp searchPred
        return (fp, contents)

searchFile :: (B.ByteString -> Bool) -> Int -> [(FilePath, B.ByteString)] -> [(FilePath, [B.ByteString])]
searchFile _ _ [] = []
searchFile fun extraLines ((fp,conts):rst) = (fp,matches): (searchFile fun extraLines rst)
  where matches = formatMatches $ searchLines (lns conts)
        lns l = zip [1..] $ B.lines l
        searchLines :: [(Int,B.ByteString)] -> [(Int,B.ByteString)]
        searchLines [] = []
        searchLines (fst@(_, line):rst) =
          if fun line
          then let (match, toSearch) = splitAt extraLines rst in
            (fst:(match ++ (searchLines toSearch)))
          else searchLines rst
        formatMatches :: [(Int, B.ByteString)] -> [B.ByteString]
        formatMatches [] = []
        formatMatches ((linNum, match):rst) = let s = B.pack $ (show linNum) ++ ": "
                                                  r = formatMatches rst
                                              in
          (B.append s (B.dropWhile isSpace match): r)         


regexSearchFile :: B.ByteString -> Int -> [(FilePath, B.ByteString)] -> [(FilePath, [B.ByteString])]
regexSearchFile _ _ [] = []
regexSearchFile pat extraLines lst = searchFile (\ln -> ln =~ pat) extraLines lst
