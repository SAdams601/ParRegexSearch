{-# LANGUAGE BangPatterns #-}
module Search where
import Text.Regex.Posix
import Data.Char
import System.Posix.Files.ByteString
import System.Directory
import Data.ByteString.Char8 (pack)
import System.FilePath.Windows
import qualified Data.ByteString as B
import System.IO.Unsafe
import Control.Exception

type FileMatches = (FilePath ,[String])

searchDirectory :: FilePath -> String -> Int -> IO [FileMatches]
searchDirectory dir pat extraLines = recurseThroughDirTree dir (searchFile pat extraLines) 
  
searchDirWPred :: FilePath -> String -> Int -> (FilePath -> IO Bool) -> IO [FileMatches]
searchDirWPred dir pat extraLines pred = recWithPred dir pred (searchFile pat extraLines) 

searchFile :: String -> Int -> FilePath -> IO FileMatches
searchFile pat extraLines fp = do
  contents <- readFile fp
  let lns = zip [1..] $ lines contents
      searchRes = searchLines lns
      format = formatMatches searchRes
  return $ (fp,format)
    where searchLines :: [(Int, String)] -> [(Int,String)]
          searchLines [] = []
          searchLines (fst@(_, line):rst) =
            if line =~ pat
            then let (match, toSearch) = splitAt extraLines rst in
              (fst:(match ++ (searchLines toSearch)))
            else searchLines rst
          formatMatches :: [(Int, String)] -> [String]
          formatMatches [] = []
          formatMatches ((linNum, match):rst) =
            ((show linNum) ++ ": " ++ (dropWhile isSpace match)): formatMatches rst

recurseThroughDirTree :: FilePath -> (FilePath -> IO a) -> IO [a]
recurseThroughDirTree dir fun = recWithPred dir (\_ -> return True) fun
          

recWithPred :: FilePath -> (FilePath -> IO Bool) -> (FilePath -> IO a) -> IO [a]
recWithPred dir pred fun = do
  abs <- makeAbsolute dir
  dirContents <- listDirectory dir
  let files = map (fixFP abs) dirContents
  res <- unsafeInterleaveMapIO (r abs) files
  return $ foldl (++) [] res
    where
      fixFP dir fp = dir ++ "/" ++ fp 
      r dir fp = do
        status <- getFileStatus $ pack fp
        if isDirectory status
          then recWithPred fp pred fun
          else do
          b <- pred fp
          if b
            then do
            res <- fun fp
            return [res]
            else return []

unsafeInterleaveMapIO f (x:xs) = unsafeInterleaveIO $ do
  y <- f x
  ys <- unsafeInterleaveMapIO f xs
  return (y : ys)
unsafeInterleaveMapIO _ [] = return []

getDirContentsByPred :: FilePath -> (FilePath -> IO Bool) -> IO [(FilePath, B.ByteString)]
getDirContentsByPred dir pred = do
  abs <- makeAbsolute dir
  dirContents <- listDirectory dir
  let files = map (fixFP abs) dirContents
  res <- unsafeInterleaveMapIO (r abs) files
  return $ foldl (++) [] res
        where
          fixFP dir fp = dir ++ "/" ++ fp 
          r dir fp = do
            status <- getFileStatus $ pack fp
            if isDirectory status
              then getDirContentsByPred fp pred
              else do
              b <- pred fp
              if b
                then do
                contents <- B.readFile fp
                return [(fp,contents)]
                else return []
