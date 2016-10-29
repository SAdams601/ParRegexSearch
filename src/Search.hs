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
import Data.String.Utils

type FileMatches = (FilePath ,[String])

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

     

getFileNamesByPred :: FilePath -> (FilePath -> IO Bool) -> IO [FilePath]
getFileNamesByPred dir pred =
  if endswith "dist" dir
  then return []
  else do
    abs <- makeAbsolute dir
    dirContents <- listDirectory dir
    let files = map (fixFP abs) dirContents
    res <- mapM (r abs) files
    return $ foldl (++) [] res
      where
        fixFP dir fp = dir ++ "/" ++ fp
        r dir fp = do
          status <- getFileStatus $ pack fp
          if isDirectory status
            then  getFileNamesByPred fp pred
            else do
            b <- pred fp
            if b
              then return [fp]
              else return []
            
        


listDirectory :: FilePath -> IO [FilePath]
listDirectory fp = do
  contents <- getDirectoryContents fp
  return $ filter (\fp -> not (fp == ".." || fp == ".")) contents
