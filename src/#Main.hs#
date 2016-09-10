module Main where
import Control.Parallel.Strategies
import ParHackageSearch

main :: IO ()
main = do
  packageContents <- readAllPackages
  print $ length packageContents
  return ()

