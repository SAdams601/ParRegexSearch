module ExactPrintSearch where
import FindTypes hiding (searchFile)
import Language.Haskell.GHC.ExactPrint.Parsers
import Data.Map as Map
import Data.Foldable

searchPackage :: [FilePath] -> IO SearchMap
searchPackage files = foldlM searchFile Map.empty files
  
searchFile :: SearchMap -> FilePath -> IO SearchMap
searchFile map fp = do
  parse <- parseModule fp
  case parse of
    Left (_,err) -> do
      putStrLn "Parse error occured: "
      putStrLn err
      return Map.empty
    Right (_, parsedSource) -> do
      return Map.empty
