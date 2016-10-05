module FindTypes where
import Data.ByteString.Char8 as BS
import Text.Regex.Posix
import StateFileProcessing
import Control.Monad.State
import Data.Map as Map

data TypeSum = T{ typeName :: ByteString,
                  hasMonad :: Bool,
                  hasApplicative :: Bool,
                  monadLoc :: Maybe FilePath,
                  appLoc :: Maybe FilePath
                }

emptySum :: ByteString -> TypeSum
emptySum name = T name False False Nothing Nothing

                           
instance Eq TypeSum where
  x == y = (typeName x) == (typeName y)

data SearchMap = Map ByteString TypeSum                       

data SearchRecord = S {mp :: SearchMap,
                       lns :: [ByteString]}

type SearchState = State SearchRecord

isBothInstances :: TypeSum -> Bool
isBothInstances t = hasMonad t && hasApplicative t

-- Is this a fold?
-- Need to go through the list of files and thread the searchmap produced by searchComp in order
-- Or does map support something like unionWith?
-- Map the search then fold the list with unionWith which combines the maps

searchFiles :: [(FilePath, ByteString)] -> SearchMap
searchFiles files = undefined

--This searches and entire file
searchFile :: SearchMap -> ByteString -> SearchMap
searchFile map file = evalState searchComp (S map (BS.lines file))
  where
    searchComp :: SearchState SearchMap
    searchComp = do
      state <- get
      case (lns state) of
        [] -> return $ mp state
        _ -> do
          let newState = findInstances state
          put newState
          map2 <- searchComp
          return map2

findInstances :: SearchRecord -> SearchRecord
findInstances S {mp=m, lns=(l:ls)} = undefined
