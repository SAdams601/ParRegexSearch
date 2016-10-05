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



isBothInstances :: TypeSum -> Bool
isBothInstances t = hasMonad t && hasApplicative t

--searchFiles :: [(FilePath, ByteString)] ->
--searchFiles

searchComp :: SearchMap -> FileState SearchMap
searchComp map = do
  rest <- get
  case rest of
    [] -> return empty
    lns -> do
      let (tss,rst) = findInstances lns map
      put rst
      map2 <- searchComp map
      return $ merge tss map2

findInstances :: [ByteString] -> SearchMap -> (SearchMap, [ByteString])
findInstances (ln:lns) map = undefined
