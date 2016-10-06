{-# LANGUAGE FlexibleContexts #-}
module FindTypes where
import Data.ByteString.Char8 as BS
import Text.Regex.Posix
import StateFileProcessing hiding (searchFile)
import Control.Monad.State
import Data.Map as Map
import Data.Maybe (fromJust)

data TypeSum = T{ typeName :: ByteString,
                  hasMonad :: Bool,
                  hasApp :: Bool,
                  monadLoc :: Maybe FilePath,
                  appLoc :: Maybe FilePath
                }

emptySum :: ByteString -> TypeSum
emptySum name = T name False False Nothing Nothing

instance Show TypeSum where
  show ts = let ln1 = "The type: " ++ unpack (typeName ts) ++ "\n"
                ln2 = if (hasMonad ts)
                      then "has a monad instance in " ++ (fromJust (monadLoc ts))
                      else "does not have a monad instance\n"
                ln3 = if (hasApp ts)
                      then "it's applicative instance can be found in " ++ (fromJust (appLoc ts))
                      else "it does not have an applicative instance.\n"
                lnEqs = "========================================\n" in
            lnEqs ++ ln1 ++ ln2 ++ ln3 ++ lnEqs
                
                           
instance Eq TypeSum where
  x == y = (typeName x) == (typeName y)

type SearchMap = Map ByteString TypeSum                       

data SearchRecord = S {mp :: SearchMap,
                       lns :: [ByteString],
                       fp :: FilePath}

type SearchState = State SearchRecord

isBothInstances :: TypeSum -> Bool
isBothInstances t = hasMonad t && hasApp t

--This could be a fold except then if two types have the same names in different projects the map would overwrite. I could solve this by having a filepath also be part of the key but I'd rather keep it simpler for now.

genMaps :: [(FilePath, ByteString)] -> [SearchMap]
genMaps files = Prelude.map f files
  where
    f (fp,bs) = searchFile fp Map.empty bs

--This searches and entire file
searchFile :: FilePath -> SearchMap -> ByteString -> SearchMap
searchFile fp map file = evalState searchComp (S map (BS.lines file) fp)
  where
    searchComp :: SearchState SearchMap
    searchComp = do
      state <- get
      case (lns state) of
        [] -> return $ mp state
        _ -> do
          let newRecord = findInstances state
          case newRecord of
            Nothing -> return ()
            (Just record) -> put record
          map2 <- searchComp
          return map2

findInstances :: SearchRecord -> Maybe SearchRecord
findInstances S {mp=m, lns=(l:ls), fp = fp} =
  let appMatch = isAppInstance l
      monadMatch = isMonadInstance l
  in
  
  if BS.null appMatch
  then if BS.null monadMatch
       then Nothing
       else
         let newMap = packMonad m monadMatch fp in
         Just (S newMap ls fp)
  else
    let newMap = packApp m monadMatch fp in
    Just (S newMap ls fp)
  where
    isAppInstance ln = ln =~ pack "instance Applicative\\s+(.+)\\swhere"
    isMonadInstance ln = ln =~ pack "instance Monad\\s+(.+)\\swhere"
    packMonad :: SearchMap -> ByteString -> FilePath -> SearchMap
    packMonad map ty fp = let oldTySum = Map.lookup ty map in
      case oldTySum of
        Nothing -> Map.insert ty (T ty True False (Just fp) Nothing) map
        Just tSum -> Map.insert ty (tSum {hasMonad = True, monadLoc = Just fp}) map
    packApp :: SearchMap -> ByteString -> FilePath -> SearchMap
    packApp map ty fp = let oldTySum = Map.lookup ty map in
      case oldTySum of
        Nothing -> Map.insert ty (T ty False True Nothing (Just fp)) map
        Just tSum -> Map.insert ty (tSum {hasApp = True, appLoc = Just fp}) map
