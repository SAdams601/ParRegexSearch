{-# LANGUAGE FlexibleContexts #-}
module FindTypes where
import Data.ByteString.Char8 as BS
import Text.Regex.PCRE
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

filterHasBoth :: [(FilePath, [SearchMap])] -> [(FilePath, [SearchMap])]
filterHasBoth = filterSearchRes (\t -> hasApp t && hasMonad t)

filterOnlyMonad :: [(FilePath, [SearchMap])] -> [(FilePath, [SearchMap])]
filterOnlyMonad = filterSearchRes (\t -> (not $ hasApp t) && hasMonad t)

filterOnlyApp :: [(FilePath, [SearchMap])] -> [(FilePath, [SearchMap])]
filterOnlyApp = filterSearchRes (\t -> hasApp t && (not $ hasMonad t))

filterSearchRes :: (TypeSum -> Bool) -> [(FilePath, [SearchMap])] -> [(FilePath, [SearchMap])]
filterSearchRes pred lst =
  let
    fLst :: [SearchMap] -> [SearchMap]
    fLst l = Prelude.map (Map.filter pred) l
    filteredLst = Prelude.map (\(fp, l) -> (fp, fLst l)) lst in
  Prelude.filter (\(fp, l) -> Prelude.not (Prelude.null l)) filteredLst

countInstances :: [(FilePath, [SearchMap])] -> Int
countInstances lst = sum (Prelude.map sum (Prelude.map (\(_, mps) -> Prelude.map Map.size mps) lst))

instance Show TypeSum where
  show ts = let ln1 = "The type: " ++ unpack (typeName ts) ++ "\n"
                ln2 = if (hasMonad ts)
                      then "has a monad instance in " ++ (fromJust (monadLoc ts)) ++ "\n"
                      else "does not have a monad instance\n"
                ln3 = if (hasApp ts)
                      then "it's applicative instance can be found in " ++ (fromJust (appLoc ts)) ++ "\n"
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
          put newRecord
          map2 <- searchComp
          return map2

findInstances :: SearchRecord -> SearchRecord
findInstances S {mp=m, lns=(l:ls), fp = fp} =
  let appMatch = isAppInstance l
      monadMatch = isMonadInstance l
  in
  if BS.null appMatch
  then if BS.null monadMatch
       then (S m ls fp)
       else
         let newMap = packMonad m monadMatch fp in
         (S newMap ls fp)
  else
    let newMap = packApp m appMatch fp in
    (S newMap ls fp)
  where
    isAppInstance ln =
      let res1 = ln =~ pack "instance Applicative\\s+(.+)\\s+where"
          res2 = ln =~ pack "instance[a-zA-Z\\s]+\\s*=>\\s*Applicative\\s+(.+)\\s+where"
          res3 = ln =~ pack "instance\\s+Applicative\\s+[a-z]\\s+=>\\s+(.+)\\s+where"
      in
      case res2 of
        [[_,match]] -> match
        _ ->
          case (res1,res3) of
            ([[_,match]],False) -> match
            _ -> BS.empty
    isMonadInstance ln =
      let res1 = ln =~ pack "instance Monad\\s+(.+)\\s+where"
          res2 = ln =~ pack "instance[a-zA-Z\\s]+\\s*=>\\s*Monad\\s+(.+)\\s+where"
          res3 = ln =~ pack "instance\\s+Monad\\s+[a-z]\\s+=>\\s+(.+)\\s+where"
      in
      case res2 of
        [[_,match]] -> match
        _ ->
          case (res1,res3) of
            ([[_,match]],False) -> match
            _ -> BS.empty
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
