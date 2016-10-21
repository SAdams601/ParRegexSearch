{-#LANGUAGE ScopedTypeVariables, DeriveDataTypeable, DeriveGeneric #-}
module ExactPrintSearch where
import FindTypes hiding (searchFile, findInstances)
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Transform
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint
import Data.Map as Map
import Data.Foldable
import Data.Maybe
import qualified GHC as GHC
import qualified OccName as GHC
import qualified Data.Generics as SYB
import qualified GHC.SYB.Utils as SYB
import qualified RdrName as GHC
import System.IO.Unsafe
import Control.Exception

searchPackage :: [FilePath] -> IO DeclMap
searchPackage files = foldlM searchFile Map.empty files

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch
  
searchFile :: DeclMap -> FilePath -> IO DeclMap
searchFile map fp = do
  parse <- catchAny (parseModule fp) $ \e -> do
    appendFile "errors.txt" ("Fatal parse exception in " ++ fp)
    appendFile "errors.txt" ((show e) ++ "\n===================\n")
    return (Left (GHC.noSrcSpan , (show e)))
  case parse of
    Left (_,err) -> do
      putStrLn $ "Parse error occured in " ++ fp ++ "\n"
      putStrLn err
      return Map.empty
    Right (anns, ps) -> do
      let mQual = checkForQualifiedMonad ps
          aQual = checkForQualifiedApplicative ps
          allInstances = findInstances (aQual,mQual) ps
      return $ Map.union map (packMap anns allInstances)



type DeclMap = Map.Map GHC.OccName DeclSum

data DeclSum = D {name :: GHC.OccName
                , hasM :: Bool
                , hasA :: Bool
                , mDecl :: Maybe (Anns,(GHC.LHsDecl GHC.RdrName))
                , aDecl :: Maybe (Anns,(GHC.LHsDecl GHC.RdrName))}

instance Show DeclSum where
  show ds = let ln1 = "The type " ++ showData (name ds) ++ "\n" in
    ln1 ++ (appLine ds) ++ (monadLine ds)
            where monadLine ds =
                    if (hasM ds)
                    then
                      let (Just (anns,md)) = mDecl ds in
                      "Has a monad instance:\n" ++ exactPrint md anns ++ "\n"
                    else "Does not define a monad instance.\n"
                  appLine ds =
                    if (hasA ds)
                    then
                      let (Just (anns,ad)) = aDecl ds in
                      "Has an applicative instance:\n" ++ exactPrint ad anns ++ "\n"
                    else "Does not define a applicative instance.\n"

impBoth :: DeclSum -> Bool
impBoth ds = hasA ds && hasM ds

impAppOnly :: DeclSum -> Bool
impAppOnly ds = hasA ds && not (hasM ds)

impMonadOnly :: DeclSum -> Bool
impMonadOnly ds = not (hasA ds) && hasM ds

splitDeclMap :: DeclMap -> (DeclMap, DeclMap, DeclMap)
splitDeclMap dm = (Map.filter impBoth dm, Map.filter impAppOnly dm, Map.filter impMonadOnly dm)

showDeclMp :: (FilePath, DeclMap) -> IO ()
showDeclMp tpl = putStrLn $ declMpStr tpl

declMpStr :: (FilePath, DeclMap) -> String
declMpStr (fp,dm) = "==========================\nPackage: " ++ fp ++ "\n" ++ (show $ Map.elems dm)

showData :: SYB.Data a => a -> String
showData = SYB.showData SYB.Parser 3

outputDeclMap :: (FilePath, DeclMap) -> IO (Int, Int,Int)
outputDeclMap (pkg, mp) = do
  let (both, onlyA, onlyM) = splitDeclMap mp
      numBoth = Map.size both
      numA = Map.size onlyA
      numM = Map.size onlyM
  writePackageToFile (pkg,both) "hasBoth.txt"
  writePackageToFile (pkg, onlyA) "hasApp.txt"
  writePackageToFile (pkg, onlyM) "hasMon.txt"
  return (numBoth,numA, numM)

writePackageToFile :: (FilePath,DeclMap) -> FilePath -> IO ()
writePackageToFile pkg fp = if Map.null (snd pkg)
                            then return ()
                            else appendFile fp (declMpStr pkg)
  

packMap :: Anns -> [(GHC.OccName,InstFlag,GHC.LHsDecl GHC.RdrName)] -> DeclMap
packMap anns = comp Map.empty
  where comp mp [] = mp
        comp mp ((tyName, flg, decl): rst) = let newMap = insertIntoDeclMap tyName flg decl mp in
          comp newMap rst
        insertIntoDeclMap nm flg decl mp = let mSum = Map.lookup nm mp
                                               newSum = mkNewSum nm flg decl in
                                           case mSum of
                                             Nothing -> Map.insert nm newSum mp
                                             (Just sm) -> Map.adjust (updateSum flg decl) nm mp 
        mkNewSum nm flg decl = let as = getAllAnns anns decl in
          case flg of
            Mnad -> D nm True False (Just (as,decl)) Nothing
            App ->  D nm False True Nothing (Just (as,decl))
        updateSum flg decl sum = let as = getAllAnns anns decl in
          case flg of
            Mnad -> sum {hasM = True, mDecl = Just (as,decl)}
            App -> sum {hasA = True, aDecl = Just (as,decl)}

--This will get the occ name that applicative is known as in the module being processed
checkForQualifiedApplicative :: GHC.ParsedSource -> Maybe GHC.ModuleName
checkForQualifiedApplicative ps = SYB.something (Nothing `SYB.mkQ` comp) ps
  where comp :: GHC.ImportDecl GHC.RdrName -> Maybe GHC.ModuleName
        comp iDecl = let (GHC.L _ modName) = (GHC.ideclName iDecl) in
          if modName == appModName
          then (GHC.ideclAs iDecl)
          else Nothing

checkForQualifiedMonad :: GHC.ParsedSource -> Maybe GHC.ModuleName
checkForQualifiedMonad ps = SYB.something (Nothing `SYB.mkQ` comp) ps
  where comp :: GHC.ImportDecl GHC.RdrName -> Maybe GHC.ModuleName
        comp iDecl = let (GHC.L _ modName) = (GHC.ideclName iDecl) in
          if modName == mModName
          then (GHC.ideclAs iDecl)
          else Nothing

mModName = GHC.mkModuleName "Control.Monad"
appModName = GHC.mkModuleName "Control.Applicative"

data InstFlag = Mnad
              | App
              deriving (SYB.Data,Show,Eq)

--This goes through a module and pulls out all monad and applicative instance declarations.
--The first argument is a tuple with the ModuleName qualifiers for Control.Applicative and Control.Monad respectively
findInstances :: (Maybe GHC.ModuleName,Maybe GHC.ModuleName) -> GHC.ParsedSource -> [(GHC.OccName,InstFlag,GHC.LHsDecl GHC.RdrName)]
findInstances (aQual,mQual) ps = SYB.everything (++) ([] `SYB.mkQ` comp) ps
  where
    comp :: GHC.LHsDecl GHC.RdrName -> [(GHC.OccName,InstFlag,GHC.LHsDecl GHC.RdrName)]
    comp c@(GHC.L _ (GHC.InstD (GHC.ClsInstD cDecl))) = 
      let (GHC.L _ declTy) = GHC.cid_poly_ty cDecl
          isRelevant = releventType declTy in
      case isRelevant of
        Nothing -> []
        Just (oNm, flg) -> [(oNm,flg, c)]
    comp _ = []
    releventType :: GHC.HsType GHC.RdrName -> Maybe (GHC.OccName,InstFlag)
    releventType = SYB.something (Nothing `SYB.mkQ` releventType')
    releventType' :: GHC.HsType GHC.RdrName -> Maybe (GHC.OccName,InstFlag)
    releventType' (GHC.HsForAllTy _ _ _ _ ty) = SYB.something (Nothing `SYB.mkQ` releventType'') ty
    releventType' _ = Nothing
    releventType'' (GHC.HsAppTy (GHC.L _ (GHC.HsTyVar nm1)) (GHC.L _ (GHC.HsTyVar nm2)))
      | isMonTy nm1 = Just (GHC.rdrNameOcc nm2, Mnad)
      | isAppTy nm1 = Just (GHC.rdrNameOcc nm2, App)
      | otherwise = Nothing
    releventType'' _ = Nothing
    isMonTy :: GHC.RdrName -> Bool
    isMonTy (GHC.Unqual occNm) = GHC.occNameString occNm == "Monad"
    isMonTy (GHC.Qual mNm occNm) = (GHC.occNameString occNm == "Monad") -- && (mNm `compMaybe` mQual)
    isMonTy _ = False
    isAppTy :: GHC.RdrName -> Bool
    isAppTy (GHC.Unqual occNm) = GHC.occNameString occNm == "Applicative"
    isAppTy (GHC.Qual mNm occNm) = (GHC.occNameString occNm == "Applicative") -- && (mNm `compMaybe` mQual)
    isAppTy _ = False

--Compares in values inside of a maybe with an unwrapped value
compMaybe :: Eq a => a -> Maybe a -> Bool
compMaybe a (Just b) = a == b
compMaybe _ Nothing = False


--This is stolen from some of my own code:
--https://github.com/SAdams601/HaRe/blob/gen-applicative/src/Language/Haskell/Refact/Utils/ExactPrint.hs#L231f
getAllAnns :: (SYB.Data a) => Anns -> a -> Anns
getAllAnns anns = generic `SYB.ext2Q` located
  where generic :: SYB.Data a => a -> Anns
        generic a = Prelude.foldr Map.union Map.empty (SYB.gmapQ (getAllAnns anns) a) 
        located :: (SYB.Data b, SYB.Data loc) => GHC.GenLocated loc b -> Anns
        located a = case (located' a) of
          Nothing -> Map.empty
          Just as -> as
          where located' :: (SYB.Data b, SYB.Data loc) => GHC.GenLocated loc b -> Maybe Anns
                located' a@(GHC.L ss b) = do
                  s <- (SYB.cast ss) :: (Maybe GHC.SrcSpan)
                  let k = mkAnnKey (GHC.L s b)
                  v <- Map.lookup k anns
                  let rst = getAllAnns anns b
                  return $ Map.singleton k v `Map.union` rst

{-
Once we get the parsedSource there are a few pieces we'll need to check

First go through the import decls and figure out if Control.Applicative is being qualified as something if it is then inside of the ClsInstDecl there is a HsAppTy where the first type will be Applicative and the second type is the type implementing Applicative.

If applicative is qualified imported like so: "import qualified Control.Applicative as App"
Then the import decl looks like this:

(L {../ParRegexSearch/test/test-package/test1.hs:4:1-43} 
      (ImportDecl 
       (Nothing) 
       (L {../ParRegexSearch/test/test-package/test1.hs:4:18-36} {ModuleName: Control.Applicative}) 
       (Nothing) 
       (False) 
       (False) 
       (True) 
       (False) 
       (Just {ModuleName: App}) 
       (Nothing)))

and the first LHsType looks like this
(HsTyVar (Qual {ModuleName: App} {OccName: Applicative}))

otherwise the LHsType looks like this:
(HsTyVar (Unqual {OccName: Applicative}))

The format of the ClsInstDecl is:

ClsInstDecl	
   cid_poly_ty :: LHsType
   namecid_binds :: LHsBinds
   namecid_sigs :: [LSig name]
   cid_tyfam_insts :: [LTyFamInstDecl name]
   cid_datafam_insts :: [LDataFamInstDecl name]
   cid_overlap_mode :: Maybe (Located OverlapMode)
-}
