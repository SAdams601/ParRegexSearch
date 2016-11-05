module SearchOutputFile where
import Text.ParserCombinators.Parsec
import ExactPrintSearch
import Text.Regex.Posix
import Data.String.Utils
import Control.Monad

data InstanceSum = IS { tyStr :: String
                      , declStr :: Maybe String
                      , flg :: InstFlag
                      }
                 deriving (Show)

processFile :: IO ()
processFile = do
  fileContents <- readFile "output.txt"
  let lns = lines fileContents
      flgs = map processOutputFileLn lns
      (aCount, mCount) = foldl (\(aC, mC) mFlg -> case mFlg of
                                          Nothing -> (aC,mC)
                                          Just App -> (aC + 1,mC)
                                          Just Mnad -> (aC, mC + 1)) (0,0) flgs
  putStrLn $ (show aCount) ++ " applicative instances were found."
  putStrLn $ (show mCount) ++ " monad instances were found."

findApplyIsAp :: IO ()
findApplyIsAp = do
  fileContents <- readFile "output.txt"
  let lns = lines fileContents
      apps = processApp lns
  filtered <- filterApps apps
  print $ length filtered
  return ()
  --writeFile "appInstances.txt" "test"

filterApps :: [InstanceSum] -> IO [(InstanceSum, (Either ParseError Bool))]
filterApps apps = let parseRes = map parseSum $ apps in
  filterM handleApp $ zip apps parseRes
  where
    parseSum sum = let (Just decl) = (declStr sum) in
      parse isApplyAp (tyStr sum) decl
    handleApp :: (InstanceSum, (Either ParseError Bool)) -> IO Bool
    handleApp (sum, res) = case res of
          Left err -> do
            putStrLn $ "Apply parser failed on type: " ++ (tyStr sum)
            print (declStr sum)
            putStrLn $ "Error was: " ++ show err
            return False
          Right True -> return True
          Right False -> return False
  
processApp :: [String] -> [InstanceSum]
processApp [] = []
processApp (ln:rst) = let pRes = parseInst ln in
  case pRes of
    Left _ -> processApp rst
    Right iSum -> if (flg iSum) == App
                  then let (sum,rst2) = extractApDecl iSum rst in
                         sum:(processApp rst2)                           
                  else processApp rst
  where extractApDecl :: InstanceSum -> [String] -> (InstanceSum,[String])
        extractApDecl iSum rst = let decl = takeWhile pred rst
                                     rst2 = drop (length decl) rst in
          (iSum{declStr = Just $ (replaceInlines . unlines) decl}, rst2)
        pred :: String -> Bool
        pred ln = not (startswith "Results from:" ln || startswith "Applicative instance:" ln || startswith "Monad instance:" ln)

replaceInlines :: String -> String
replaceInlines = (replace "{-# INLINE pure #-}" "") . (replace "{-# INLINE (<*>) #-}" "")
  
processOutputFileLn :: String -> (Maybe InstFlag)
processOutputFileLn ln = let pRes = parseInst ln in
  case pRes of
    Left _ -> Nothing
    Right iSum -> Just $ flg iSum

parseInst :: String -> Either ParseError InstanceSum
parseInst = parse parseSingleInstance "none"

parseSingleInstance :: CharParser () InstanceSum
parseSingleInstance  = mkSum <$> (string "instance" *> parseContext *> parseFlg) <*> parseType
  where
    mkSum :: InstFlag -> String -> InstanceSum
    mkSum flg tyStr = IS tyStr Nothing flg
parseContext :: CharParser () (Maybe String)
parseContext = spaces *> option Nothing (Just <$> try (manyTill anyChar (string "=>"))) <* spaces
parseFlg :: CharParser () InstFlag
parseFlg = (parseMon <|> parseApp) <* spaces
parseMon = Mnad <$ string "Monad"
parseApp = App <$ string "Applicative"
parseType :: CharParser () String
parseType = manyTill anyChar (string "where")


isApplyAp :: CharParser () Bool
isApplyAp = try (parsePure *> parseApply) <|> (parseApply <*  parsePure)

parsePure = spaces *> string "pure" <* manyTill anyChar (string "=") <* (manyTill anyChar (char ';' <|> newline))

parseApply = manyTill anyChar (string "<*>") *> manyTill anyChar (char '=') *> defIsApp
defIsApp =  try ((const True) <$> (manyTill anyChar (string " ap") <* (space1 <|> (spaces *> char ';')))) <|> ((const False) <$> manyTill anyChar (char ';' <|> newline))
  
space1 = space <* spaces
