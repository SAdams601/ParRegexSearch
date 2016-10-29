module SearchOutputFile where
import Text.ParserCombinators.Parsec
import ExactPrintSearch

data InstanceSum = IS { tyStr :: String
                      , declStr :: Maybe String
                      , flg :: InstFlag
                      }
                 deriving (Show)

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
