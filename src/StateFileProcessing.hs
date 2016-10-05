module StateFileProcessing where
import Control.Monad.State
import Data.ByteString.Char8 as BS
import Prelude hiding (lines)
import Text.Regex.Posix

{-is file will attempt to abstract the idea of filtering a file using the state monad.
The general pattern of a file search is if a set of lines match some predicate then those lines should be included in the result then the rest of the file can be processed in a similar matter.

In this case the state s is a ByteString representing the unprocessed contents of the file, the result will be a list of lines that were matched by the predicate
-}

type Match = [ByteString]

type FileState = State [ByteString]

searchListOfFiles :: ([ByteString] -> (Match,[ByteString])) -> [(FilePath, ByteString)] -> [(FilePath, [Match])]
searchListOfFiles pred files = Prelude.map collectMatches files
  where collectMatches (fp, bs) = (fp, (searchFile pred bs))

searchFile :: ([ByteString] -> (Match,[ByteString])) -> ByteString -> [Match]
searchFile pred file = let lns = lines file
  in evalState (searchComp pred) lns

searchComp :: ([ByteString] -> (Match,[ByteString])) -> FileState [Match]
searchComp pred = do
  rest <- get
  case rest of
    [] -> return []
    lns -> do
      let (match, rst) = pred lns
      put rst
      otherMatches <- searchComp pred
      case match of
        [] -> return otherMatches
        _ ->  return (match: otherMatches)

--The most basic predicate that matches an applicative instance declaration and grabs the next two lines
appInstancePred :: [ByteString] -> (Match,[ByteString])
appInstancePred (ln:lns) = if ln =~ (pack "instance Applicative")
                           then let match = ln : Prelude.take 2 lns in
                                  (match, Prelude.drop 2 lns)
                           else ([], lns)

monadInstancePred :: [ByteString] -> (Match,[ByteString])
monadInstancePred (ln:lns) = if ln =~ (pack "instance Monad\\s")
                           then let match = ln : Prelude.take 2 lns in
                                  (match, Prelude.drop 2 lns)
                           else ([], lns)

pureIsAp :: [ByteString] -> (Match,[ByteString])
pureIsAp (ln:lns) = if ln =~ (pack "instance Applicative")
                           then let (appLn, rst) = findAp lns in
                                  if (BS.null appLn)
                                  then ([], rst)
                                  else ([ln,appLn], rst)
                           else ([], lns)
  where
    findAp :: [ByteString] -> (ByteString, [ByteString])
    findAp [] = (BS.empty,[])
    findAp (l:rst) = if l =~ (pack "\\(<\\*>\\)\\s*=\\s*ap")
                     then (l,rst)
                     else findAp rst

--"<\\*>.*=.*(`| )ap(`| )"

