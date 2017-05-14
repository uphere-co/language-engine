import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
--
import WordNet

parseFile :: (Show a) => (Text -> Maybe a) -> FilePath -> IO ()
parseFile p fp = do
  txt <- TIO.readFile fp 
  let lst = take 100 $ filter (not.isComment) (T.lines txt)
  mapM_ (print . (\x -> (x,p x))) lst


main = do
  -- parseFile parseIndex "/scratch/wavewave/wordnet/WordNet-3.0/dict/index.verb"
  parseFile (parseData True) "/scratch/wavewave/wordnet/WordNet-3.0/dict/data.verb"
