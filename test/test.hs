import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
--
import WordNet
import WordNet.Type


parseFile :: FilePath -> IO ()
parseFile fp = do
  txt <- TIO.readFile fp 
  let lst = take 100 $ filter (not.isComment) (T.lines txt)
  mapM_ (print . (\x -> (x,parse x))) lst

main = parseFile "/scratch/wavewave/wordnet/WordNet-3.0/dict/index.verb"
