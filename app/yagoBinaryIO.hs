import           WikiEL.ETL.RDF
import           WikiEL.ETL.Util
import           WikiEL.ETL.RDF.Binary

yagoBinarySave :: IO ()
yagoBinarySave = do
  --lines <- readlines "yago/sample"
  lines <- readlines "wordnet"
  let
    rows = mapParsed readlineYAGO lines
    --bin = encode rows
  --mapM_ print (decodeYagoRDF bin)
  --encodeFile "yago/sample.bin" rows
  print (length rows)
  --encodeFile "wordnet.bin" rows
  
yagoBinaryLoad :: IO ()
yagoBinaryLoad = do
  --rows <- decodeYAGOFile "yago/sample.bin"
  rows <- decodeYAGOFile "wordnet.bin"
  print (length rows)
  

main :: IO ()
main = yagoBinarySave
--main = yagoBinaryLoad
