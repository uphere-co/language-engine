module Main where

import           Data.Aeson                as Aeson
import qualified Data.Attoparsec.Text      as A
import           Data.Binary               as Binary
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text.IO              as T.IO
import           Hexdump
--
import           NLP.Parser.PennTreebankII
import           NLP.Type.PennTreebankII


readPennTree pennfile = A.parseOnly (A.many1 (A.skipSpace *> pnode)) <$> T.IO.readFile pennfile

main :: IO ()
main = do
  let fp = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj/23/wsj_2301.parse"

  e <- readPennTree fp
  case e of
    Left err -> print err
    Right tr -> do
     let lbstr_aeson = Aeson.encode tr
         lbstr_binary = Binary.encode tr
     BL.putStrLn lbstr_aeson
     putStrLn (prettyHex (BL.toStrict lbstr_binary))

 
     let test = do tr_aeson  <- Aeson.decode lbstr_aeson   :: Maybe [PennTree]
                   tr_binary <- Just (Binary.decode lbstr_binary) :: Maybe [PennTree]
                   return (tr_aeson == tr_binary)

     print (Aeson.decode lbstr_aeson :: Maybe [PennTree])
     print (Binary.decode lbstr_binary :: [PennTree])
     
     print test
