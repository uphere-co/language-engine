{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Foldable

import           Data.IORef
import qualified Data.Text     as T
import qualified Data.Text.IO  as T.IO
import           Data.Text.Read        (decimal)
import           System.IO
import           Text.Printf
--
import           WordNet.Format
import           WordNet.Query
import           WordNet.Type
import           WordNet.Type.POS
--
import           OntoNotes.App.Load


parsePair (w1:w2:ws) = (T.tail w1,T.tail w2)

main = do
  iref <- newIORef (0 :: Int)
  wndb <- loadDB (cfg^.cfg_wordnet_dict)
  txt <- T.IO.readFile "VOS-glossTags-g"

  let ps = map (parsePair . T.words) . T.lines $ txt

   
  forM_ ps $ \(idstr1,idstr2) -> do
    case (decimal idstr1,decimal idstr2) of
      (Right (id1,_),Right (id2,_)) -> do
        let mv = (dataDB wndb POS_V) ^. at id1
            mo = (dataDB wndb POS_N) ^. at id2
        case (mv,mo) of
          (Just v, Just o) -> do
            let vlex = (T.intercalate "," . map formatLI) $ v^.data_word_lex_id
                vgloss = v^.data_gloss
                olex = (T.intercalate "," . map formatLI) $ o^.data_word_lex_id
                ogloss = o^.data_gloss

            putStrLn "--------------------------------------------------------------------------------------------"
            putStrLn $ printf "V: %-25s | %s " vlex vgloss
            putStrLn $ printf "O: %-25s | %s " olex ogloss
            -- print (vtxt,otxt)
          _ -> do
            modifyIORef iref (+1)
            -- error (show ((id1,mv),(id2,mo)))
      _ -> error (show (idstr1,idstr2))

  i <- readIORef iref
  hPutStrLn stderr ("# of error cases : " ++ show i)
