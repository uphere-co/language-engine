{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Lens          hiding (Level)
import           Control.Monad.Loops
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.ByteString.Char8   as B
import           Data.Default
import           Data.Foldable
import qualified Data.IntMap           as IM
import           Data.List                    (foldl',minimumBy,sort,sortBy,zip4)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text               as T
import qualified Data.Text.IO            as T.IO
import           Language.Java           as J
import           System.Console.Haskeline
import           System.Environment
import           Text.Printf
--
import qualified CoreNLP.Proto.CoreNLPProtos.Sentence  as S
import qualified CoreNLP.Proto.CoreNLPProtos.Token     as TK
import           CoreNLP.Simple
import           CoreNLP.Simple.Convert
import           CoreNLP.Simple.Type
import           CoreNLP.Simple.Type.Simplified
import           CoreNLP.Simple.Util
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import           SRL.Format                                  (formatBitree,showVerb)
import           SRL.Feature
import           SRL.Feature.Clause
import           SRL.Feature.Dependency
import           SRL.Feature.Verb


convertToken_charIndex :: TK.Token -> Maybe Token
convertToken_charIndex t = do
  (b',e') <- (,) <$> t^.TK.beginChar <*> t^.TK.endChar
  let (b,e) = (fromIntegral b',fromIntegral e')
  w <- cutf8 <$> (t^.TK.originalText)
  p <- identifyPOS . cutf8 <$> (t^.TK.pos)
  l <- cutf8 <$> (t^.TK.lemma)
  return (Token (b,e) w p l)

formatLemmaPOS t = printf "%10s %5s" (t^.token_lemma) (show (t^.token_pos))


runParser pp txt = do
  doc <- getDoc txt
  ann <- annotate pp doc
  pdoc <- getProtoDoc ann
  let psents = getProtoSents pdoc
  
      parsetrees = map (\x -> pure . decodeToPennTree =<< (x^.S.parseTree) ) psents
      sents = map (convertSentence pdoc) psents
      Right deps = mapM sentToDep psents

      tktokss = map (getTKTokens) psents
      tokss = map (mapMaybe convertToken_charIndex) tktokss
  return (psents,sents,tokss,parsetrees,deps)


sentStructure pp txt = do
  (psents,sents,tokss,mptrs,deps) <- runParser pp txt
  -- putStrLn "--------------------------------------------------------------------------------------------------"
  -- print (
  -- T.IO.putStrLn txt
  -- putStrLn "---------------------------------------------------------------"
  flip mapM_ (zip4 psents sents mptrs deps) $ \(psent,sent,mptr,dep) -> do
    flip mapM_ mptr $ \ptr -> do
      let tkns = zip [0..] (getTKTokens psent)
          tkmap = IM.fromList (mapMaybe (\tk -> (tk^._1,) <$> tk^._2.TK.word.to (fmap cutf8)) tkns)
      
      let itr = mkAnnotatable (mkPennTreeIdx ptr)
          lmap= mkLemmaMap psent
          iltr = lemmatize lmap itr
          idltr = depLevelTree dep iltr
          vps = verbPropertyFromPennTree lmap ptr
          vtree = verbTree vps idltr
      putStrLn "--------------------------------------------------------------------------------------------------"
      T.IO.putStrLn  . T.intercalate "\t" . map (\(i,t) ->  (t <> "-" <> T.pack (show i))) . zip [0..] . map snd . toList $ ptr

      putStrLn "--------------------------------------------------------------------------------------------------"      
      mapM_ (putStrLn . formatLemmaPOS) . concatMap (filter (\t -> isVerb (t^.token_pos))) $ tokss
          
      -- mapM_ (T.IO.putStrLn . formatBitree (^._2.to (showVerb tkmap))) vtree
      -- putStrLn "---------------------------------------------------------------"
      showClauseStructure lmap ptr
      -- putStrLn "---------------------------------------------------------------"
      -- (T.IO.putStrLn . prettyPrint 0) ptr
      putStrLn "--------------------------------------------------------------------------------------------------"  
      









queryProcess pp = do
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
    let input = T.pack input'
    sentStructure pp input
    

main = do
  clspath <- getEnv "CLASSPATH"
  J.withJVM [ B.pack ("-Djava.class.path=" ++ clspath) ] $ do
    pp <- prepare (def & (tokenizer .~ True)
                       . (words2sentences .~ True)
                       . (postagger .~ True)
                       . (lemma .~ True)
                       . (constituency .~ True)
                  )

    -- mapM_ (sentStructure pp . (^._3) ) ordered
    -- sentStructure pp txt
    queryProcess pp
