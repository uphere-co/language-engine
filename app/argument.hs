{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception
import           Control.Lens                hiding ((<.>))
import           Control.Monad
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson
import qualified Data.Attoparsec.Text       as A
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable
import           Data.Function                      (on)
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict        as HM
import qualified Data.IntMap                as IM
import           Data.List
import           Data.Maybe                         (fromJust,fromMaybe,maybeToList)
import           Data.Monoid
import           Data.Text                          (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import           Data.Traversable
import qualified Data.Tree                  as Tree
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           Text.Printf
--
import           CoreNLP.Simple.Type.Simplified
import           NLP.Parser.PennTreebankII
import           NLP.Printer.PennTreebankII
import           NLP.Type.PennTreebankII
import qualified NLP.Type.PennTreebankII.Separated as N
import           PropBank.Format
import           PropBank.Parser.Prop
import           PropBank.Match
import           PropBank.Query
import           PropBank.Type.Frame
import           PropBank.Type.Match
import           PropBank.Type.Prop
import           PropBank.Util                     (merge)
import           SRL.Feature.Clause
import           SRL.Feature.Verb
import           SRL.Type.Verb
import           Text.Format.Tree


data MatchResult = ExactMatch Range
                 | MergeMatch [Range]
                 | Unmatched
                 deriving Show


newtype NoOverlapSegments = NoOverlapSegments [Range]
                          deriving Show

mkNoOverlapSegments :: [Range] -> Maybe NoOverlapSegments
mkNoOverlapSegments [] = Just (NoOverlapSegments [])
mkNoOverlapSegments (x:xs) =
  let ys = sortBy (compare `on` fst) (x:xs)
      zs = zip ys (tail ys)
  in fmap (NoOverlapSegments . (head ys :))
          (mapM (\((i,j),(i',j')) -> if j < i' then Just (i',j') else Nothing) zs)


test = mkNoOverlapSegments [(1,3), (7,9), (4,5) ]
test2 = mkNoOverlapSegments [(1,3), (6,9), (4,6) ]


newtype ContiguousSegments = ContiguousSegments [[Range]]
                           deriving Show


-- the implementation is not very good. I will change it.
mkContiguousSegments :: NoOverlapSegments -> ContiguousSegments
mkContiguousSegments (NoOverlapSegments xs) = ContiguousSegments (go [] [] xs)
  where go deck   acc []     = acc ++ [reverse deck]
        go []     acc (r:rs) = go [r] acc rs
        go (z:zs) acc (r:rs) = let (i,j) = r
                                   (i',j') = z
                               in if j'+1 == i
                                  then go (r:z:zs) acc          rs
                                  else go [r]      (acc ++ [reverse (z:zs)]) rs

test3 = fmap mkContiguousSegments test

contiguousMatch :: ContiguousSegments -> Range -> [Range]
contiguousMatch (ContiguousSegments segs) (i,j) = foldMap match segs 
  where match xs = 
          let (_,bs) = break (\x->x^._1 == i) xs
              (es,rs) = break (\x->x^._2 == j) bs
          in case rs of
               []    -> []
               (r:_) -> es++[r]


toMatchResult :: [Range] -> MatchResult
toMatchResult []     = Unmatched
toMatchResult (x:[]) = ExactMatch x
toMatchResult xs     = MergeMatch xs


errorHandler h_err msg action = do
  r <- try action
  case r of
    Left (e :: SomeException) -> hPutStrLn h_err msg >> hFlush h_err
    _ -> return ()

  
prepare {- framedir -} basedir = do
  -- propdb <- constructFrameDB framedir
  -- let preddb = constructPredicateDB propdb
  dtr <- build basedir
  let fps = sort (toList (dirTree dtr))
      props = filter (\x -> takeExtensions x == ".prop") fps
      trees = filter (\x -> takeExtensions x == ".parse") fps
  return ((),props,trees) -- (preddb,props,trees)


readPropBank propfile = liftIO $ parsePropWithFileField NoOmit <$> T.IO.readFile propfile


readOrigPennTree pennfile
  = hoistEither . A.parseOnly (A.many1 (A.skipSpace *> pnode))
    =<< liftIO (T.IO.readFile pennfile)


readJSONList :: (FromJSON a) => FilePath -> EitherT String IO [a]
readJSONList file = EitherT $ eitherDecode <$> liftIO (BL.readFile file)


loadMatchArticle ptreedir {- framedir -} basedir article = do
  (preddb,props,trees) <- liftIO $ prepare {- framedir -} basedir
  let findf = find (\f -> takeBaseName f == article)
  flip traverse ((,) <$> findf props <*> findf trees) $ \(fprop,ftree) -> do
    insts <- readPropBank fprop
    proptrs' <- readOrigPennTree ftree
    let proptrs = map convertTop proptrs'
        ptreefile = article <.> "corenlp_ptree"
        depfile = article <.> "corenlp_udep"
        plemmafile = article <.> "corenlp_lemma"
    coretrs :: [PennTree]  <- readJSONList  (ptreedir </> ptreefile)
    coredeps :: [Dependency] <- readJSONList (ptreedir </> depfile)
    corelmas :: [[(Int,Text)]] <- readJSONList (ptreedir </> plemmafile)
    let cores = zip3 coretrs coredeps corelmas
        pairs = zip cores proptrs
    return (merge (^.inst_tree_id) pairs insts)


propbankCorpus ptreedir basedir article = do
  void . runEitherT $ do
    lst <- concat <$> loadMatchArticle ptreedir {- framedir -} basedir article
    liftIO . flip mapM_ lst $ \(i,(((coretr,coredep,corelma),proptr),insts)) -> do
      putStrLn "\n\n\n-------------"
      putStrLn $ "sentence " ++ show i
      putStrLn "-------------"
      let tokens = map (^._2) . toList $ coretr
      T.IO.putStrLn (T.intercalate " " tokens)

      putStrLn "\n\n======"
      let minsts = matchInstances (coretr,proptr) insts
          lmap = IM.fromList (map (_2 %~ Lemma) corelma)
          verbprops = verbPropertyFromPennTree lmap coretr
          tr = clauseStructure verbprops (bimap (\(rng,c) -> (rng,N.convert c)) id (mkPennTreeIdx coretr))
      
      {- 
      putStrLn "-----"
      putStrLn "propbank"
      putStrLn "-----"
      flip mapM_ minsts $ \minst-> do
        let inst = minst^.mi_instance
            args = minst^.mi_arguments
        print (findRelNode (minst^.mi_arguments),inst^.inst_lemma_roleset_id)
        mapM_ (print . (\a->(a^.ma_argument.arg_label.to pbLabelText,a^..ma_nodes.traverse.mn_node._1))) args

      putStrLn "-----"
      putStrLn "verb property"
      putStrLn "-----"
      print $ map (\vp->(vp^.vp_index,vp^.vp_lemma.to unLemma)) verbprops
      -- print verbprops
      -- print $ map (\vp->getVerbArgs tr vp) verbprops
      -}
      showClauseStructure lmap coretr
      
      putStrLn "-----"
      putStrLn "propbank match test"
      putStrLn "-----"
      
      flip mapM_ minsts $ \minst-> do
        let inst = minst^.mi_instance
            args = filter (\a->a^.ma_argument.arg_label /= Relation) (minst^.mi_arguments)
        putStrLn "*************"
        T.IO.putStrLn (formatRoleSetID (inst^.inst_lemma_roleset_id))
        let relidx = findRelNode (minst^.mi_arguments)
            mvpva = do vp <- find (\vp->vp^.vp_index==relidx) verbprops
                       va <- getVerbArgs tr vp
                       return (vp,va)
        case mvpva of
          Nothing -> putStrLn "unmatched!"
          Just (vp,va) -> do
            print "relation matched"
            let vargs = maybeToList (va^.va_arg0) ++ va^.va_args
            
            flip mapM_ args $ \arg -> do
              let ns = arg^..ma_nodes.traverse.mn_node._1
                  getRng = either (^._1) (\x->(x^._1,x^._1)) 
              let nosegs = fromJust (mkNoOverlapSegments (map getRng vargs))
              putStrLn $
                printf "%15s : %s"
                  (arg^.ma_argument.arg_label.to pbLabelText)              
                  (show (zip ns (map (toMatchResult . contiguousMatch (mkContiguousSegments nosegs)) ns)))
        -- mapM_ (print . (\a->(a^.ma_argument.arg_label.to pbLabelText,a^..ma_nodes.traverse.mn_node._1))) args

      putStrLn "-----"
      putStrLn "dependency"
      putStrLn "-----"
      let tokens = map (^._2) . toList $ coretr
          tkmap = IM.fromList (zip [0..] tokens)
          deptree = fmap (\(i,r) -> let t = fromMaybe "" (IM.lookup (i-1) tkmap) in (i-1,t,r))
                      (dependencyLabeledTree coredep)
      T.IO.putStrLn (linePrint (T.pack.show) deptree)


main = do
  let --  article = "wsj_2445"
      ptreedir = "/scratch/wavewave/run/ontonotes_corenlp_ptree_udep_lemma_20170710"
      -- framedir = "/scratch/wavewave/MASC/Propbank/Propbank-orig/framefiles"
      basedir = "/scratch/wavewave/LDC/ontonotes/b/data/files/data/english/annotations/nw/wsj"

  dtr <- build basedir
      
  let fps = sort (toList (dirTree dtr))
      parsefiles = filter (\x -> takeExtensions x == ".parse") fps
  flip mapM_ parsefiles $ \f -> do
    let article = takeBaseName f
    putStrLn "\n\n\n=============================================================================================="
    print article
    putStrLn "=============================================================================================="

    errorHandler stderr ("error happened in " ++ article) (propbankCorpus ptreedir basedir article)
