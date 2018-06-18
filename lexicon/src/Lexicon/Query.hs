{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexicon.Query where

import           Control.Lens
import           Data.Function                (on)
import           Data.Hashable                (Hashable)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Data.List                    (foldl',group,groupBy,maximumBy,sort)
import           Data.Maybe                   (listToMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T.IO
import           Data.Text.Read               (decimal)
--
import           NLP.Type.SyntaxProperty      (Voice)
--
import           Lexicon.Mapping.Addition     (additionalMapping)
import           Lexicon.Type


parseRoleInst :: [Text] -> RoleInstance
parseRoleInst (_i:lma:sense:frame:rest)
  = let lst = map (\w -> let x:y:_ = T.splitOn ":" w in (x,y)) rest
    in ((lma,Verb,sense),("frame",frame):lst)
parseRoleInst x = error ("parseRoleInstance: " ++ show x)


loadRoleInsts :: FilePath -> IO [RoleInstance]
loadRoleInsts fp = map parseRoleInst . map T.words . T.lines <$> T.IO.readFile fp


convertONIDtoSenseID :: Text -> Text -> (Text,POSVorN,Text)
convertONIDtoSenseID lma sense_num =
  if lma == "hold" && sense_num == "5"       -- this is an ad hoc treatment for group 2
  then (lma,Verb,"2." <> sense_num)
  else (lma,Verb,"1." <> sense_num)


parseGRel :: Text -> GRel
parseGRel w = case T.splitOn "-" w of
                t:ts -> case t of
                          "NP"   -> GR_NP   (listToMaybe ts >>= identifyGA)
                          "S"    -> GR_S    (listToMaybe ts >>= identifyGA)
                          "SBAR" -> GR_SBAR (listToMaybe ts >>= identifyGA)
                          "PP"   -> case ts of
                                      prep:"ing":_ -> GR_PP (Just (prep,True))
                                      _prep:_:_     -> error "missing case in parseGRel"
                                      prep:[]      -> GR_PP (Just (prep,False))
                                      []           -> GR_PP Nothing
                          "ADVP" -> GR_ADVP (listToMaybe ts)
                          "ADJP" -> GR_ADJP
                          _      -> GR_X w
                _ -> GR_X w
  where
    identifyGA "SBJ" = Just GASBJ
    identifyGA "1"   = Just GA1
    identifyGA "2"   = Just GA2
    identifyGA _     = Nothing


parseWithNullCheck :: (Text -> a) -> Text -> Maybe a
parseWithNullCheck f w = if w == "null" then Nothing else Just (f w)


parseRolePattInst
  :: (Read p) =>
     [Text] -> (SenseID, ArgPattern p GRel, Int)
parseRolePattInst ws@[lma',sense',mvoice,marg0,marg1,marg2,marg3,marg4,count] =
    ( sid
    , ArgPattern (parseWithNullCheck (read . T.unpack) mvoice)
                 (parseWithNullCheck parseGRel marg0)
                 (parseWithNullCheck parseGRel marg1)
                 (parseWithNullCheck parseGRel marg2)
                 (parseWithNullCheck parseGRel marg3)
                 (parseWithNullCheck parseGRel marg4)
    , either (error ("error: " ++ show ws)) fst (decimal count)
    )
  where lma = T.intercalate "-" . init . T.splitOn "-" $ lma'
        sid = convertONIDtoSenseID lma sense'
parseRolePattInst _ = error "parseRolePattInst"


adjustRolePattInsts :: [RolePattInstance Voice] -> [RolePattInstance Voice]
adjustRolePattInsts roles = foldl' adjust roles additionalMapping
  where
    adjust :: [RolePattInstance Voice]
           -> (SenseID,ArgPattern Voice GRel)
           -> [RolePattInstance Voice]
    adjust rs (sid,patt) =
      let defresult = [(patt,1)]
          (ps,ns) = break ((== sid) . (^._1)) rs
      in case ns of
           []          -> ps ++ ((sid,defresult) : ns)
           ((_,opatts):ns') ->
             if null opatts
             then ps ++ ((sid,defresult) : ns')
             else
               let newresult =
                     let (_,maxn) = maximumBy (compare `on` (^._2)) opatts
                         (p1s,n1s) = break ((== patt) . (^._1)) opatts
                     in case n1s of
                          [] -> (patt,maxn) : p1s
                          (_:n1s') -> p1s ++ ((patt,maxn) : n1s')
               in ps ++ ((sid,newresult) : ns')




loadRolePattInsts :: (Read v,Hashable v) => FilePath -> IO [RolePattInstance v]
loadRolePattInsts fp = do
  txt <- T.IO.readFile fp
  let getLemmaSense x = x^._1
      roles_from_file = map (\xs  -> (getLemmaSense (head xs),map (\x->(x^._2,x^._3)) xs))
                      . groupBy ((==) `on` getLemmaSense)
                      . map parseRolePattInst
                      . map T.words
                      . T.lines
                      $ txt
  return roles_from_file


cutHistogram :: Double -> [(a,Int)] -> [(a,Int)]
cutHistogram c xs = let ys = scanl (\(!acc,f) (x,i) -> (acc+i, f . ((x,i):))) (0,id) xs
                        n = fst (last ys )
                        ncut= fromIntegral n * c
                    in (\x -> (x^._2) []) . head  . snd . break (\x->fromIntegral (x^._1) >= ncut) $ ys



loadIdioms :: FilePath -> IO (HashMap SenseID [[Text]])
loadIdioms fp = do
  txt <- T.IO.readFile fp
  let ts = map (map T.strip . T.splitOn "\t") (T.lines txt)
      f !acc (lma:onid:xs) =
        let sid = (lma,Verb,onid) -- convertONIDtoSenseID lma onid
            xss = (map (T.words . head) . group . sort) xs
        in HM.insert sid xss acc
      f !acc _ = acc
  return (foldl' f HM.empty ts)
