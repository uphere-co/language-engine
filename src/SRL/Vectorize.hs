module SRL.Vectorize where

import           Control.Lens
import           Data.List                 (foldl')
import           Data.Maybe                (catMaybes)
import           Data.Text                 (Text)
import qualified Data.Text            as T
import           Data.Vector.Storable      (Vector,MVector(..))
import qualified Data.Vector.Storable as V
import           Foreign.ForeignPtr
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.C.Types
--
import           FastText.Binding
import           NLP.Type.PennTreebankII
import           PropBank.Type.Prop
--
import           SRL.Type

foreign import ccall "get_vector" c_get_fasttextvector :: FastTextVector -> IO (Ptr CFloat)

initWVDB :: FilePath -> IO FastText
initWVDB binfile = do
  cstr_bin <- newCString binfile
  t <- newFastText
  str_bin <- newCppString cstr_bin
  fastTextloadModel t str_bin
  return t

word2vec :: FastText -> Text -> IO (Vector CFloat)
word2vec ft w =
  withCString (T.unpack w) $ \cstr_word -> do
    let c_size = 300  -- we need to replace this with correct impl.
    v <- newFastTextVector c_size
    str_word <- newCppString cstr_word
    fastTextgetVector ft v str_word
    cfloat <- c_get_fasttextvector v
    ptr <- newForeignPtr_ cfloat
    let mv = MVector (fromIntegral c_size) ptr
    let v = V.create (return mv)
    return $! v

{- 
data RolesetID = SenseID Int  -- ^ sense id
               | LV           -- ^ light verb
               | ER           -- ^ treebank error
               | DP           -- ^ verb gap construction
-}
    

pblabel2idx :: PropBankLabel -> Maybe Int
pblabel2idx Relation = Nothing
pblabel2idx (NumberedArgument n) | n <= 4 && n >= 0 = Just n
                                 | otherwise        = Nothing
pblabel2idx (Modifier m) = Just (fromEnum m + 5)
pblabel2idx (LinkArgument l) = Just (fromEnum l + fromEnum (maxBound :: ModifierType) + 6)

mk1HotVec :: Int -> Maybe Int -> Vector CFloat
mk1HotVec dim Nothing = V.replicate dim 0
mk1HotVec dim (Just n) = V.generate dim (\i -> if i == n then 1.0 else 0.0)

pblabel2vec = mk1HotVec dim . pblabel2idx
  where dim = fromEnum (maxBound :: LinkType) + fromEnum (maxBound :: ModifierType) + 6

position2vec :: Position -> Vector CFloat
position2vec Before = mk1HotVec 2 (Just 0)
position2vec After  = mk1HotVec 2 (Just 1)
position2vec Embed  = mk1HotVec 2 Nothing

direction2vec :: Direction -> Vector CFloat
direction2vec Up   = mk1HotVec 2 (Just 0)
direction2vec Down = mk1HotVec 2 (Just 1)

enum2vec :: (Bounded a, Enum a) => a -> Vector CFloat
enum2vec x = mk1HotVec dim (Just (fromEnum x))
  where dim = fromEnum (maxBound :: POSTag) + 1

ptp2vec :: ParseTreePath -> Vector CFloat
ptp2vec xs = if n < maxn then v0 V.++ V.replicate (maxn-n) 0 else V.take maxn v0
  where ptp2idx1 (Left  p,d) = enum2vec p V.++ enum2vec d
        ptp2idx1 (Right t,d) = enum2vec t V.++ enum2vec d
        --
        v0 = foldl' (\acc x -> acc V.++ ptp2idx1 x) V.empty xs
        n = V.length v0
        dimc = fromEnum (maxBound :: ChunkTag) + 1
        dimp = fromEnum (maxBound :: POSTag) + 1
        maxn  = 10*(dimc + 2) + dimp+2 

argnode2vec :: FastText -> ArgNodeFeature -> IO (Maybe (Vector CFloat))
argnode2vec ft (arglabel,(_,ptp,Just (_,(_,(pos,word))))) = do
  let -- v1 = pblabel2vec arglabel 
      v2 = ptp2vec ptp
      v3 = enum2vec pos
  v4 <- word2vec ft word
  let v = {- v1 V.++ -} v2 V.++ v3 {- V.++ v4 -}
  v `seq` return (Just v)
argnode2vec ft (arglabel,(_,ptp,Nothing)) = return Nothing

 
inst2vec :: FastText -> InstanceFeature -> IO [(Int,RoleSet,PropBankLabel,Range,Vector CFloat)]
inst2vec ft ifeat = do
  predv <- {- (V.++) <$> word2vec ft (ifeat^._2._1) <*> -} pure (enum2vec (ifeat^._3))
  rs <- flip traverse (concat (ifeat^._4)) $ \nfeat -> do
    let n = ifeat^._1
        roleset=  ifeat^._2
        label = nfeat^._1
        rng = nfeat^._2._1
    mvec <- argnode2vec ft nfeat
    return $ fmap (\v -> (n,roleset,label,rng, predv V.++ v)) mvec
  return (catMaybes rs)
