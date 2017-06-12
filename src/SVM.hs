{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SVM where

import           Control.Applicative
import           Control.Monad        (void)
import           Data.Text            (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Text.Read
import           Foreign.C.String      (withCString)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (alloca,free)
import           Foreign.Marshal.Array (allocaArray,mallocArray,pokeArray)
import           Foreign.Ptr
import           Foreign.Storable      (poke)
--
import           Bindings.SVM


newtype SVM = SVM (Ptr C'svm_model)


{- 
parseLine :: [Text] -> Either String (Int,[(Int,Double)])
parseLine lst =
  let p (x:xs) = do (l,_) <- signed decimal x
                    vs <- mapM parseEach xs 
                    return (l, vs)
                    
  in case p lst of
       Left err -> Left (err ++ (T.unpack (T.intercalate " " lst)))
       Right r -> Right r 

-}

{- 
parseEach :: Text -> Either String (Int,Double)
parseEach txt = do
  let (x0:x1:_) = T.splitOn ":" txt
  (n,_) <- decimal x0
  (r,_) <- double x1
  return (n,r)
-}

convertNode :: (Int,Double) -> C'svm_node
convertNode (i,v) = C'svm_node { c'svm_node'index = fromIntegral i
                               , c'svm_node'value = realToFrac v }

endNode :: C'svm_node
endNode = C'svm_node (-1) 0


convertY :: Ptr CDouble -> [Double] -> IO ()
convertY p labels = do
  let labels' :: [CDouble] = map realToFrac labels
  pokeArray p labels'



convertX1 :: [(Int,Double)] -> IO (Ptr C'svm_node)
convertX1 nodes = do
  let l = length nodes
  p <- mallocArray (l+1)
  pokeArray p (map convertNode nodes ++ [endNode])
  return p
  
  
withProblem :: [(Double,[(Int,Double)])] -> (C'svm_problem -> IO a) -> IO a
withProblem dat action = do
  let l = length dat
  allocaArray l $ \p_y -> do
    allocaArray l $ \p_x -> do
      let daty = map fst dat
      convertY p_y daty
      p_xs <- mapM convertX1 (map snd dat)
      pokeArray p_x p_xs 
      let prob = C'svm_problem { c'svm_problem'l = fromIntegral l
                               , c'svm_problem'y = p_y
                               , c'svm_problem'x = p_x
                               }
      r <- prob `seq` action prob
      deleteProblem prob
      return r

deleteProblem _ = return ()

{- 
readInputFile :: IO [(Int,[(Int,Double)])]
readInputFile = do
  putStrLn "svm-train"

  txt <- TIO.readFile "./real-sim"
  let xs = T.lines txt
      xss = map T.words xs 
  print (length xs)

  let einputs = mapM parseLine xss
  case einputs of
    Left err -> error err
    Right inputs -> return inputs

-}

param =
  C'svm_parameter { c'svm_parameter'svm_type = c'EPSILON_SVR -- c'C_SVC
                  , c'svm_parameter'kernel_type = c'RBF
                  , c'svm_parameter'degree = 3
                  , c'svm_parameter'gamma = 0.5
                  , c'svm_parameter'coef0 = 0 
                  , c'svm_parameter'cache_size  = 1000
                  , c'svm_parameter'eps = 1e-3
                  , c'svm_parameter'C = 8.0
                  , c'svm_parameter'nr_weight = 0
                  , c'svm_parameter'weight_label = nullPtr
                  , c'svm_parameter'weight = nullPtr
                  , c'svm_parameter'nu = 0.5
                  , c'svm_parameter'p = 0.1
                  , c'svm_parameter'shrinking = 1
                  , c'svm_parameter'probability = 0 
                  }

trainSVM :: [(Double,[(Int,Double)])]-> IO SVM
trainSVM inputs = do
  -- inputs <- readInputFile
  alloca $ \p_param -> do 
    alloca $ \p_prob -> do
      withProblem inputs $ \prob -> do
        poke p_param param
        poke p_prob prob
        {- void $ -}
        SVM <$> c'svm_train p_prob p_param

saveSVM :: FilePath -> SVM -> IO ()
saveSVM fp (SVM p_model) =
  withCString fp $ \cstr ->
    c'svm_save_model cstr p_model

loadSVM :: FilePath -> IO SVM
loadSVM fp = SVM <$> withCString fp c'svm_load_model

predict :: SVM -> [(Int,Double)] -> IO Double
predict (SVM p_model) nodes = do
  p_nodes <- convertX1 nodes
  v <- c'svm_predict p_model p_nodes
  v `seq` do
    free p_nodes
    return (realToFrac v)
