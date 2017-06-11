{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad       (void)
import           Data.Text           (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Text.Read
import           Foreign.Ptr
--
import           Bindings.SVM

foreign import ccall "mymain0" c_mymain0 :: IO ()
foreign import ccall "mymain1" c_mymain1 :: Ptr () -> Ptr () -> IO ()

foreign import ccall "getParam" c_getParam :: IO (Ptr ())
foreign import ccall "getProb" c_getProb :: IO (Ptr ())

parseLine :: [Text] -> Either String (Int,[(Int,Double)])
parseLine (x:xs) = do
  (l,_) <- decimal x
  vs <- mapM parseEach xs 
  -- fst <$> decimal x
  return (l, vs)

parseEach :: Text -> Either String (Int,Double)
parseEach txt = do
  let (x0:x1:_) = T.splitOn ":" txt
  (n,_) <- decimal x0
  (r,_) <- double x1
  return (n,r)

{- 
createProblem v = do -- #TODO Check the problem dimension. Libsvm doesn't
                    node_array <- newArray xs
                    class_array <- newArray y
                    offset_array <- newArray $ offsetPtrs node_array
                    return (C'svm_problem (fromIntegral dim) 
                                          class_array 
                                          offset_array
                           ,node_array) 
    where 
        dim = length v
        lengths = map (V.length . snd) v
        offsetPtrs addr = take dim 
                          [addr `plusPtr` (idx * sizeOf (C'svm_node undefined undefined)) 
                          | idx <- scanl (+) 0 lengths]
        y   = map (double2CDouble . fst)  v
        xs  = concatMap (V.toList . snd) v
-}

readInputFile :: IO ()
readInputFile = do
  putStrLn "svm-train"

  txt <- TIO.readFile "./real-sim"
  let xs = T.lines txt
      xss = map T.words xs 
  print (length xs)

  mapM_ (print . parseLine) (take 2 xss )

main :: IO ()
main = do
  c_mymain0

  ptr_parameters <- c_getParam
  ptr_problem <- c_getProb
  
  -- c_mymain1 param prob
  void $ c'svm_train (castPtr ptr_problem) (castPtr ptr_parameters)
  
