{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative
import           Control.Monad        (void)
import           Data.Text            (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Text.Read
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Marshal.Array (allocaArray,mallocArray,pokeArray)
import           Foreign.Ptr
import           Foreign.Storable      (poke)
--
import           Bindings.SVM

foreign import ccall "mymain0" c_mymain0 :: IO ()
foreign import ccall "mymain1" c_mymain1 :: Ptr () -> Ptr () -> IO ()

foreign import ccall "getParam" c_getParam :: IO (Ptr ())
foreign import ccall "getProb" c_getProb :: IO (Ptr ())

parseLine :: [Text] -> Either String (Int,[(Int,Double)])
parseLine lst =
  let p (x:xs) = do (l,_) <- signed decimal x
                    vs <- mapM parseEach xs 
                    return (l, vs)
                    
  in case p lst of
       Left err -> Left (err ++ (T.unpack (T.intercalate " " lst)))
       Right r -> Right r 


    
parseEach :: Text -> Either String (Int,Double)
parseEach txt = do
  let (x0:x1:_) = T.splitOn ":" txt
  (n,_) <- decimal x0
  (r,_) <- double x1
  return (n,r)


convertNode :: (Int,Double) -> C'svm_node
convertNode (i,v) = C'svm_node { c'svm_node'index = fromIntegral i
                               , c'svm_node'value = realToFrac v }

endNode :: C'svm_node
endNode = C'svm_node (-1) 0


convertY :: Ptr CDouble -> [Int] -> IO ()
convertY p labels = do
  let labels' :: [CDouble] = map fromIntegral labels
  pokeArray p labels'



convertX1 :: [(Int,Double)] -> IO (Ptr C'svm_node)
convertX1 nodes = do
  let l = length nodes
  p <- mallocArray (l+1)
  pokeArray p (map convertNode nodes ++ [endNode])
  return p
  
  
withProblem :: [(Int,[(Int,Double)])] -> (C'svm_problem -> IO a) -> IO a
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
      action prob



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

  
main :: IO ()
main = do
  inputs <- readInputFile
  alloca $ \p_prob -> do
    withProblem inputs $ \prob -> do
      poke p_prob prob
      c_mymain0
      ptr_parameters <- c_getParam
      void $ c'svm_train p_prob (castPtr ptr_parameters)
  
