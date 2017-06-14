{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Lens              ((^?),(^.),(^..), only )
import           Control.Monad             (join)
-- import           Control.Monad.Identity
import           Control.Monad.Loops       (whileJust_)
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.HashMap.Strict as HM
import           Data.List                 (sort)
import           Data.Maybe                (fromJust,listToMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
import           Data.Text.Read            (decimal)
import           Data.Time.Clock
import           Data.Time.Format          (parseTimeM,defaultTimeLocale)
import           Options.Applicative
import           System.Console.Haskeline
import           System.FilePath
import           System.Directory
import           Text.Taggy.Lens
--
import           HFrameNet.Parse.Frame         (p_frame)
import qualified HFrameNet.Parse.LexUnit as LU (p_lexUnit)
import           HFrameNet.Query

processLU fp = do
  putStrLn fp
  
  txt <- TLIO.readFile fp
  let lu = head (txt ^.. (html . allNamed (only "lexUnit")))
  --print lu
  --print (LU.p_lexUnit lu)
  case LU.p_lexUnit lu of
    Nothing -> error fp
    Just x -> return () --print x
  

data ProgOption = ProgOption { fnDataDir :: FilePath
                             , progCommand :: String
                             } deriving Show

pOptions :: Parser ProgOption
pOptions = ProgOption <$> strOption (long "dir" <> short 'd' <> help "Directory")
                      <*> strArgument (help "command frame/lu")

  -- switch (long "frame" <> short 'f' <> help "process frame, default is lexical unit")

progOption :: ParserInfo ProgOption 
progOption = info pOptions (fullDesc <> progDesc "query program for FrameNet")



queryFrame :: FilePath -> IO ()
queryFrame dir = do
  cnts <- getDirectoryContents dir
  let lst = (map (\x -> dir </> x) . filter (\x -> takeExtensions x == ".xml")) cnts
  framemap <- constructFrameDB lst
  runInputT defaultSettings $ whileJust_ (getInputLine "% ") $ \input' -> liftIO $ do
     let input = T.pack input'
     print (HM.lookup input (framemap^.frameDB))
     -- mapM_ processFrame $ map (\x -> dir </> x) lst


main :: IO ()
main = do
  opt <- execParser progOption
  -- let dir = fnDataDir opt
  case progCommand opt of
    "frame" -> queryFrame (fnDataDir opt </> "frame")
    "lu" -> do
      let dir = fnDataDir opt </> "lu"
      cnts <- getDirectoryContents dir
      let lst = filter (\x -> takeExtensions x == ".xml") $ sort cnts
      mapM_ processLU $ map (\x -> dir </> x) lst
    o -> putStrLn ("cannot understand command " ++ o )
