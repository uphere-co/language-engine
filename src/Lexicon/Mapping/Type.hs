{-# LANGUAGE OverloadedStrings #-}

module Lexicon.Mapping.Type where

import           Data.Text            (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as T.IO


type SenseID = (Text,Text)

type PBArg = Text

type FNFrameElement = Text


type RoleMapInstance = (SenseID,[(PBArg,FNFrameElement)])

parseRoleMap :: [Text] -> RoleMapInstance
parseRoleMap (i:lma:sense:frame:rest) = let lst = map (\w -> let x:y:_ = T.splitOn ":" w in (x,y)) rest
                                        in ((lma,sense),("frame",frame):lst)


loadRoleMap :: FilePath -> IO [RoleMapInstance]
loadRoleMap fp = do
  txt <- T.IO.readFile fp
  let rolemap = map parseRoleMap . map T.words . T.lines $ txt
  return rolemap



