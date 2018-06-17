{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module FrameNet.Type.Definition where

import           Control.Lens         ((^?),(^.),(^..),only)
import           Data.List            (intersperse)
import           Data.Text            (Text)
import qualified Data.Text       as T
import           Text.Taggy.Lens

-- data Ex =

data EContent = ETEXT Text
              | ERET
              | EM Text
              | EFEX Text
              deriving (Show)

data CContent = CTEXT Text
              | CFEN Text
              | CEX [EContent]
              | CRET
              deriving (Show)



data DefRoot = DefRoot [CContent]




p_fen :: [Node] -> [CContent]
p_fen (NodeContent txt:_) = [CFEN txt]
p_fen _                   = []

p_ctext :: Text -> [CContent]
p_ctext = filter (\case CTEXT t -> not (T.null t); _ -> True) . intersperse CRET . map CTEXT . T.lines

p_etext :: Text -> [EContent]
p_etext = filter (\case ETEXT t -> not (T.null t); _ -> True) . intersperse ERET . map ETEXT . T.lines

p_m :: [Node] -> [EContent]
p_m (NodeContent txt:_) = [EM txt]
p_m _ = []

p_ex1 :: Node -> [EContent]
p_ex1 (NodeContent txt) = p_etext txt
p_ex1 (NodeElement e)   = case e^.name of
                            "fex" -> maybe [] (return . EFEX) (e^.attr "name")
                            "m" -> p_m (e^.children)
                            _ -> []

p_ex :: [Node] -> [EContent]
p_ex ns = ns >>= p_ex1

p_content :: Node -> [CContent]
p_content (NodeContent txt) = p_ctext txt
p_content (NodeElement e) = case e^.name of
                              "fen" -> p_fen (e^.children)
                              "ex" -> [CEX (p_ex (e^.children))]
                              _     -> []

p_defRoot txt = DefRoot $ do e <- txt^..html.allNamed (only "def-root")
                             p_content =<< (e^.children)


{-
p_defroot :: Element -> [Node]
p_defroot x =
-}
