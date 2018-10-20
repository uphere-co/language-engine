{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-name-shadowing -fno-warn-unused-matches -fno-warn-unused-top-binds #-}

module SRL.Analyze.Format.OGDF (
    mkOGDFSVG
  ) where

import           Control.Lens ((^.))
import           Control.Monad (void, when)
import           Control.Monad.Loops (iterateUntilM)
import           Data.Bits ((.|.))
import           Data.ByteString.Char8 (useAsCString)
import           Data.Foldable (forM_)
import           Data.List (find)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import           Data.Traversable (forM)
import           Foreign.C.String (withCString)
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import           Formatting ((%),(%.))
import qualified Formatting as F
import           System.IO (hPutStrLn, stderr)
--
import           STD.CppString
import           STD.Deletable
import           OGDF.DPoint
import           OGDF.DPolyline
import           OGDF.EdgeElement
import           OGDF.Graph
import           OGDF.GraphAttributes
import           OGDF.GraphIO
import           OGDF.LayoutModule
import           OGDF.MedianHeuristic
import           OGDF.NodeElement
import           OGDF.OptimalHierarchyLayout
import           OGDF.OptimalRanking
import           OGDF.SugiyamaLayout
--
import           Lexicon.Type (FNFrame(..),FNFrameElement(..))
import           SRL.Analyze.Type (MeaningGraph
                                  ,MGVertex(..)
                                  ,mg_vertices,mg_edges
                                  ,me_start,me_end,me_prep,me_relation
                                  ,mv_id
                                  )


nodeGraphics     = 0x000001
edgeGraphics     = 0x000002
edgeIntWeight    = 0x000004
edgeDoubleWeight = 0x000008
edgeLabel        = 0x000010
nodeLabel        = 0x000020
edgeType         = 0x000040
nodeType         = 0x000080
nodeId           = 0x000100
edgeArrow        = 0x000200
edgeStyle        = 0x000400
nodeStyle        = 0x000800
nodeTemplate     = 0x001000
edgeSubGraphs    = 0x002000
nodeWeight       = 0x004000
threeD           = 0x010000


defaultSugiyama ga = do
  sl <- newSugiyamaLayout
  or <- newOptimalRanking
  sugiyamaLayout_setRanking sl or
  mh <- newMedianHeuristic
  sugiyamaLayout_setCrossMin sl mh

  ohl <- newOptimalHierarchyLayout
  optimalHierarchyLayout_layerDistance ohl 15.0
  optimalHierarchyLayout_nodeDistance ohl 12.5
  optimalHierarchyLayout_weightBalancing ohl 10.0
  sugiyamaLayout_setLayout sl ohl
  call sl ga
  delete sl



mkOGDFSVG :: FilePath -> MeaningGraph -> IO ()
mkOGDFSVG file mg = do
  putStrLn "mkOGDFSVG"
  g <- newGraph
  ga <- newGraphAttributes g (nodeGraphics
                             .|. edgeGraphics
                             .|. nodeLabel
                             .|. edgeLabel
                             .|. edgeStyle
                             .|. nodeStyle
                             .|. nodeTemplate
                             )

  ivs <- forM (mg^.mg_vertices) $ \v -> do
           n <- graph_newNode g
           str <- graphAttributes_label ga n
           let txt = case v of
                   MGEntity {..}    -> _mv_text
                   MGPredicate {..} -> unFNFrame _mv_frame
               bstr = TE.encodeUtf8 txt
               width = (T.length txt) * 6
           graphAttributes_width ga n >>= \p -> poke p (fromIntegral width)
           graphAttributes_height ga n >>= \p -> poke p 20

           useAsCString bstr $ \cstrnode -> do
             strnode <- newCppString cstrnode
             cppString_append str strnode
           pure (v^.mv_id,n)
  forM_ (mg^.mg_edges) $ \e -> do
    let Just (_,n1) = find (\(i,_) -> i == e^.me_start) ivs
        Just (_,n2) = find (\(i,_) -> i == e^.me_end) ivs
    e' <- graph_newEdge g n1 n2
    str <- graphAttributeslabelE ga e'
    let txt = unFNFrameElement (e^.me_relation) <> maybe "" (":" <>) (e^.me_prep)
        bstr = TE.encodeUtf8 txt
    useAsCString bstr $ \cstredge -> do
      stredge <- newCppString cstredge
      cppString_append str stredge

    pure ()

  -- v^.mv_id
  defaultSugiyama ga

  withCString file $ \cstrsvg -> do
    strsvg <- newCppString cstrsvg
    graphIO_drawSVG ga strsvg
    delete strsvg

  delete g
  delete ga
