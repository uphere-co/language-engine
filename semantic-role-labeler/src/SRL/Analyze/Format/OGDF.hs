{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-name-shadowing -fno-warn-unused-matches -fno-warn-unused-top-binds #-}

module SRL.Analyze.Format.OGDF (
    mkOGDFSVG
  , example
  , example2
  ) where

import           Control.Lens ((^.))
import           Control.Monad (void, when)
import           Control.Monad.Loops (iterateUntilM)
import           Data.Bits ((.|.))
import           Data.ByteString.Char8 (useAsCString)
import           Data.Foldable (forM_)
import           Data.List (find)
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
import           Lexicon.Type (FNFrame(..))
import           SRL.Analyze.Type (MeaningGraph
                                  ,MGVertex(..)
                                  ,mg_vertices,mg_edges
                                  ,me_start,me_end,mv_id
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
  sugiyamaLayoutsetRanking sl or
  mh <- newMedianHeuristic
  sugiyamaLayoutsetCrossMin sl mh

  ohl <- newOptimalHierarchyLayout
  optimalHierarchyLayoutlayerDistance ohl 15.0
  optimalHierarchyLayoutnodeDistance ohl 12.5
  optimalHierarchyLayoutweightBalancing ohl 10.0
  sugiyamaLayoutsetLayout sl ohl
  call sl ga
  delete sl



mkOGDFSVG :: MeaningGraph -> IO ()
mkOGDFSVG mg = do
  putStrLn "mkOGDFSVG"
  g <- newGraph
  ga <- newGraphAttributes g (nodeGraphics
                             .|. edgeGraphics
                             .|. nodeLabel
                             .|. edgeStyle
                             .|. nodeStyle
                             .|. nodeTemplate
                             )

  ivs <- forM (mg^.mg_vertices) $ \v -> do
           n <- graphnewNode g
           str <- graphAttributeslabel ga n
           let txt = case v of
                   MGEntity {..}    -> _mv_text
                   MGPredicate {..} -> unFNFrame _mv_frame
               bstr = TE.encodeUtf8 txt
           useAsCString bstr $ \cstrnode -> do
             strnode <- newCppString cstrnode
             cppStringappend str strnode
           pure (v^.mv_id,n)
  forM_ (mg^.mg_edges) $ \e -> do
    let Just (_,n1) = find (\(i,_) -> i == e^.me_start) ivs
        Just (_,n2) = find (\(i,_) -> i == e^.me_end) ivs
    graphnewEdge g n1 n2
    pure ()

  -- v^.mv_id
  defaultSugiyama ga

  withCString "test_mg.svg" $ \cstrsvg -> do
    strsvg <- newCppString cstrsvg
    graphIOdrawSVG ga strsvg
    delete strsvg

  delete g
  delete ga


example :: IO ()
example = do
  let len = 11

  g <- newGraph
  ga <- newGraphAttributes g (nodeGraphics .|. edgeGraphics)

  forM_ [1 .. len-1] $ \i -> do
    left <- graphnewNode g
    p_x1 <- graphAttributesx ga left
    poke p_x1 (fromIntegral (-5*(i+1)))
    p_y1 <- graphAttributesy ga left
    poke p_y1 (fromIntegral (-20*i))
    p_width1 <- graphAttributeswidth ga left
    poke p_width1 (fromIntegral (10*(i+1)))
    p_height1 <- graphAttributesheight ga left
    poke p_height1 15

    bottom <- graphnewNode g
    p_x2 <- graphAttributesx ga bottom
    poke p_x2 (fromIntegral (20*(len-i)))
    p_y2 <- graphAttributesy ga bottom
    poke p_y2 (fromIntegral (5*(len+1-i)))
    p_width2 <- graphAttributeswidth ga bottom
    poke p_width2 15
    p_height2 <- graphAttributesheight ga bottom
    poke p_height2 (fromIntegral (10*(len+1-i)))

    e <- graphnewEdge g left bottom

    poly <- graphAttributesbends ga e
    pt1 <- newDPoint 10 (fromIntegral (-20*i))
    -- pt15 <- newDPoint 0 0
    _pt2 <- newDPoint (fromIntegral (20*(len-i))) (-10)
    dPolylinepushBack poly pt1
    --dPolylinepushBack poly pt15
    -- dPolylinepushBack poly pt2

    pure ()


  withCString "manual_graph_test.gml" $ \cstr -> do
    str <- newCppString cstr
    graphIOwriteGML ga str
    delete str


  n0@(NodeElement n0') <- graphfirstNode g

  when (n0' /= nullPtr) $ void $
    flip (iterateUntilM (\(NodeElement n'') -> n'' == nullPtr)) n0 $ \n -> do
      i <- nodeElementindex n
      x <- peek =<< graphAttributesx      ga n
      y <- peek =<< graphAttributesy      ga n
      w <- peek =<< graphAttributeswidth  ga n
      h <- peek =<< graphAttributesheight ga n
      let int = F.left 3 ' ' %. F.int
          dbl = F.left 6 ' ' %. F.float
          txt = F.sformat (int % " " % dbl % " " % dbl % " " % dbl % " " % dbl)
                  (fromIntegral i :: Int)
                  (realToFrac x :: Double)
                  (realToFrac y :: Double)
                  (realToFrac w :: Double)
                  (realToFrac h :: Double)
      TIO.putStrLn txt
      nodeElementsucc n

  withCString "test.svg" $ \cstr -> do
    str <- newCppString cstr
    graphIOdrawSVG ga str
    delete str

  delete ga
  delete g
  pure ()


example2 :: IO ()
example2 = do
  g <- newGraph
  ga <- newGraphAttributes g (   nodeGraphics
                             .|. edgeGraphics
                             .|. nodeLabel
                             .|. edgeStyle
                             .|. nodeStyle
                             .|. nodeTemplate )

  withCString "unix-history.gml" $ \cstr -> do
    str <- newCppString cstr
    b <- graphIOreadGML ga g str

    if (b == 0)
      then hPutStrLn stderr "Could not load unix-history.gml"
      else do
        sl <- newSugiyamaLayout
        putStrLn "sl created"
        or <- newOptimalRanking
        putStrLn "or created"
        sugiyamaLayoutsetRanking sl or
        putStrLn "setRanking done"
        mh <- newMedianHeuristic
        putStrLn "mh created"
        sugiyamaLayoutsetCrossMin sl mh
        putStrLn "setCrossMin"

        ohl <- newOptimalHierarchyLayout
        putStrLn "ohl created"
        optimalHierarchyLayoutlayerDistance ohl 15.0
        optimalHierarchyLayoutnodeDistance ohl 12.5
        optimalHierarchyLayoutweightBalancing ohl 10.0
        sugiyamaLayoutsetLayout sl ohl
        putStrLn "setLayout ohl"
        call sl ga
        putStrLn "SL.call(GA)"
        delete sl
        pure ()

        withCString "test2.gml" $ \cstr -> do
          str <- newCppString cstr
          graphIOwriteGML ga str
          delete str


        withCString "test2.svg" $ \cstrsvg -> do
          strsvg <- newCppString cstrsvg
          graphIOdrawSVG ga strsvg
          delete strsvg

  delete g
  delete ga
