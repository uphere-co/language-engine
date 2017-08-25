{-# LANGUAGE TemplateHaskell #-}

module OntoNotes.App.Analyze.Type where

import           Control.Lens      ((^.),_2,makeLenses)
import           Data.Function     (on)
import           Data.List         (maximumBy)
import           Data.Text         (Text)
--
import           Lexicon.Type      (SenseID)
--


data ExceptionalFrame = FrameCopula  | FrameIdiom | FrameLightVerb | FrameNone
                      deriving (Show,Eq,Ord)


data TextifiedFrame = TF { _tf_frameID :: Text
                         , _tf_feCore :: [Text]
                         , _tf_fePeri :: [Text]
                         }
                    deriving (Show,Eq,Ord)

makeLenses ''TextifiedFrame                             

                           
data ONSenseFrameNetInstance = ONFNInstance { _onfn_senseID :: SenseID
                                            , _onfn_definition :: Text
                                            , _onfn_frame :: Either ExceptionalFrame TextifiedFrame
                                            }
                             deriving (Show,Eq,Ord)


makeLenses ''ONSenseFrameNetInstance                                      



chooseFrame :: [(ONSenseFrameNetInstance,Int)] -> Maybe (ONSenseFrameNetInstance,Int)
chooseFrame [] = Nothing
chooseFrame xs = Just (maximumBy (compare `on` (^._2)) xs)

