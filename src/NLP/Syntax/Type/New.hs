{-# LANGUAGE TypeFamilies #-}

module NLP.Syntax.Type.New where

import           NLP.Syntax.Type
import           NLP.Syntax.Type.XBar

type instance Property   X_V t = VerbProperty (Zipper t)

type instance Specifier  X_V t = ()
type instance Adjunct    X_V t = ()
type instance Complement X_V t = [Zipper t]

type VP = XP X_V

type instance Property   X_T t = ()

type instance Specifier  X_T t = Maybe (ATNode (DP (Zipper t)))
type instance Adjunct    X_T t = ()
type instance Complement X_T t = VP t

type TP = XP X_T

type instance Property   X_C t = Maybe (Zipper t)   -- complementizer

type instance Specifier  X_C t = ()
type instance Adjunct    X_C t = ()
type instance Complement X_C t = TP t

type CP = XP X_C

