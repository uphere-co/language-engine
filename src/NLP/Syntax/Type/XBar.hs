{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE StandaloneDeriving #-}

module NLP.Syntax.Type.XBar where


data X = V | T | C

type family Property x tag :: *
 
type family Specifier x tag :: *
type family Complement x tag :: *
type family Adjunct x tag :: *

type family Zipper x tag :: *

data XP x tag = XP { _headX             :: Property x tag
                   , _maximalProjection :: Zipper x tag
                   , _specifier         :: Specifier x tag
                   , _adjunct           :: Adjunct x tag
                   , _complement        :: Complement x tag
                   }


         
