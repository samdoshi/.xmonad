{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Flip ( flipLayout
            , Flip
            ) where

import           Control.Arrow                (second)
import           Graphics.X11                 (Rectangle (..))
import           XMonad.Core                  (SomeMessage (..), fromMessage)
import           XMonad.Layout                (Resize (Expand, Shrink))
import           XMonad.Layout.LayoutModifier (LayoutModifier,
                                               ModifiedLayout (..),
                                               handleMessOrMaybeModifyIt,
                                               pureModifier)
import           XMonad.Util.XUtils           (fi)


flipLayout :: l a -> ModifiedLayout Flip l a
flipLayout = ModifiedLayout Flip

reflectRect :: Rectangle -> Rectangle -> Rectangle
reflectRect (Rectangle sx _ sw _) (Rectangle rx ry rw rh) =
  Rectangle (2*sx + fi sw - rx - fi rw) ry rw rh

data Flip a = Flip deriving (Show, Read)

instance LayoutModifier Flip a where
    pureModifier Flip r _ wrs = (map (second $ reflectRect r) wrs, Just Flip)
    handleMessOrMaybeModifyIt Flip m
      | Just Expand <- fromMessage m = pure $ Just $ Right $ SomeMessage Shrink
      | Just Shrink <- fromMessage m = pure $ Just $ Right $ SomeMessage Expand
    handleMessOrMaybeModifyIt _ _ = pure Nothing
