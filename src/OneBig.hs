{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OneBig ( OneBig (..)
              ) where

import           Graphics.X11.Xlib.Types (Dimension, Position,
                                          Rectangle (Rectangle), rect_height,
                                          rect_width)
import           XMonad.Core             (LayoutClass (pureLayout, pureMessage),
                                          SomeMessage, fromMessage)
import           XMonad.Layout           (Resize (Expand, Shrink))
import qualified XMonad.StackSet         as W

data OneBig a = OneBig Rational Rational
              deriving (Read, Show)

instance LayoutClass OneBig a where
  pureLayout = oneBigLayout
  pureMessage = oneBigMessage

-- | Processes Shrink/Expand messages
oneBigMessage :: OneBig a -> SomeMessage -> Maybe (OneBig a)
oneBigMessage (OneBig cx cy) m = fmap resize (fromMessage m)
  where resize Shrink = limitResize (cx - delta) (cy - delta)
        resize Expand = limitResize (cx + delta) (cy + delta)
        delta = 1/100
        limitResize x y | x <= 0 || y <= 0 = OneBig cx cy
                        | x >= 1 || y >= 1 = OneBig cx cy
                        | otherwise        = OneBig  x  y

-- | Main layout function
oneBigLayout :: OneBig a -> Rectangle -> W.Stack a -> [(a, Rectangle)]
oneBigLayout (OneBig cx cy) rect stack = [(masterWindow, masterRect)]
                                         ++ divideRight rightRect rightWindows
                                         ++ divideBottom bottomRect bottomWindows
  where windowStack = W.integrate stack
        numWindows = length windowStack
        masterWidth = round $ fromIntegral (rect_width rect) * cx
        windowHeight = rect_height rect
        numRightWindows = calcRightWindows numWindows masterWidth windowHeight
        masterWindow = head windowStack
        otherWindows  = tail windowStack
        rightWindows = take numRightWindows otherWindows
        bottomWindows = drop numRightWindows otherWindows
        masterRect = cmaster numWindows numRightWindows cx cy rect
        bottomRect = cbottom cx cy rect
        rightRect  = cright cx cy rect

-- | Calculate how many windows must be placed at bottom
calcRightWindows :: Int        -- ^ Total no of windows
                 -> Dimension  -- ^ Width of bottom window
                 -> Dimension  -- ^ Height of bottom window
                 -> Int        -- ^ No of windows at the bottom
calcRightWindows 1 _ _ = 0
calcRightWindows 2 _ _ = 1
calcRightWindows 3 _ _ = 1
calcRightWindows 4 _ _ = 2
calcRightWindows n w h = fromIntegral w*(n-1) `div` fromIntegral (h+fromIntegral w)

-- | Calculate rectangle for master window
cmaster :: Int -> Int -> Rational -> Rational -> Rectangle -> Rectangle
cmaster n m cx cy (Rectangle x y sw sh) = Rectangle x y w h
  where w = if n > 1 then
              round (fromIntegral sw * cx)
            else
              sw
        h = if n > m + 1 then
              round (fromIntegral sh * cy)
            else
              sh

-- | Calculate rectangle for bottom windows
cbottom :: Rational -> Rational -> Rectangle -> Rectangle
cbottom cx cy (Rectangle sx sy sw sh) = Rectangle sx y w h
  where w = round (fromIntegral sw * cx)
        h = round (fromIntegral sh * (1 - cy))
        y = round (fromIntegral sh * cy + fromIntegral sy)

-- | Calculate rectangle for right windows
cright :: Rational -> Rational -> Rectangle -> Rectangle
cright cx _ (Rectangle sx sy sw sh) = Rectangle x sy w sh
  where w = round (fromIntegral sw * (1 - cx))
        x = round (fromIntegral sw * cx + fromIntegral sx)

-- | Divide bottom rectangle between windows
divideBottom :: Rectangle -> [a] -> [(a, Rectangle)]
divideBottom (Rectangle x y w h) ws = zip ws rects
  where n = length ws
        oneW = fromIntegral w `div` n
        oneRect = Rectangle x y (fromIntegral oneW) h
        rects = take n $ iterate (shiftR (fromIntegral oneW)) oneRect

-- | Divide right rectangle between windows
divideRight :: Rectangle -> [a] -> [(a, Rectangle)]
divideRight (Rectangle x y w h) ws = if n == 0 then [] else zip ws rects
  where n = length ws
        oneH = fromIntegral h `div` n
        oneRect = Rectangle x y w (fromIntegral oneH)
        rects = take n $ iterate (shiftB (fromIntegral oneH)) oneRect

-- | Shift rectangle right
shiftR :: Position -> Rectangle -> Rectangle
shiftR s (Rectangle x y w h) = Rectangle (x + s) y w h

-- | Shift rectangle bottom
shiftB :: Position -> Rectangle -> Rectangle
shiftB s (Rectangle x y w h) = Rectangle x (y + s) w h
