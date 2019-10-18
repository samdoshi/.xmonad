{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Machines ( Machine(..)
                , Conditional
                , onCarbon
                , onCobalt
                ) where

import           Data.Maybe      (fromMaybe)
import           XMonad.Core     (LayoutClass (..))
import qualified XMonad.StackSet as W

data Machine = Carbon
             | Cobalt
             | Unknown
             deriving (Eq, Show, Read)

onCarbon :: (LayoutClass lt a, LayoutClass lf a) => Machine -> lt a -> lf a -> Conditional lt lf a
onCarbon m = Conditional (m == Carbon)

onCobalt :: (LayoutClass lt a, LayoutClass lf a) => Machine -> lt a -> lf a -> Conditional lt lf a
onCobalt m = Conditional (m == Cobalt)

data Conditional lt lf a = Conditional Bool (lt a) (lf a)
                         deriving (Read, Show)

instance (LayoutClass lt a, LayoutClass lf a, Show a) => LayoutClass (Conditional lt lf) a where
    runLayout (W.Workspace i p@(Conditional c lt lf) ms) r =
      if c
        then do (wrs, mlt') <- runLayout (W.Workspace i lt ms) r
                return (wrs, Just $ mkNewConditionalT p mlt')
        else do (wrs, mlt') <- runLayout (W.Workspace i lf ms) r
                return (wrs, Just $ mkNewConditionalF p mlt')

    handleMessage (Conditional bool lt lf) m
        | bool      = handleMessage lt m >>= maybe (return Nothing) (\nt -> return . Just $ Conditional bool nt lf)
        | otherwise = handleMessage lf m >>= maybe (return Nothing) (\nf -> return . Just $ Conditional bool lt nf)

    description (Conditional True  l1 _) = description l1
    description (Conditional _     _ l2) = description l2


mkNewConditionalT :: Conditional l1 l2 a -> Maybe (l1 a) -> Conditional l1 l2 a
mkNewConditionalT (Conditional _ lt lf) mlt' =
  (\lt' -> Conditional True lt' lf) $ fromMaybe lt mlt'

mkNewConditionalF :: Conditional l1 l2 a -> Maybe (l2 a) -> Conditional l1 l2 a
mkNewConditionalF (Conditional _ lt lf) mlf' =
  (\lf' -> Conditional False lt lf') $ fromMaybe lf mlf'
