{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module SWTOR.UIProfile.Placement
( Placement (..), Bounds (..)
, place
) where

import Control.Monad.Writer
import Data.Foldable
import Data.List

import SWTOR.UIProfile.Layout

data Placement = Placement{ placAlign   :: Alignment
                          , placPos     :: (Double, Double)
                          , placBounds  :: Bounds
                          , placElement :: Element
                          }
  deriving (Eq, Ord, Show, Read)

data Bounds = Bounds{ boundsL :: Double
                    , boundsT :: Double
                    , boundsR :: Double
                    , boundsB :: Double
                    }
  deriving (Eq, Ord, Show, Read)

place :: Layout -> (Globals, [Placement])
place Screen{..} = (scrGlobals, execWriter (traverse_ placeScreen scrPrims))

placeScreen :: MonadWriter [Placement] m => LayoutPrim -> m ()
placeScreen ly
  | Just screenAlign <- layoutPrimParentAlignment ly =
      placePrim screenAlign (Bounds 0 0 0 0) ly
  | otherwise = pure ()  -- No elements inside.

placePrim :: MonadWriter [Placement] m => Alignment -> Bounds -> LayoutPrim -> m ()
placePrim screenAlign parentBounds prim =
  case prim of
    Anchor{..} -> do
      let thisBounds = connect (lyParentAlign, parentBounds) (lyThisAlign, lyElement)
          plac = Placement { placAlign   = screenAlign
                           , placPos     = boundsAnchor screenAlign thisBounds
                           , placBounds  = thisBounds
                           , placElement = lyElement
                           }
      tell [plac]
      traverse_ (placePrim screenAlign thisBounds) lyChildren

    Box{..} -> do
      (_, placements) <- listen $ traverse_ (placePrim screenAlign parentBounds) lyInside
      let bounds = case placements of
                     [] -> parentBounds  -- If nothing inside, use the parent bounds.
                     _  -> (foldl1' unionBounds . map placBounds) placements
      traverse_ (placePrim screenAlign bounds) lyChildren

unionBounds :: Bounds -> Bounds -> Bounds
unionBounds (Bounds la ta ra ba) (Bounds lb tb rb bb) =
  Bounds (min la lb) (min ta tb) (max ra rb) (max ba bb)

connect :: (Alignment, Bounds) -> (Alignment, Element) -> Bounds
connect (parentAlign, parent) (childAlign, child) =
  Bounds x y (x + w) (y + h)
  where
    (x, y) = (xParent + xOff + xAlign, yParent + yOff + yAlign)
    (w, h) = elementSize child
    (xParent, yParent) = boundsAnchor parentAlign parent
    (xOff, yOff) = elemOffset child
    (xAlign, yAlign) =
      case childAlign of
        TL -> (0, 0)
        BL -> (0, -h)
        L  -> (0, -0.5 * h)
        TR -> (-w, 0)
        BR -> (-w, -h)
        R  -> (-w, -0.5 * h)
        T  -> (-0.5 * w, 0)
        B  -> (-0.5 * w, -h)
        C  -> (-0.5 * w, -0.5 * h)

boundsAnchor :: Alignment -> Bounds -> (Double, Double)
boundsAnchor al Bounds{..} =
  case al of
    TL -> (boundsL, boundsT)
    BL -> (boundsL, boundsB)
    L  -> (boundsL, 0.5 * (boundsT + boundsB))
    TR -> (boundsR, boundsT)
    BR -> (boundsR, boundsB)
    R  -> (boundsR, 0.5 * (boundsT + boundsB))
    T  -> (0.5 * (boundsL + boundsR), boundsT)
    B  -> (0.5 * (boundsL + boundsR), boundsB)
    C  -> (0.5 * (boundsL + boundsR), 0.5 * (boundsT + boundsB))
