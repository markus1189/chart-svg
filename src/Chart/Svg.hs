{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Svg
  ( ViewBox(..)
  , aspect
  , ratio
  , ChartSvg(..)
  , treeRect
  , treeGlyph
  , treeLine
  , treeShape
  , treeText
  , tree
  , projectSpots
  , projectSpotsWith
  , chartSvg_
  , chartSvg
  , chartSvgWith
  , fittedSvg
  , pad
  , frame
  , defaultFrame
  , renderXml
  , renderXmlWith
  , xmlToText
  , write
  , writeWith
  , rotateDA
  , translateDA
  , rotate
  , translate
  , ScratchStyle(ScratchStyle)
  , defaultScratchStyle
  , clearScratchStyle
  , scratchSvgWith
  , scratch
  , scratchWith
  , scratchSvg
  , ChartSvgStyle(ChartSvgStyle)
  , defaultChartSvgStyle
  , renderChartSvg
  ) where

import Chart.Core
import Chart.Numeric
import Codec.Picture.Types
import Graphics.Svg as Svg hiding (Point)
import Graphics.Svg.CssTypes as Svg hiding (Point)
import Control.Lens hiding (transform)
import Linear.V2
import NumHask.Prelude as P hiding (Group, rotate, Element)
import Text.XML.Light.Output
import qualified Data.Map as Map
import qualified Data.Text as Text

-- * Svg
-- | An Svg ViewBox
newtype ViewBox a = ViewBox
  { vbArea :: Rect a
  } deriving (Show, Eq, Semigroup, Functor)

-- | convert a ratio of x-plane : y-plane to a ViewBox with a height of one.
aspect :: (FromRatio a, Multiplicative a) => a -> ViewBox a
aspect a = ViewBox $ Rect (a * (-0.5)) (a * 0.5) (-0.5) 0.5

-- | convert a ViewBox to a ratio
ratio :: (Divisive a, Subtractive a) => ViewBox a -> a
ratio (ViewBox (Rect x z y w)) = (z-x)/(w-y)

-- | Svg of a Chart consists of
-- An Svg `Tree` list and a ViewBox
data ChartSvg a = ChartSvg
  { vbox :: ViewBox a
  , chartTrees :: [Tree]
  } deriving (Eq, Show)

instance (BoundedLattice a) => Semigroup (ChartSvg a) where
  (ChartSvg a b) <> (ChartSvg a' b') = ChartSvg (a<>a') (b<>b')

instance (Chartable a) => Monoid (ChartSvg a) where
  mempty = ChartSvg (ViewBox (toRect one)) mempty

-- * svg primitives
-- | Rectange svg
treeRect :: (ToRatio a) => Rect a -> Tree
treeRect a =
  RectangleTree $
  rectUpperLeftCorner .~ (Num x, Num (-w)) $
  rectWidth .~ Num (z-x) $
  rectHeight .~ Num (w-y) $
  defaultSvg
  where
    (Rect x z y w) = fromRational <$> a

-- | Text svg
treeText :: (ToRatio a) => P.Text -> Pair a -> Tree
treeText t p =
  TextTree Nothing (textAt (Num x, Num (-y)) t)
  where
    (Pair x y) = fromRational <$> p

-- | Text svg with rotation
treeTextRotate :: (ToRatio a) => P.Text -> a -> Pair a -> Tree
treeTextRotate t rot p =
  TextTree Nothing (textAt (Num x, Num (-y)) t) &
  drawAttr .~ rotatePDA rot p
  where
    (Pair x y) = fromRational <$> p

-- | GlyphShape to svg primitive
treeShape :: GlyphShape -> Double -> Pair Double -> Tree
treeShape CircleGlyph s p =
  CircleTree $ Circle mempty (Num x, Num (-y)) (Num (fromRational s/2))
  where
    (Pair x y) = fromRational <$> p
treeShape SquareGlyph s p = treeRect (translateRect p ((s*) <$> toRect one))
treeShape (RectSharpGlyph x') s p =
  treeRect (translateRect p (toRect $ scale (Pair s (x'*s)) one))
treeShape (RectRoundedGlyph x'' rx ry) s p  =
  RectangleTree $
  rectUpperLeftCorner .~ (Num (x+x'), Num (-(w+y'))) $
  rectWidth .~ Num (fromRational z-fromRational x) $
  rectHeight .~ Num (fromRational w-fromRational y) $
  rectCornerRadius .~ (Num (fromRational rx), Num (fromRational ry)) $
  defaultSvg
  where
    (Area x z y w) = fromRational <$> scale (Pair s (x''*s)) one
    (Pair x' y') = fromRational <$> p
treeShape (TriangleGlyph (Pair xa ya) (Pair xb yb) (Pair xc yc)) s p  =
  PolygonTree $
  polygonPoints .~ rps $
  drawAttr . transform .~ Just [Translate x' (-y')] $
  defaultSvg
  where
    rps =
      [ V2 (s*xa) (-s*ya)
      , V2 (s*xb) (-s*yb)
      , V2 (s*xc) (-s*yc)
      ]
    (Pair x' y') = fromRational <$> p
treeShape (EllipseGlyph x') s (Pair x y) =
  EllipseTree $ Ellipse mempty (Num (fromRational x), Num (-(fromRational y)))
  (Num $ fromRational s/two) (Num $ (fromRational x'*fromRational s)/two)
treeShape (VLineGlyph x') s (Pair x y) =
  LineTree $ Line (mempty & strokeWidth .~ Last (Just (Num (fromRational x'))))
  (Num (fromRational x), Num (fromRational $ y - s/two))
  (Num (fromRational x), Num (fromRational $ y + s/two))
treeShape (HLineGlyph x') s (Pair x y) =
  LineTree $ Line (mempty & strokeWidth .~ Last (Just (Num $ fromRational x')))
  (Num (fromRational $ x - s/two), Num $ fromRational y)
  (Num (fromRational $ x + s/two), Num (fromRational y))
treeShape SmileyGlyph s' p =
  groupTrees mempty
  [ CircleTree
    $ defaultSvg
    & drawAttr . fillColor .~ (Last $ Just $ ColorRef $ PixelRGBA8 255 255 0 255)
    & circleCenter .~ (Num (0.5 * fromRational s), Num (0.5 * fromRational s))
    & circleRadius .~ Num (0.5 * fromRational s)
  , EllipseTree
    $ defaultSvg
    & drawAttr . fillColor .~ (Last $ Just $ ColorRef $ PixelRGBA8 255 255 255 255)
    & ellipseCenter .~ (Num (0.35 * fromRational s), Num (0.3 * fromRational s))
    & ellipseXRadius .~ Num (0.05 * fromRational s)
    & ellipseYRadius .~ Num (0.1 * fromRational s)
  , EllipseTree
    $ defaultSvg
    & drawAttr . fillColor .~ (Last $ Just $ ColorRef $ PixelRGBA8 255 255 255 255)
    & ellipseCenter .~ (Num (0.65 * fromRational s), Num (0.3 * fromRational s))
    & ellipseXRadius .~ Num (0.05 * fromRational s)
    & ellipseYRadius .~ Num (0.1 * fromRational s)
  , GroupTree $
    defaultSvg &
    groupChildren .~
    [ PathTree $
      defaultSvg
      & pathDefinition .~
      [ MoveTo OriginAbsolute [V2 0 0]
      , EllipticalArc OriginAbsolute [(0.38*s,0.4*s,0.1*s,False,False, V2 (0.65*s) 0)]
      ]
      & drawAttr . fillColor .~ (Last $ Just FillNone)
      & drawAttr . strokeColor .~ (Last $ Just $ ColorRef $
                                   PixelRGBA8 0 0 0 255)
      & drawAttr . strokeWidth .~ Last (Just (Num (fromRational s * 0.03)))
      & drawAttr . transform .~ Just [Translate (0.18*s) (0.65*s)]
    ]
  ]
  & drawAttr . transform .~ Just [Translate (x - s/2) (y - s/2)]
  where
    s = fromRational s'
    (Pair x y) = fromRational <$> p

-- | GlyphStyle to svg primitive
treeGlyph :: GlyphStyle -> Pair Double -> Tree
treeGlyph s =
  treeShape (s ^. #shape) (s ^. #size)

-- | line svg
treeLine :: (Chartable a) => [Pair a] -> Tree
treeLine xs =
  PolyLineTree $
  polyLinePoints .~ ((\(Pair x y) -> V2 x (-y)) . fmap fromRational <$> xs) $
  defaultSvg

-- | convert a Chart to svg
tree :: (Chartable a) => Chart a -> Tree
tree (Chart (TextA s ts) xs) =
  groupTrees (daText s) (zipWith treeText ts (toPair <$> xs))
tree (Chart (GlyphA s) xs) =
  groupTrees (daGlyph s) (treeGlyph s <$> (toPair . fmap fromRational <$> xs))
tree (Chart (LineA s) xs) =
  groupTrees (daLine s) [treeLine (toPair <$> xs)]
tree (Chart (RectA s) xs) =
  groupTrees (daRect s) (treeRect <$> (toRect <$> xs))

-- | add drawing attributes as a group svg wrapping a [Tree]
groupTrees :: DrawAttributes -> [Tree] -> Tree
groupTrees da' tree' =
  defaultSvg &
  drawAttr %~ (da'<>) &
  groupChildren .~ tree' &
  GroupTree

-- | create a ChartSvg from a [Chart] and a ViewBox
chartSvg_ :: (Chartable a) =>
  ViewBox a -> [Chart a] -> ChartSvg a
chartSvg_ (ViewBox a) cs = ChartSvg
  (ViewBox a)
  (tree <$> cs)

projectSpots :: (Chartable a) => Rect a -> [Chart a] -> [Chart a]
projectSpots a cs = cs'
  where
    xss = projectTo2 a (spots <$> cs)
    ss = annotation <$> cs
    cs' = zipWith Chart ss xss

projectSpotsWith :: (Chartable a) => Rect a -> Rect a -> [Chart a] -> [Chart a]
projectSpotsWith new old cs = cs'
  where
    xss = fmap (projectOn new old) <$> spots <$> cs
    ss = annotation <$> cs
    cs' = zipWith Chart ss xss

-- | convert a [Chart] to a ChartSvg, projecting Chart data to the supplied ViewBox, and expanding the ViewBox for chart style if necessary
chartSvg :: (Chartable a) =>
  ViewBox a -> [Chart a] -> ChartSvg a
chartSvg (ViewBox a) cs = chartSvg_ (ViewBox $ styleBoxes cs') cs'
  where
    cs' = projectSpots a cs

-- | convert a [Chart] to a ChartSvg, projecting Chart data from a specified Rect to the supplied ViewBox, and expanding the ViewBox for chart style if necessary
chartSvgWith :: (Chartable a) =>
  ViewBox a -> Rect a -> [Chart a] -> ChartSvg a
chartSvgWith (ViewBox new) old cs = chartSvg_ (ViewBox $ new <> styleBoxes cs') cs'
  where
    cs' = projectSpotsWith new old cs

-- | convert a [Chart] to a ChartSvg, setting the ViewBox equal to the Chart data area
fittedSvg :: (Chartable a) =>
  [Chart a] -> ChartSvg a
fittedSvg cs =
  chartSvg (ViewBox $ styleBoxes cs) cs

-- | widen a ChartSvg ViewBox by a fraction of the size.
pad :: (Chartable a) => a -> ChartSvg a -> ChartSvg a
pad p (ChartSvg (ViewBox vb) s) = ChartSvg (ViewBox (widenProp p vb)) s

-- | add an enclosing fitted frame to a ChartSvg
frame :: (Chartable a) => RectStyle -> ChartSvg a -> ChartSvg a
frame o (ChartSvg (ViewBox vb) _) =
  ChartSvg
  (ViewBox vb)
  ((:[]) . tree $ Chart (RectA o) [Area' vb])

-- | a default frame
defaultFrame :: (Chartable a) => ChartSvg a -> ChartSvg a
defaultFrame ch = frame (border 0.01 blue 1.0) ch <> ch

-- | render a ChartSvg to an xml Document with the supplied size and various bits and pieces
renderXmlWith :: (ToRatio a) => Pair a -> Map.Map Text.Text Element -> Text.Text -> [CssRule] -> FilePath -> ChartSvg a -> Document
renderXmlWith (Pair wid hei) defs desc css fp (ChartSvg (ViewBox vb) ts) =
  Document
  ((\(Rect x z y w) -> Just (x,-w,z-x,w-y)) $ fromRational <$> vb)
  (Just (Num (fromRational wid)))
  (Just (Num (fromRational hei)))
  ts (Map.mapKeys Text.unpack defs) (Text.unpack desc) css fp

-- | render a ChartSvg to an xml Document with the supplied size
renderXml :: (ToRatio a) => Pair a -> ChartSvg a -> Document
renderXml p = renderXmlWith p Map.empty "" [] ""

-- | render an xml document to Text
xmlToText :: Document -> P.Text
xmlToText = Text.pack . ppcElement defaultConfigPP . xmlOfDocument

-- | write a ChartSvg to a svg file with various Document attributes.
writeWith :: (ToRatio a) => FilePath -> Pair a -> Map.Map Text.Text Element -> Text.Text -> [CssRule] -> ChartSvg a -> IO ()
writeWith fp p defs desc css c = saveXmlFile fp (renderXmlWith p defs desc css fp c)

-- | write a ChartSvg to an svg file.
write :: (ToRatio a) => FilePath -> Pair a -> ChartSvg a -> IO ()
write fp p = writeWith fp p Map.empty "" []

-- * transformations
-- | A DrawAttributes to rotate by x degrees.
rotateDA :: (ToRatio a) => a -> DrawAttributes
rotateDA r = mempty & transform .~ Just [Rotate (fromRational r) Nothing]

-- | A DrawAttributes to rotate around a point by x degrees.
rotatePDA :: (ToRatio a) => a -> Pair a -> DrawAttributes
rotatePDA r p = mempty & transform .~ Just [Rotate (fromRational r) (Just (x,-y))]
  where
    (Pair x y) = fromRational <$> p

-- | A DrawAttributes to translate by a Point.
translateDA :: (ToRatio a) => Pair a -> DrawAttributes
translateDA (Pair x y) = mempty & transform .~ Just
  [Translate (fromRational x) (-fromRational y)]

-- | Rotate a ChartSvg expanding the ViewBox as necessary.
-- Multiple rotations will expand the bounding box conservatively.
rotate :: (Chartable a, TrigField a) => a -> ChartSvg a -> ChartSvg a
rotate r (ChartSvg (ViewBox vb) c) = 
  ChartSvg
  (ViewBox $ rotateRect r vb)
  [groupTrees (rotateDA r) c]

-- | Translate a ChartSvg also moving the ViewBox
translate :: (Additive a, ToRatio a) => Pair a -> ChartSvg a -> ChartSvg a
translate p (ChartSvg (ViewBox vb) c) = 
  ChartSvg
  (ViewBox $ translateRect p vb)
  [groupTrees (translateDA p) c]

-- * development helpers

data ScratchStyle = ScratchStyle
  { fileName :: FilePath
  , size :: Double
  , ratioAspect :: Double
  , outerPad :: Double
  , innerPad :: Double
  , frame' :: ChartSvg Double -> ChartSvg Double
  , maybeOrig :: Maybe (Double, Color)
  } deriving (Generic)

defaultScratchStyle :: ScratchStyle
defaultScratchStyle  = ScratchStyle "other/scratchpad.svg" 200 1.5 1.05 1.05 defaultFrame (Just (0.04, red))

clearScratchStyle :: ScratchStyle
clearScratchStyle  = ScratchStyle "other/scratchpad.svg" 400 1 1 1 defaultFrame Nothing

scratchSvgWith :: ScratchStyle -> [ChartSvg Double] -> IO ()
scratchSvgWith s x =
  write
  (s ^. #fileName)
  (Pair (s ^. #ratioAspect * s ^. #size) (s ^. #size)) $
  pad (s ^. #outerPad) $
  (s ^. #frame') $
  pad (s ^. #innerPad) $
  mconcat x

scratchSvg :: [ChartSvg Double] -> IO ()
scratchSvg = scratchSvgWith defaultScratchStyle

scratch :: [Chart Double] -> IO ()
scratch = scratchWith defaultScratchStyle

scratchWith :: ScratchStyle -> [Chart Double] -> IO ()
scratchWith s x =
  write
  (s ^. #fileName)
  (Pair (s ^. #ratioAspect * s ^. #size) (s ^. #size)) $
  pad (s ^. #outerPad) $
  (s ^. #frame') $
  pad (s ^. #innerPad) $
  chartSvg
  (aspect (s ^. #ratioAspect))
  (orig' <> x) where
  orig' = case s ^. #maybeOrig of
    Nothing -> mempty
    Just (n,c) -> [showOriginWith n c]

data ChartSvgStyle = ChartSvgStyle
  { sizex :: Double
  , sizey :: Double
  , chartAspect :: Double
  , outerPad :: Maybe Double
  , innerPad :: Maybe Double
  , chartFrame :: Maybe RectStyle
  , orig :: Maybe (Double, Color)
  } deriving (Generic)

defaultChartSvgStyle :: ChartSvgStyle
defaultChartSvgStyle = ChartSvgStyle 600 400 1.5 (Just 1.05) (Just 1.05) (Just $ border 0.01 blue 1.0) (Just (0.04, red))

renderChartSvg :: Double -> Double -> ChartSvg Double -> Text.Text
renderChartSvg x y =
  xmlToText . renderXml (Pair x y)
