{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

module Chart.Core
  ( Chart(..)
  , Chartable
  , Annotation(..)
  , annotationText
  , DrawAttributes(..)
  , RectStyle(..)
  , defaultRectStyle
  , blob
  , clear
  , border
  , TextStyle(..)
  , defaultTextStyle
  , AlignH(..)
  , textToAlignH
  , GlyphStyle(..)
  , defaultGlyphStyle
  , GlyphShape(..)
  , toGlyphShape
  , fromGlyphShape
  , toGlyph
  , fromGlyph
  , LineStyle(..)
  , defaultLineStyle
  , styleBoxText
  , styleBoxGlyph
  , styleBoxDA
  , daRect
  , daText
  , daGlyph
  , daLine
  , styleBox
  , styleBoxes
  , projectWithStyle
  , projectWithStyles
  , showOriginWith
  , Color(..)
  , pattern Color
  , blue
  , grey
  , red
  , black
  , white
  , blend
  , pixelate
  , placedLabel
  ) where

import Chart.Numeric
import Codec.Picture.Types
import Control.Exception
import Control.Lens hiding (transform, (.=))
import Data.Aeson
import Data.Generics.Labels ()
import Graphics.Svg as Svg hiding (Point, Text)
import NumHask.Prelude as P hiding (Group)
import qualified Data.Text as Text
import Lucid

-- * Chart
-- | A `Chart` consists of
-- - a list of spots on the xy-plane
-- - svg draw attributes for the chart
-- - specific style of representation for each spot (an Annotation)
data Chart a = Chart
  { annotation :: Annotation
  , spots :: [Spot a]
  } deriving (Eq, Show, Generic)

-- | the aspects a number needs to be to form the data for a chart
type Chartable a =
  ( ToRatio a
  , FromRatio a
  , FromInteger a
  , Subtractive a
  , Field a
  , BoundedJoinSemiLattice a
  , BoundedMeetSemiLattice a)

-- | a piece of chart structure
-- | The use of #rowName with these Annotation collection doesn't seem to mesh well with polymorphism, so a switch to concrete types (which fit it with svg-tree methods) occurs at this layer, and the underlying data structure is a lot of Doubles
data Annotation
  = RectA RectStyle
  | TextA TextStyle [Text.Text]
  | GlyphA GlyphStyle
  | LineA LineStyle
  deriving (Eq, Show, Generic)

instance ToJSON Annotation
instance FromJSON Annotation

annotationText :: Annotation -> Text
annotationText (RectA _) = "RectA"
annotationText TextA{} = "TextA"
annotationText (GlyphA _) = "GlyphA"
annotationText (LineA _) = "LineA"

-- | Rectangle styling
data RectStyle = RectStyle
  { borderSize :: Double
  , borderColor :: Color
  , borderOpacity :: Double
  , color :: Color
  , opacity :: Double
  , rotation :: Maybe (Double, Pair Double)
  , translation :: Maybe (Pair Double)
  } deriving (Show, Eq, Generic)

instance ToJSON RectStyle
instance FromJSON RectStyle

-- | the official style
defaultRectStyle :: RectStyle
defaultRectStyle = RectStyle 0.005 grey 0.5 blue 0.5 Nothing Nothing

daRect :: RectStyle -> DrawAttributes
daRect o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (fromRational $ o ^. #borderSize))) .
  (strokeColor .~ Last (Just $ ColorRef (promotePixel $ uncolor $ o ^. #borderColor))) .
  (strokeOpacity .~ Just (fromRational $ o ^. #borderOpacity)) .
  (fillColor .~ Last (Just $ ColorRef (promotePixel $ uncolor $ o ^. #color))) .
  (fillOpacity .~ Just (fromRational $ o ^. #opacity)) .
  maybe identity
    (\(r, Pair x y) -> transform %~ (<> Just [Rotate r (Just (x,y))])) (o ^. #rotation) .
  maybe identity (\(Pair x y) -> transform %~ (<> Just [Translate x (-y)])) (o ^. #translation)

-- | solid rectangle, no border
blob :: Color -> Double -> RectStyle
blob c s = RectStyle zero black zero c s Nothing Nothing

-- | clear and utrans rect
clear :: RectStyle
clear = RectStyle zero black zero black zero Nothing Nothing

-- | transparent rectangle, with border
border :: Double -> Color -> Double -> RectStyle
border s c o = RectStyle s c o black zero Nothing Nothing

-- | Text styling
data TextStyle = TextStyle
  { size :: Double
  , color :: Color
  , opacity :: Double
  , alignH :: AlignH
  , hsize :: Double
  , vsize :: Double
  , nudge1 :: Double
  , rotation :: Maybe (Double, Pair Double)
  , translation :: Maybe (Pair Double)
  } deriving (Show, Eq, Generic)

instance ToJSON TextStyle
instance FromJSON TextStyle

data AlignH = MiddleH | StartH | EndH deriving (Eq, Show, Generic)

instance ToJSON AlignH
instance FromJSON AlignH

toTextAnchor :: AlignH -> TextAnchor
toTextAnchor MiddleH = TextAnchorMiddle
toTextAnchor StartH = TextAnchorStart
toTextAnchor EndH = TextAnchorEnd

textToAlignH :: Text -> AlignH
textToAlignH "MiddleH" = MiddleH
textToAlignH "StartH" = StartH
textToAlignH "EndH" = EndH
textToAlignH _ = MiddleH

instance ToHtml AlignH where
  toHtml = toHtml . (show :: AlignH -> Text)
  toHtmlRaw = toHtmlRaw . (show :: AlignH -> Text)

-- | the offical text style
defaultTextStyle :: TextStyle
defaultTextStyle =
  TextStyle 0.08 grey 1.0 MiddleH 0.45 1.1 (-0.25) Nothing Nothing

-- | doesn't include the rotation text style which needs to be specified in the same layer as the placement.
daText :: TextStyle -> DrawAttributes
daText o =
  mempty &
  (fontSize .~ Last (Just $ Num (o ^. #size))) .
  (strokeWidth .~ Last (Just $ Num 0)) .
  (strokeColor .~ Last (Just FillNone)) .
  (fillColor .~ Last (Just $ ColorRef (promotePixel $ uncolor $ o ^. #color))) .
  (fillOpacity .~ Just (fromRational $ o ^. #opacity)) .
  (textAnchor .~ Last (Just (toTextAnchor $ o ^. #alignH))) .
  maybe identity
    (\(r, Pair x y) -> transform %~ (<> Just [Rotate r (Just (x,y))])) (o ^. #rotation) .
  maybe identity (\(Pair x y) -> transform %~ (<> Just [Translate x (-y)])) (o ^. #translation)

-- | the extra area from text styling
-- FIXME PLEASE
styleBoxText :: (FromRatio a) =>
  TextStyle -> Text.Text -> Rect a
styleBoxText o t = fromRational <$> maybe flat (\r -> rotateRect (fst r) flat) (o ^. #rotation)
    where
      flat = Rect ((-x'/two) + x'*origx) (x'/two + x'*origx) ((-y'/two) - n1') (y'/two - n1')
      s = case getLast (daText o ^. fontSize) of
        Just (Num n) -> fromRational n
        _ -> 0.0
      h = o ^. #hsize
      v = o ^. #vsize
      n1 = o ^. #nudge1
      x' = s * h * fromRational (Text.length t)
      y' = s * v
      n1' = s * n1
      origx = case daText o ^. textAnchor of
        Last (Just TextAnchorStart) -> 0.5
        Last (Just TextAnchorEnd) -> -0.5
        Last (Just TextAnchorMiddle) -> 0.0
        _ -> 0.0

-- | Glyph styling
data GlyphStyle = GlyphStyle
  { size :: Double -- ^ glyph radius
  , color :: Color -- ^ fill color
  , opacity :: Double
  , borderColor :: Color -- ^ stroke color
  , borderOpacity :: Double
  , borderSize :: Double -- ^ stroke width (adds a bit to the bounding box)
  , shape :: GlyphShape
  , rotation :: Maybe (Double, Pair Double)
  , translation :: Maybe (Pair Double)
  } deriving (Show, Eq, Generic)

instance ToJSON GlyphStyle
instance FromJSON GlyphStyle

-- | the offical circle style
defaultGlyphStyle :: GlyphStyle
defaultGlyphStyle = GlyphStyle 0.03 blue 0.3 grey 0.3 0.015 CircleGlyph Nothing Nothing

-- | glyph shapes
data GlyphShape
  = CircleGlyph
  | SquareGlyph
  | EllipseGlyph Double
  | RectSharpGlyph Double
  | RectRoundedGlyph Double Double Double
  | TriangleGlyph (Pair Double) (Pair Double) (Pair Double)
  | VLineGlyph Double
  | HLineGlyph Double
  | SmileyGlyph
  deriving (Show, Eq, Generic)

instance ToJSON GlyphShape

instance FromJSON GlyphShape
 
fromGlyphShape :: (IsString s) => GlyphShape -> (s, [Double])
fromGlyphShape CircleGlyph = ("Circle", [])
fromGlyphShape SquareGlyph = ("Square", [])
fromGlyphShape (EllipseGlyph n) = ("Ellipse", [n])
fromGlyphShape (RectSharpGlyph n) = ("RectSharp", [n])
fromGlyphShape (RectRoundedGlyph n1 n2 n3) = ("RectRounded", [n1,n2,n3])
fromGlyphShape (TriangleGlyph (Pair x1 y1) (Pair x2 y2) (Pair x3 y3)) =
  ("Triangle", [x1, y1, x2, y2, x3, y3])
fromGlyphShape (VLineGlyph n) = ("VLine", [n])
fromGlyphShape (HLineGlyph n) = ("HLine", [n])
fromGlyphShape SmileyGlyph = ("Smiley", [])

toGlyphShape :: (IsString s, Eq s) => (s, [Double]) -> GlyphShape
toGlyphShape ("Circle", _) = CircleGlyph
toGlyphShape ("Square", _) = SquareGlyph
toGlyphShape ("Ellipse", n:_) = EllipseGlyph n
toGlyphShape ("RectSharp", n:_) = RectSharpGlyph n
toGlyphShape ("RectRounded", n1:n2:n3:_) = RectRoundedGlyph n1 n2 n3
toGlyphShape ("Triangle", x1:y1:x2:y2:x3:y3:_) = TriangleGlyph (Pair x1 y1) (Pair x2 y2) (Pair x3 y3)
toGlyphShape ("VLine", n:_) = VLineGlyph n
toGlyphShape ("HLine", n:_) = HLineGlyph n
toGlyphShape ("Smiley", _) = SmileyGlyph
toGlyphShape _ = SmileyGlyph

toGlyph :: (Eq a, IsString a) => a -> GlyphShape
toGlyph sh =
  case sh of
    "Circle" -> CircleGlyph
    "Square" -> SquareGlyph
    "Triangle" -> TriangleGlyph (Pair (-1) 0) (Pair 1 0) (Pair 0 1)
    "Ellipse" -> EllipseGlyph 1.5
    "Rectangle" -> RectSharpGlyph 1.5
    "Rounded Rectangle" -> RectRoundedGlyph 1.5 0.1 0.1
    "Verticle Line" -> VLineGlyph 0.01
    "Horizontal Line" -> HLineGlyph 0.01
    "Smiley Face" -> SmileyGlyph
    _ -> CircleGlyph

fromGlyph :: (IsString a) => GlyphShape -> a
fromGlyph sh =
  case sh of
    CircleGlyph -> "Circle"
    SquareGlyph -> "Square"
    TriangleGlyph{} -> "Triangle"
    EllipseGlyph _ -> "Ellipse"
    RectSharpGlyph _ -> "Rectangle"
    RectRoundedGlyph{} -> "Rounded Rectangle"
    VLineGlyph _ -> "Verticle Line"
    HLineGlyph _ -> "Horizontal Line"
    SmileyGlyph -> "Smiley Face"

daGlyph :: GlyphStyle -> DrawAttributes
daGlyph o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (o ^. #borderSize))) .
  (strokeColor .~ Last (Just $ ColorRef (promotePixel $ uncolor $ o ^. #borderColor))) .
  (strokeOpacity .~ Just (fromRational $ o ^. #borderOpacity)) .
  (fillColor .~ Last (Just $ ColorRef (promotePixel $ uncolor $ o ^. #color))) .
  (fillOpacity .~ Just (fromRational $ o ^. #opacity)) .
  maybe identity
    (\(r, Pair x y) -> transform %~ (<> Just [Rotate r (Just (x,y))])) (o ^. #rotation) .
  maybe identity (\(Pair x y) -> transform %~ (<> Just [Translate x (-y)])) (o ^. #translation)

-- | the extra area from glyph styling
styleBoxGlyph :: (Chartable a) => GlyphStyle -> Rect a
styleBoxGlyph s = toRect $ fromRational <$> case sh of
  EllipseGlyph a -> scale (Pair sz (a*sz)) one
  RectSharpGlyph a -> scale (Pair sz (a*sz)) one
  RectRoundedGlyph a _ _ -> scale (Pair sz (a*sz)) one
  VLineGlyph a -> scale (Pair (a*sz) sz) one
  HLineGlyph a -> scale (Pair sz (a*sz)) one
  _ -> (sz*) <$> one
  where
    sh = s ^. #shape 
    sz = s ^. #size

-- | line style
data LineStyle = LineStyle
  { width :: Double
  , color :: Color
  , opacity :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON LineStyle
instance FromJSON LineStyle

-- | the official default line style
defaultLineStyle :: LineStyle
defaultLineStyle = LineStyle 0.02 blue 0.5

daLine :: LineStyle -> DrawAttributes
daLine o =
  mempty &
  (strokeWidth .~ Last (Just $ Num (o ^. #width))) .
  (strokeColor .~ Last (Just $ ColorRef (promotePixel $ uncolor $ o ^. #color))) .
  (strokeOpacity .~ Just (fromRational $ o ^. #opacity)) .
  (fillColor .~ Last (Just FillNone))

-- | the extra area from the stroke element of an svg style attribute
styleBoxStroke :: (FromRatio a) => DrawAttributes -> Rect a
styleBoxStroke das = fromRational <$> Rect (-x/2) (x/2) (-x/2) (x/2)
  where
    x = case das ^. Svg.strokeWidth & getLast of
      Just (Num x') -> x'
      _ -> 0

-- | the extra geometric dimensions of a 'DrawAttributes'
-- only handles stroke width and transformations
styleBoxDA :: (ToRatio a, FromRatio a, Subtractive a) => DrawAttributes -> Spot a -> Rect a
styleBoxDA da r = fromRational <$> r' where
  r' = foldr tr (toRect $ fromRational <$> (Area' $ styleBoxStroke da) + r)
    (da ^. transform & maybe [] identity)
  tr a x = case a of
    Translate x' y' -> translateRect (Pair x' (-y')) x
    TransformMatrix{} ->
      throw (NumHaskException "TransformMatrix transformation not yet implemented")
    Scale s Nothing -> (s*) <$> x
    Scale sx (Just sy) -> toRect $ scale (Pair sx sy) (Area' x)
    Rotate d Nothing -> rotateRect d x
    Rotate d (Just (x',y')) -> rotateRect d (translateRect (Pair x' y') x)
    SkewX _ -> throw (NumHaskException "SkewX transformation not yet implemented")
    SkewY _ -> throw (NumHaskException "SkewY transformation not yet implemented")
    TransformUnknown -> x

-- | the extra geometric dimensions of a Chart (from both style and draw attributes)
styleBox :: (Chartable a) => Chart a -> Rect a
styleBox (Chart (TextA s ts) xs) = fold $ zipWith (\t x ->
  (styleBoxDA (daText s) . Area' . translateRect (toPair x) $ styleBoxText s t)) ts xs
styleBox (Chart (GlyphA s) xs) = fold
  (styleBoxDA (daGlyph s) . Area' . flip translateRect (styleBoxGlyph s) . toPair <$> xs)
styleBox (Chart (RectA s) xs) = fold
  (styleBoxDA (daRect s) <$> xs)
styleBox (Chart (LineA s) xs) = fold
  (styleBoxDA (daLine s) <$> xs)

-- | the extra geometric dimensions of a [Chart]
styleBoxes :: (Chartable a) => [Chart a] -> Rect a
styleBoxes xss = fold $ styleBox <$> xss

-- | project data to a ViewBox based on style effects
projectWithStyle :: (Chartable a) =>
  Rect a -> Chart a -> Chart a
projectWithStyle vb ch@(Chart s xs) =
  Chart s (projectOn vb (styleBox ch) <$> xs)

-- | project data to a ViewBox based on style effects
projectWithStyles :: (Chartable a) =>
  Rect a -> [Chart a] -> [Chart a]
projectWithStyles vb chs =
  zipWith Chart ss (fmap (projectOn vb (styleBoxes chs)) <$> xss)
  where
    ss = (\(Chart s _) -> s) <$> chs
    xss = (\(Chart _ xs) -> xs) <$> chs

-- | include a circle at the origin with size and color
showOriginWith :: forall a. (Chartable a) => Double -> Color -> Chart a
showOriginWith s c =
  Chart
  (GlyphA $
    #borderSize .~ 0.0 $
    #size .~ s $
    #color .~ c $
    defaultGlyphStyle)
  [zero]

-- * color
newtype Color = Color' { uncolor :: PixelRGB8 } deriving (Eq, Show, Generic)

pattern Color :: Pixel8 -> Pixel8 -> Pixel8 -> Color
pattern Color r g b = Color' (PixelRGB8 r g b)
{-# COMPLETE Color #-}

instance ToJSON Color where
  toJSON (Color r g b) = object ["r" .= r, "g" .= g, "b" .= b]

instance FromJSON Color where
  parseJSON = withObject "Color" $ \v ->
    Color <$>
    v .: "r" <*>
    v .: "g" <*>
    v .: "b"

-- | the official chart-unit blue
blue :: Color
blue = Color 93 165 218

-- | the official chart-unit grey
grey :: Color
grey = Color 102 102 102

-- | black
black :: Color
black = Color 0 0 0

-- | white
white :: Color
white = Color 255 255 255

-- | red
red :: Color
red = Color 255 0 0

-- | interpolate between two colors
blend :: Double -> Color -> Color -> Color
blend c (Color' p1) (Color' p2) = Color' $ mixWithAlpha f (f 0) p1 p2 where
  f _ x0 x1 = fromIntegral (round (fromIntegral x0 + c * (fromIntegral x1 - fromIntegral x0)) :: Integer)

-- | create pixel data from a function on a Pair
pixelate :: (Lattice a, Field a, Subtractive a, FromInteger a) =>
  (Pair a -> Double) -> Spot a -> Pair Int -> Color -> Color -> [(Spot a, Color)]
pixelate f r g c0 c1 = (\(x,y) -> (Area' x, blend y c0 c1)) <$> ps'
  where
    ps = rectF f (toRect r) g
    rs = snd <$> ps
    rs' = project (space1 rs :: Range Double) (Range 0 1) <$> rs
    ps' = zip (fst <$> ps) rs'

placedLabel :: Pair Double -> Double -> Text.Text -> Chart Double
placedLabel p d t =
  Chart (TextA (defaultTextStyle & #rotation .~ Just (d,p) & #translation .~ Just p) [t]) [zero]
