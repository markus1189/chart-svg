{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

import Chart.Core
import Chart.Numeric
import Chart.Svg
import Codec.Picture.Types
import Control.Lens
import Data.Generics.Labels()
import Data.List ((!!))
import Graphics.Svg.CssTypes hiding (Point)
import NumHask.Prelude hiding (Text, rotate)
import qualified Data.Map as Map
import qualified Data.Text as Text

ropts :: [RectStyle]
ropts =
  [ blob (Color 93 165 218) 0.5
  , blob (Color 120 80 60) 0.5
  ]

rss :: [[Spot Double]]
rss = fmap Area' <$>
  [ rectXY (\x -> exp (-(x ** 2) / 2)) (Range -5 5) 50
  , rectXY (\x -> 0.5 * exp (-(x ** 2) / 8)) (Range -5 5) 50
  ]

rs :: RectStyle
rs = RectStyle 0.1 (Color 102 102 102) 0.5 (Color 102 5 102) 0.5 Nothing Nothing

rs' :: RectStyle
rs' = rs &  #opacity .~ 0.1 & #borderOpacity .~ 0.1

oneChart :: Chart Double
oneChart = Chart (RectA rs) [one]

oneChart' :: Chart Double
oneChart' = Chart (RectA rs') [one]

rotateOne :: ChartSvg Double
rotateOne = defaultFrame $
  chartSvg (aspect 1) [showOriginWith 0.04 red] <>
  chartSvg (aspect 1) [oneChart'] <>
  rotate 30 (chartSvg (aspect 1) [oneChart])

translateOne :: ChartSvg Double
translateOne = defaultFrame $
  chartSvg (aspect 1) [showOriginWith 0.04 red] <>
  chartSvg (aspect 1) [oneChart'] <>
  translate (Pair 1 1) (rotate 30 (chartSvg (aspect 1) [oneChart]))

rectChart :: Chart Double
rectChart = Chart (RectA $ ropts!!0) (rss!!0)

rectCharts :: [Chart Double]
rectCharts =
  zipWith (\s xs -> Chart (RectA s) xs) ropts rss

-- * text
ts :: [(Text.Text, Spot Double)]
ts = zip
  (map Text.singleton ['a' .. 'y'])
  [Point (sin (x * 0.1)) x | x <- [0 .. 25]]

textChart :: Chart Double
textChart =
  Chart
  (TextA (defaultTextStyle & #size .~ (1.0 :: Double)) ["abcdefghij"])
 
  [zero]

textsChart :: Chart Double
textsChart =
  Chart
  ( TextA
    (defaultTextStyle &
    #size .~ 0.2)
    (fst <$> ts)
  )
 
  (snd <$> ts)

circle' :: Chart Double
circle' =
     Chart
      ( GlyphA (defaultGlyphStyle &
        #size .~ 1 &
        #borderSize .~ 0.2))
     
      [zero]

smiley :: Chart Double
smiley =
     Chart
      ( GlyphA (defaultGlyphStyle &
        #size .~ 1 &
        #borderSize .~ (0.02 :: Double) &
        #shape .~ SmileyGlyph))
     
      [zero]

glyphs :: [Chart Double]
glyphs = zipWith
     (\(sh, bs) p ->
         Chart
         ( GlyphA (defaultGlyphStyle &
                    #size .~ (0.2 :: Double) &
                    #borderSize .~ bs &
                    #shape .~ sh))
         [p])
     [ (CircleGlyph, 0.01 :: Double)
     , (SquareGlyph, 0.01)
     , (RectSharpGlyph 0.75, 0.01)
     , (RectRoundedGlyph 0.75 0.01 0.01, 0.01)
     , (EllipseGlyph 0.75, 0)
     , (VLineGlyph 0.02, 0)
     , (HLineGlyph 0.02, 0)
     , (SmileyGlyph, 0.01)
     ]
     [Point x 0 | x <- [0..7]]


gdata :: [[Spot Double]]
gdata = fmap Point' <$>
  [ dataXY sin (Range 0 (2*pi)) 30
  , dataXY cos (Range 0 (2*pi)) 30
  ]

gopts :: [GlyphStyle]
gopts =
  [ #borderSize .~ 0.001 $
    #size .~ 0.1 $
    defaultGlyphStyle
  , #borderSize .~ 0.001 $
    #size .~ 0.1 $
    #color .~ Color 100 30 30 $
    #shape .~ RectRoundedGlyph 1.5 0.01 (0.01 :: Double) $
    defaultGlyphStyle
  ]

glyphsChart :: [Chart Double]
glyphsChart = zipWith (\d s -> Chart (GlyphA s) d) gdata gopts

-- textual
boundText :: [Chart Double]
boundText =
  [ Chart (RectA defaultRectStyle) (Area' . styleBox <$> cs)
  , Chart a1 ps
  ]
  where
  t1 = fst <$> ts
  ps = projectTo (vbArea (aspect 3 :: ViewBox Double)) (snd <$> ts)
  t1s = TextA (defaultTextStyle & #size .~ 0.2) . (:[]) <$> t1
  cs = zipWith (\x y -> Chart x y) t1s ((:[]) <$> ps)
  a1 = TextA (defaultTextStyle & #size .~ 0.2) t1

pixel' :: (Pair Double -> Double) -> [Chart Double]
pixel' f =
  (\(r,c) -> Chart (RectA (RectStyle 0 black 0 c 1 Nothing Nothing)) [r]) <$>
  pixelate f (fmap (pi*) one) (Pair 100 100) blue grey

f1 :: TrigField a => Pair a -> a
f1 (Pair x y) = sin (cos (tan x)) * sin (cos (tan y))

cssCrisp :: CssRule
cssCrisp = CssRule [] [CssDeclaration "shape-rendering" [[CssString "crispEdges"]]]

label :: [Chart Double]
label =
  [placedLabel (Pair (1.0 :: Double) 1.0) (45.0 :: Double) "text at (1,1) rotated by 45 degrees"]

-- * lines
ls :: [[Spot Double]]
ls = fmap Point' <$>
  map (uncurry Pair) <$>
  [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
  , [(0.0, 0.0), (3.0, 3.0)]
  , [(0.5, 4.0), (0.5, 0)]
  ]

lopts :: [LineStyle]
lopts =
  zipWith (\w c -> defaultLineStyle & #color .~ c & #width .~ w)
  [0.015, 0.03, 0.01]
  [ Color 197 140 75
  , Color 60 127 43
  , Color 56 42 140
  ]

lines :: [Chart Double]
lines = zipWith (\d s -> Chart (LineA s) d) ls lopts

-- gline
gopts3 :: [GlyphStyle]
gopts3 =
  zipWith
  (\x y ->
     #color .~ x $
     #borderColor .~ x $
     #borderSize .~ 0.005 $
     #shape .~ y $
     #size .~ 0.08 $
     defaultGlyphStyle)
  [ Color 120 67 30
  , Color 30 48 130
  , Color 60 60 60
  ]
  [EllipseGlyph 1.5, SquareGlyph, CircleGlyph]

glines :: [Chart Double]
glines = cs <> gs where
  cs = zipWith (\d s -> Chart (LineA s) d) ls lopts
  gs = zipWith (\d s -> Chart (GlyphA s) d) ls gopts3

lgdata :: [(Text.Text, Spot Double)]
lgdata =
  (\(p@(Point x y)) -> (show x <> "," <> show y, fromIntegral <$> p)) <$>
    (Point <$> [0 .. 5] <*> [0 .. 5] :: [Spot Int])

lglyph :: [Chart Double]
lglyph = txt <> gly where
  txt = (\(t, p) -> Chart (TextA
    ( defaultTextStyle &
      #opacity .~ 0.2 &
      #translation .~ Just (Pair 0 0.04))
                           [t])
    [p])
    <$> lgdata
  gly = (\d -> Chart (GlyphA
    ( defaultGlyphStyle &
      #size .~ 0.01 &
      #borderSize .~ 0 &
      #color .~ black))
     
      [d]) <$> (snd <$> lgdata)

code :: (Semigroup a, IsString a) => a -> a
code x = "\n```\n" <> x <> "\n```\n"

main :: IO ()
main = do
  writeFile "other/mempty.md" (code $ xmlToText $ renderXml (Pair (200.0 :: Double) 200.0) mempty)

  write "other/one.svg" (Pair 200.0 200.0)
    (pad 1.1 $ chartSvg (aspect 1) [Chart (RectA defaultRectStyle) [one]] :: ChartSvg Double)
  write "other/rotateOne.svg" (Pair 200.0 200.0) rotateOne
  write "other/translateOne.svg" (Pair 200.0 200.0) translateOne
  scratchWith (defaultScratchStyle & #fileName .~ "other/rectChart.svg")
    [rectChart]
  scratchWith (defaultScratchStyle & #fileName .~ "other/rectCharts.svg")
    rectCharts
  writeWith "other/pixel.svg" (Pair 200.0 200.0) Map.empty "" [cssCrisp] (chartSvg (aspect 1) (pixel' f1))
  scratchWith
    (defaultScratchStyle &
     #fileName .~ "other/textChart.svg" &
     #ratioAspect .~ 3
    )
    [textChart]
  scratchWith
    (defaultScratchStyle &
     #fileName .~ "other/textsChart.svg" &
     #ratioAspect .~ 3
    )
    [textsChart]
  scratchWith
    (defaultScratchStyle &
     #fileName .~ "other/boundText.svg" &
     #ratioAspect .~ 3
    )
    boundText
  scratchWith
    (defaultScratchStyle &
     #fileName .~ "other/label.svg" &
     #ratioAspect .~ 1
    )
    label
  scratchWith
    (defaultScratchStyle &
     #fileName .~ "other/circle.svg" &
     #ratioAspect .~ 1
    )
    [circle']
  scratchWith
    (defaultScratchStyle &
     #fileName .~ "other/glyphs.svg" &
     #ratioAspect .~ 3
    )
    glyphs
  scratchWith
    (defaultScratchStyle &
     #fileName .~ "other/smiley.svg" &
     #ratioAspect .~ 1
    )
    [smiley]
  scratchWith
    (defaultScratchStyle &
     #fileName .~ "other/glyphsChart.svg" &
     #ratioAspect .~ 3
    )
    glyphsChart
  scratchWith
    (defaultScratchStyle &
     #fileName .~ "other/lglyph.svg" &
     #ratioAspect .~ 1.5
    )
    lglyph
  scratchWith
    (defaultScratchStyle &
     #fileName .~ "other/lines.svg" &
     #ratioAspect .~ 1.5
    )
    lines
  scratchWith
    (defaultScratchStyle &
     #fileName .~ "other/glines.svg" &
     #ratioAspect .~ 1.5
    )
    glines
  scratchWith
    (defaultScratchStyle &
     #fileName .~ "other/compound.svg" &
     #ratioAspect .~ 1.5
    )
    (lglyph <> glines)
  putStrLn (" 👍" :: Text.Text)

