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

import Chart.Svg
import Control.Lens 
import qualified Data.Text as Text
import Data.Generics.Labels()
import Chart.Hud
import Chart.Spot
import Chart.Types
import Data.List (zipWith3)
import Protolude
import NumHask.Point
import NumHask.Rect
import GHC.Exts

tri1 :: Int -> Int -> Double -> [Chart Double]
tri1 a b s =
  [ Chart
    (GlyphA
      (defaultGlyphStyle &
        #shape .~ TriangleGlyph (Point 0 0)
        (Point (fromIntegral a) 0)
        (Point (fromIntegral a) (fromIntegral b)) &
        #borderSize .~ 0.01 &
        #size .~ s &
        #translate ?~ c))
    [SpotPoint $ Point (fromIntegral a) (fromIntegral b)]
  ]
  where
    c :: Point Double
    c = Point (-s*(fromIntegral a*2)/3) (-s*fromIntegral b/3)

tri2 :: Integer -> Integer -> Double -> Double -> Double -> Double -> [Chart Double]
tri2 a b s gap bs txts =
  [ Chart
    (GlyphA
      (defaultGlyphStyle &
        #shape .~ TriangleGlyph (Point 0 0)
        (Point (fromIntegral a) 0)
        (Point (fromIntegral a) (fromIntegral b)) &
        #borderSize .~ bs &
        #size .~ s &
        #translate ?~ c))
    [SpotPoint $ Point (fromIntegral a) (fromIntegral b)]
  ] <>
  zipWith3
  (\side bump ts' -> Chart
    (TextA (ts' & #translate ?~ c + bump) (show <$> [side]))
    [SpotPoint $ Point (fromIntegral a) (fromIntegral b)])
  [a,b,h]
  [ Point (s * fromIntegral a / 2) ((-gap) -
              0.65 *
              realToFrac (ts ^. #vsize) *
              realToFrac (ts ^. #size))
  , Point (s * fromIntegral a + gap) (s * fromIntegral b / 2)
  , Point (s * fromIntegral a / 2 - gap * (fromIntegral b ** 2.0 / fromIntegral h ** 2)) (s * fromIntegral b / 2 + gap * (fromIntegral a ** 2.0 / fromIntegral h ** 2))
  ]
  [ ts
  , ts & #anchor .~ AnchorStart
  , ts & #rotation ?~ (- atan (fromIntegral b/fromIntegral a) * (180 / pi))
  ]
  where
    c :: Point Double
    c = Point (-s*(fromIntegral a*2)/3) (-s*fromIntegral b/3)
    h :: Integer
    h = round $ sqrt (fromIntegral (a^(2::Integer)+b^(2::Integer)) :: Float) :: Integer
    ts = defaultTextStyle & #size .~ txts

tri3 :: Integer -> Integer -> Double -> Double -> Double -> Double -> [Chart Double]
tri3 a b s gap bs txts =
  [ Chart
    (GlyphA
      (defaultGlyphStyle &
        #shape .~ TriangleGlyph (Point 0 0)
        (Point (fromIntegral a / ns) 0)
        (Point (fromIntegral a / ns) (fromIntegral b / ns)) &
        #borderSize .~ bs & #size .~ s &
        #translate ?~ ((/ns) <$> c)))
    [SpotPoint $ Point (fromIntegral a) (fromIntegral b)]
  ] <>
  zipWith3
  (\side bump ts' -> Chart
    (TextA (ts' & #translate ?~ ((/ns) <$> (c + bump))) (show <$> [side]))
    [SpotPoint $ Point (fromIntegral a) (fromIntegral b)])
  [a,b,h]
  [ Point (s * fromIntegral a / 2) ((-gap) -
              0.65 *
              realToFrac (ts ^. #vsize) *
              realToFrac (ts ^. #size))
  , Point (s * fromIntegral a + gap) (s * fromIntegral b / 2)
  , Point (s * fromIntegral a / 2 - gap * (fromIntegral b ** 2.0 / fromIntegral h ** 2)) (s * fromIntegral b / 2 + gap * (fromIntegral a ** 2.0 / fromIntegral h ** 2))
  ]
  [ ts
  , ts & #anchor .~ AnchorStart
  , ts & #rotation ?~ (- atan (fromIntegral b/fromIntegral a) * (180 / pi))
  ]
  where
    c :: Point Double
    c = Point (-s*(fromIntegral a*2)/3) (-s*fromIntegral b/3)
    h :: Integer
    h = round $ sqrt (fromIntegral (a^(2::Integer)+b^(2::Integer)) :: Float) :: Integer
    ts = defaultTextStyle & #size .~ txts
    ns = log $ sqrt $ fromIntegral a * fromIntegral b / 2

tri2s :: Double -> Double -> Double -> Double -> [(Integer,Integer)] -> [Chart Double]
tri2s s gap bs txts ab =
  mconcat $ (\(a, b) -> tri2 a b s gap bs txts) <$> ab

tri2ss :: Double -> Double -> Double -> Double -> Maybe (Rect Double) -> Integer -> ChartSvg Double
tri2ss s gap bs txts r n =
  hudSvgWith unitRect (defRectS area) hud1 (tri2s s gap bs txts psf)
  where
    ps = euclid n <> ((\(a,b) -> (a,-b)) <$> euclid n)
    psf = maybe ps (\(Rect x z y w) -> filter (\(a,b) -> fromIntegral a >= x && fromIntegral a <= z && fromIntegral b >= y && fromIntegral b <= w) ps) r
    area = r <|>
      foldRect 
       (toRect .
         (\ (a, b) -> SpotPoint (Point (fromIntegral a) (fromIntegral b)))
         <$> ps)

tri3s :: Double -> Double -> Double -> Double -> [(Integer,Integer)] -> [Chart Double]
tri3s s gap bs txts ab =
  mconcat $ (\(a, b) -> tri3 a b s gap bs txts) <$> ab

tri3ss :: Double -> Double -> Double -> Double -> Maybe (Rect Double) -> Integer -> ChartSvg Double
tri3ss s gap bs txts r n =
  hudSvgWith unitRect (defRectS area) hud1 (tri3s s gap bs txts psf)
  where
    ps = euclid n <> ((\(a,b) -> (a,-b)) <$> euclid n)
    psf = maybe ps (\(Rect x z y w) -> filter (\(a,b) -> fromIntegral a >= x && fromIntegral a <= z && fromIntegral b >= y && fromIntegral b <= w) ps) r
    area =
      r <|>
      foldRect
       (toRect .
         (\ (a, b) -> SpotPoint (Point (fromIntegral a) (fromIntegral b)))
         <$> ps)

corners :: Rect Double -> Double -> [Chart Double]
corners (Rect x z y w) s =
  [Chart
  (GlyphA $
   #borderSize .~ 0 $
    #size .~ s $
    defaultGlyphStyle)
  [SP x y, SP x w, SP z y, SP z w]]

hud1 :: [Hud Double]
hud1 =
  [ tick PlaceBottom defaultTick <>
    tick PlaceLeft defaultTick
  ]

euclid :: Integer -> [(Integer,Integer)]
euclid x = filter (\(a,b) -> a/=0 && b/=0) $ (\m n -> (m*m - n*n, 2*m*n)) <$> [1..x] <*> [1..x] :: [(Integer,Integer)]

main :: IO ()
main = do
  write "other/tri1.svg" (Point 400.0 400)
    (pad 1.1 $ hudSvg (aspect 1) [hud1] (tri1 3 4 0.1 <> Main.corners (Rect 0 3 0 4) 0.1))
  write "other/tri2.svg" (Point 400 400)
    (pad 1.1 $ hudSvgWith (aspect 1) (Rect 0 20 0 20) hud1 (tri2 5 12 0.05 0.025 0.01 0.01))
  write "other/tri2s.svg" (Point 400 400)
    (pad 1.1 (tri2ss 0.00004 0.0001 0 0 (Just (Rect 0 3000 0 3000)) 60))
  write "other/tri3s.svg" (Point 400 400)
    (pad 1.1 (tri3ss 0.0001 0.0001 0 0 (Just (Rect 0 4000 0 4000)) 100))
  putStrLn (" 👍" :: Text.Text)

