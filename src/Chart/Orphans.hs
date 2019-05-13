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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chart.Orphans where

-- import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Generics.Labels ()
import qualified Graphics.Svg as Svg
import qualified Graphics.Svg.CssTypes as CssSvg
import NumHask.Prelude as P hiding (Group)
import Codec.Picture
import Chart.Core

instance (ToJSON a) => ToJSON (Chart a)
instance (FromJSON a) => FromJSON (Chart a)

newtype DrawAtts = DrawAtts { unDA :: Svg.DrawAttributes } deriving (Eq, Show, Generic)

instance ToJSON DrawAtts
instance ToJSON Svg.DrawAttributes
instance ToJSON Svg.TextAnchor
instance ToJSON Svg.FontStyle
instance ToJSON Svg.Cap
instance ToJSON Svg.LineJoin
instance ToJSON Svg.Texture
instance ToJSON Svg.Transformation
instance ToJSON Svg.FillRule
instance ToJSON Svg.ElementRef
instance ToJSON CssSvg.Number

instance FromJSON DrawAtts
instance FromJSON Svg.DrawAttributes
instance FromJSON Svg.TextAnchor
instance FromJSON Svg.FontStyle
instance FromJSON Svg.Cap
instance FromJSON Svg.LineJoin
instance FromJSON Svg.Texture
instance FromJSON Svg.Transformation
instance FromJSON Svg.FillRule
instance FromJSON Svg.ElementRef
instance FromJSON CssSvg.Number

instance ToJSON PixelRGBA8 where
  toJSON (PixelRGBA8 r g b a) = object ["r" .= r, "g" .= g, "b" .= b, "a" .= a]

instance FromJSON PixelRGBA8 where
  parseJSON = withObject "Color" $ \v ->
    PixelRGBA8 <$>
    v .: "r" <*>
    v .: "g" <*>
    v .: "b" <*>
    v .: "a"

