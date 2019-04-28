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

module Chart.Core where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Generics.Labels ()
import qualified Graphics.Svg as Svg
import NumHask.Prelude as P hiding (Group)
import Codec.Picture

newtype DrawAtts = DrawAtts { unDA :: Svg.DrawAttributes } deriving (Eq, Show, Generic)

instance ToJSON DrawAtts where
  toJSON (DrawAtts da) =
    object
    [ "strokeWidth" .= (da ^. Svg.strokeWidth)
    , "strokeColor" .= (da ^. Svg.strokeColor)
    ]


{-
instance FromJSON DrawAtts where
  parseJSON = withObject "DrawAtts" $ \v ->
    DrawAtts . Svg.DrawAttributes <$> (fmap (withObject "" parseJSON))
-}

instance ToJSON Svg.Number where
  toJSON (Svg.Num x) = object ["Num" .= x]
  toJSON (Svg.Px x) = object ["Px" .= x]
  toJSON (Svg.Em x) = object ["Em" .= x]
  toJSON (Svg.Percent x) = object ["Percent" .= x]
  toJSON (Svg.Pc x) = object ["Pc" .= x]
  toJSON (Svg.Mm x) = object ["Mm" .= x]
  toJSON (Svg.Cm x) = object ["Cm" .= x]
  toJSON (Svg.Point x) = object ["Point" .= x]
  toJSON (Svg.Inches x) = object ["Inches" .= x]

instance ToJSON Svg.Texture where
  toJSON (Svg.ColorRef (PixelRGBA8 r b g a)) = 
    object
    [ "r" .= r
    , "g" .= g
    , "b" .= b
    , "a" .= a
    ]
  toJSON (Svg.TextureRef s) = 
    object
    [ "TextureRef" .= s
    ]
  toJSON Svg.FillNone =
    object
    [ "FillNone" .= ("FillNone" :: Text)
    ] 

