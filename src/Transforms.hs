module Transforms where

import Layout
import qualified Graphics.Gloss as G (rotate, translate)

-- Rotate an image in degrees.
-- Wraps the Gloss Picture rotation.
rotate :: Int -> Image -> Image
rotate r = fmap (G.rotate $ fromIntegral r)

offset :: Int -> Int -> Image -> Image
offset x y = fmap $ G.translate (fromIntegral x) (fromIntegral y)
