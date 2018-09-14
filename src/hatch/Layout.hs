{-# LANGUAGE DeriveFunctor #-}

module Layout where

import Graphics.Gloss (Picture (Pictures), pictures, translate)


-- Our Arrange type represents some arrangement of images
data Arrange a = Vert (Arrange a) (Arrange a)
               | Horz (Arrange a) (Arrange a)
               | Supp (Arrange a) (Arrange a)
               | Leaf a
    deriving (Show, Functor)


-- We define a type synonym just to make dealing with Images a little nicer
type Image = Arrange Picture


-- Combinator functions

-- Horizontal composition
infixr 4 <|>
(<|>) :: Image -> Image -> Image
a <|> b = Horz a b

infixr 3 <->
-- Vertical composition
(<->) :: Image -> Image -> Image
a <-> b = Vert a b

infixr 2 <@>
-- Superimposition
(<@>) :: Image -> Image -> Image
a <@> b = Supp a b

-- Assume images to be exactly 200x125 for simplicity
imgW :: Int
imgW = 200
imgH :: Int
imgH = 125

type Size = (Int, Int)

width :: Size -> Int
width = fst

height :: Size -> Int
height = snd

-- | `size` @img calculates the size of @img
size :: Image -> Size
size (Leaf _) = (imgW, imgH)
size (Vert i j) = (max iW jW, iH+jH)
    where (iW,iH) = size i
          (jW,jH) = size j
size (Horz i j) = (iW+jW, max iH jH)
    where (iW,iH) = size i
          (jW,jH) = size j
size (Supp i j) = (max iW jW, max iH jH)
    where (iW,iH) = size i
          (jW,jH) = size j

-- Convert from Images to Gloss's picture format
layout :: Image -> Picture
layout (Vert a b) = pictures $ let
        tb = translate 0 (fromIntegral (height $ size a)) $ layout b
        ta = case layout a of
            Pictures ps -> ps
            xs -> [xs]
    in tb : ta
layout (Horz a b) = pictures $ let
        tb = translate (fromIntegral (width $ size a)) 0 $ layout b
        ta = case layout a of
            Pictures ps -> ps
            xs -> [xs]
    in tb : ta
layout (Supp a b) = pictures $ let
        tb = layout b
        ta = case layout a of
            Pictures ps -> ps
            xs -> [xs]
    in tb : ta
layout (Leaf p  ) = p
