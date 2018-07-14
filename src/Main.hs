{-#LANGUAGE DeriveFunctor#-}

module Main where

import Graphics.Gloss hiding (red, rotate, animate, translate)
import Graphics.Gloss.Game (png)
import qualified Graphics.Gloss as G (red, rotate, animate, translate)
import Debug.Trace

{-------------------------------------------------------------------------------
                                  Data Types
-------------------------------------------------------------------------------}

type Image = Arrange Picture

data Animal = Cat | Dog
    deriving (Show, Read)

data Arrange a = Vert (Arrange a) (Arrange a)
               | Horz (Arrange a) (Arrange a)
               | Leaf a
    deriving (Show, Functor)


imgW :: Int
imgW = 200
imgH :: Int
imgH = 125

{-------------------------------------------------------------------------------
                             Principal Functions
-------------------------------------------------------------------------------}

red :: Image -> Image
red = fmap (color G.red)

rotate :: Int -> Image -> Image
rotate r = fmap (G.rotate $ fromIntegral r)

offset :: Int -> Int -> Image -> Image
offset x y = fmap $ G.translate (fromIntegral x) (fromIntegral y)

(<|>) :: Image -> Image -> Image
a <|> b = Horz a b

(<->) :: Image -> Image -> Image
a <-> b = Vert a b

cat :: Image
cat = Leaf $ png "cat.png"

layout :: Image -> Picture
layout (Vert a b) = pictures $ let
        tb = G.translate 0 (fromIntegral imgH) $ layout b
        ta = case layout a of
            Pictures ps -> ps
            xs -> [xs]
    in tb : ta
layout (Horz a b) = pictures $ let
        tb = G.translate (fromIntegral imgW) 0 $ layout b
        ta = case layout a of
            Pictures ps -> ps
            xs -> [xs]
    in tb : ta
layout (Leaf p  ) = p


{-------------------------------------------------------------------------------
                           Program Initialisation
-------------------------------------------------------------------------------}

window :: Display
window = InWindow "FUNctional Programming" (1280, 960) (10, 10)

background :: Color
background = white

main :: IO ()
main = animSteps fps window background $ layout . animate
--main = display window background $ layout . anim

animSteps :: Int -> Display -> Color -> (Int -> Picture) -> IO ()
animSteps steps window background animFunc = G.animate window background (animFunc . tStep)
    where
        tStep :: Float -> Int
        tStep t = ceiling (t/framePeriod)
        framePeriod = 1.0 / fromIntegral steps

fps :: Int
fps = 60

row 0 x = x
row n x = x <|> row (n-1) x

drawing :: Image
drawing = cat


animate :: Int -> Image
animate frameNumber = rotate frameNumber $ offset (-300) 0 cat



--
