module Main where

import Workshop

main = runPart2 animation

{-------------------------------------------------------------------------------
                            Edit below this line
-------------------------------------------------------------------------------}

animation :: Int -> Image
animation frameNumber = offset (inted sin frameNumber) (inted cos frameNumber) $
                        rotate (0-frameNumber) $ fan 20 300 cat

fan :: Int -> Int -> Image -> Image
fan num dist img = foldr1 (<@>) (zipWith ($) trans imgs)
    where
        trans = map (rotate $ ) $ map (\x -> (x*360) `div` num) [1..num]
        imgs = repeat $ offset dist 0 img

inted :: (Float -> Float) -> Int -> Int
inted f n = round $ (f.(/50).fromIntegral) n * 100
