module Main where

import Workshop

main = runPart1 drawing

{-------------------------------------------------------------------------------
                            Edit below this line
-------------------------------------------------------------------------------}

drawing :: Image
drawing = cat <|> (blank <-> dog) <|> cat
