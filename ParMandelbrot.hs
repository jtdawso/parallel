-- File: ParMandelbrot.hs
-- Author: Justin Dawson
 
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank
import qualified Data.Vector.Unboxed as V
import Data.Complex
import qualified Data.Text as Text
import Control.Concurrent
import Control.Monad
import Control.Parallel.Strategies
import Debug.Trace
import GHC.Word

import Data.Time

step = 0.0025


mandelbrot:: Double -> Double -> [Word8]
mandelbrot  x y = let val = x :+ y
                      zs = take 255 $ iterate (\z -> z^2 + val) 0
                      iter = fromIntegral $ length $ takeWhile (\intermediate -> magnitude intermediate < 2) zs
                   in [iter, 0, iter, 255]

main :: IO()
main =  blankCanvas 3000 {middleware=[]}$ \ context -> do
        putStrLn "Start Request"
        start <- getCurrentTime

        let v =  [(x,y)| y<-[1,(1-step) .. -1], x <-[-2, (-2 + step) .. 0.5]]
        let w =  ((1+) . abs . round) $ (0.5 - (-2))/step 
        let h =  ((1+) . abs . round) $ ((-1) - 1) / step
        let res = parMap (rparWith rdeepseq) (\(x,y)->mandelbrot x y) $ v

        send context $ putImageData (ImageData (fromIntegral w) (fromIntegral h) (V.fromList (concat res)), [0,0])
        stop <- getCurrentTime
        print $ diffUTCTime stop start

