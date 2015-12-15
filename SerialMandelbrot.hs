--SerialMandelbrot.hs
--Author Justin Dawson (JDawson@ku.edu)

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank
import qualified Data.Vector.Unboxed as V
import Data.Complex
import qualified Data.Text as Text
import Control.Monad
import Debug.Trace
import GHC.Word


step= 0.0025

mandelbrot :: Double -> Double -> [Word8]
mandelbrot  x y = let val = x :+ y
                      zs = take 255 $ iterate (\z -> z^2 + val) 0
                      iter = fromIntegral $ length $ takeWhile (\intermediate -> magnitude intermediate < 2) zs
                   in [iter,0,iter,255]



main:: IO()
main = blankCanvas 3000 $ \ context -> do
   putStrLn "Start Request"
   let v =  [(x,y)| y<-[1,(1-step) .. -1], x <-[-2, (-2 + step) .. 0.5]]
   let res = map (\(x,y)->mandelbrot x y) $ v 
   send context $ do let w = ((abs . round) $ (0.5 - (-2)) / step) + 1
                     let h = ((abs . round) $ ((-1) -1) / step) + 1
                     putImageData (ImageData (fromIntegral w) (fromIntegral h) (V.fromList (concat res)), [0,0])
