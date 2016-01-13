--File: ForkIOMandelbrot.hs
--Author: Justin Dawson (JDawson@ku.edu)

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank
import qualified Data.Vector.Unboxed as V
import Data.Complex
import qualified Data.Text as Text
import Control.Concurrent
import Control.Monad (forM_)
import GHC.Word
import Debug.Trace

import Data.Time

step = 0.0025

mandelbrot:: Double -> Double -> [Word8]
mandelbrot x y = let val = x :+ y
                     zs = take 255 $ iterate (\z -> z^2 + val) 0
                     iter = fromIntegral $ length $ takeWhile (\intermediate -> magnitude intermediate < 2) zs
                  in [iter,0,iter,255]

main :: IO()
main = blankCanvas 3000 {middleware=[]}$ \ context -> do
           print "Start Request"
           start <- getCurrentTime
           let v1 =  [(x,y)| y<-[1,(1-step) .. 0], x <-[-2, (-2 + step) .. -0.75]]
           let v2 =  [(x,y)| y<-[1,(1-step) .. 0], x <-[-0.75,(-0.75 + step)  .. 0.5]]
           let v3 =  [(x,y)| y<-[0,(0-step) .. -1], x <-[-2, (-2 + step) .. -0.75]]
           let v4 =  [(x,y)| y<-[0,(0-step) .. -1], x <-[-0.75,(-0.75+step)  .. 0.5]]
           let vs = zip [0..] [v1,v2,v3,v4]
           let h =  ((1+) . abs . round) ((0-1) / step)
           let w =  ((1+) . abs . round) (((-0.75) - (-2)) / step)
           sequence $  map (forkIO . (\(num, v) -> 
                       let res = map (\(x,y) -> mandelbrot x y) v 
                        in  send context $ do 
                                  let x = (num `mod` 2)* w
                                  let y = (num `div` 2) * h
                                  putImageData (ImageData (fromIntegral w) (fromIntegral h) (V.fromList (concat res)) , [fromIntegral x,fromIntegral y ])
                           )) vs
           yield
           stop <- getCurrentTime
           print $ diffUTCTime stop start
           return () 
