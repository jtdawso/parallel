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

step = 0.0025


mandelbrot:: Double -> Double -> [Word8]
mandelbrot  x y = let val = x :+ y
                      zs = take 255 $ iterate (\z -> z^2 + val) 0
                      iter = fromIntegral $ length $ takeWhile (\intermediate -> magnitude intermediate < 2) zs
                   in [iter, 0, iter, 255]

main :: IO()
main =  blankCanvas 3000 $ \ context -> do
        putStrLn "Start Request"
        let v =  [(x,y)| y<-[1,(1-step) .. -1], x <-[-2, (-2 + step) .. 0.5]]
        let w =  ((1+) . abs . round) $ (0.5 - (-2))/step 
        let h =  ((1+) . abs . round) $ ((-1) - 1) / step
        let res = parMap (rparWith rdeepseq) (\(x,y)->mandelbrot x y) $ v
        send context $ putImageData (ImageData (fromIntegral w) (fromIntegral h) (V.fromList (concat res)), [0,0])

{-
main :: IO ()
main = blankCanvas 3000 $ \ context -> do
          putStrLn "Received Request"
          let v =  [(x,y)| y<-[1,(1-step) .. -1], x <-[-2, (-2 + step) .. 0.5]]
          let res = traceEvent "TESTING, THIS IS THE BEGINNING OF THE PARMAPS ADFS:DLJS:DLJFS:LDJFSLDJF" $ parMap (rparWith rdeepseq) (\(x,y)->mandelbrot x y) $ v 
          traceEvent "HERE IS THE FIRST PRINT HERE IS THE FIRST PRINT HERE IS THE FIRST PRINT HERE IS THE FIRST PRINT FIRST PRINT FIRST PRINT FIRST PRINT FIRST PRINT" $ print (length (filter (\(x,y) -> x < 0.5) v)) 
          traceEvent "HERE IS THE SECOND PRINT HERE IS THE SECOND PRINT HERE IS THE SECOND PRINT HERE IS THE SECOND PRINT HERE IS THE SECOND PRINT" $ print (length (filter (\(x,y,color) -> x < 0.5) res)) 
          traceEvent "SEND CONTEXT SEND CONTEXT SEND CONTEXT SEND CONTEXT SEND CONTEXT SEND CONTEXT SEND CONTEXT SEND CONTEXT SEND CONTEXT SEND CONTEXT" $ send context $ mySquareList res
          return ()
  -}           
