-- File: RepaMandelbrot.hs
-- Author: Justin Dawson (JDawson@ku.edu)
 
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank
import qualified Data.Vector.Unboxed as V
import Data.Complex
import qualified Data.Text as Text
import Control.Monad
import qualified Data.Array.Repa as Repa 
import qualified Data.ByteString as BS
import Data.Array.Repa hiding ((++), map)
import Data.Word  (Word32, Word8)
import Data.Bits

import Data.Time

step = 0.0025

convert :: Word32 -> [Word8]
convert x = let words = [ fromIntegral (x `shiftR` 16)
                        , fromIntegral (x `shiftR` 8)
                        , fromIntegral x
                        , 255]
             in words

mandelbrot:: Double -> Double -> Word32
mandelbrot x y = let val = x :+ y
                     zs = take 255 $ iterate (\z -> z^2 + val) 0
                     iter = fromIntegral $ length $ takeWhile (\intermediate -> magnitude intermediate  < 2 ) $ zs
                     color = BS.foldl (\acc x -> (acc `shiftL` 8) .|. (fromIntegral x)) zeroBits $ BS.pack [iter,0,iter]
                  in color

main :: IO ()
main = blankCanvas 3000 {middleware=[]} $ \ context -> do
          putStrLn "Received Request"
          start <- getCurrentTime
          --let v =  [(x,y)| y<-[1,(1-step) .. -1], x <-[-2, (-2 + step) .. 0.5]]
          let h = ((1+) . abs . round) $ ((-1) -1) / step
          let w = ((1+) . abs . round) $ (0.5 -(-2)) / step
          let v = fromFunction (Z :. h:. w) (\ (Z :. j :. i)-> ( (fromIntegral i)*step - 2, 1- (fromIntegral j) * step) )
          let res = Repa.map (\(x,y)->mandelbrot x y) v 

          send context $ do arr <- computeP res :: Canvas (Array U DIM2 Word32)
                            putImageData (ImageData (w) (h) ((V.fromList (concatMap convert (toList arr)))), [0,0])
          stop <- getCurrentTime
          print $ diffUTCTime stop start
             
