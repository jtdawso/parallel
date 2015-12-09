-- File: ParMandelbrot.hs
-- Author: Justin Dawson
 
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank
import qualified Data.Vector.Unboxed as V
import Data.Complex
import qualified Data.Text as Text
import Control.Monad
import qualified Data.Array.Repa as Repa 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Array.Repa hiding ((++), map)
import Data.Word  (Word32)
import Data.Bits
step = 0.0025

convert :: Word32 -> String
convert x = let words = [ fromIntegral (x `shiftR` 16)
                        , fromIntegral (x `shiftR` 8)
                        , fromIntegral x]
             in (BSC.unpack . BS.pack) words

mySquareList xs = forM_ xs (\(x,y,color) -> mySquare x y color)

mySquare x y color = do
 save()
 translate((x*300)+800,(y*300)+500)
 fillStyle  (Text.pack ("#"++ (convert color)))
 fillRect(0,0,2,2)
 restore()
 
redYellow = ["F00","F10","F20","F30","F40","F50","F60","F70","F80","F90","FA0","FB0"]
blackGreen = ["000","010","020","030","040","050","060","070","080","090","0A0","0B0"]
blueGreen = ["0F0","0E1","0D2","0C3","0B4","0A5","096","087","078","069","05A","04B"]
yellowBlue = ["FF0","EE1","DD2","CC3","BB4","AA5","996","887","778","669","55A","44B"]
yellowRedBlue = ["FF0","FE0","FC0","FA0","F84","A75","096","087","078","069","05A","04B"]
blackGreenBlue = ["000","0A0","0A0","0C0","0F0","0C5","0A6","087","078","069","05A","04B"]


--intToHex n = (['0'..'9'] ++ ['A'..'F']) !! ((n `div` 16) ) 
--intToHex n = (['A','A','B','B','C','C','D','D','E','E','F','F']) !! (if (n `div` 12) >= 12 then 11 else (n`div`12) ) 
intToHex:: Int -> BS.ByteString
intToHex n =  blackGreen !! (if (n `div` 12) >= 12 then 11 else (n`div`12) ) 

mandelbrot:: Double -> Double -> (Double, Double, Word32)
mandelbrot x y = let val = x :+ y
                     zs = take 255 $ iterate (\z -> z^2 +val) 0
                     iter = length $ takeWhile (\intermediate -> magnitude intermediate  < 2 ) $ zs
                     color = BS.foldl (\acc x -> (acc `shiftL` 8) .|. (fromIntegral x)) zeroBits $ intToHex iter
                  in (x,y,color)

 
main :: IO ()
main = blankCanvas 3000 $ \ context -> do
          putStrLn "Received Request"
          --let v =  [(x,y)| y<-[1,(1-step) .. -1], x <-[-2, (-2 + step) .. 0.5]]
          let h = (abs . round) $ (1.0 - (-1.0)) / step ::Int
          let w = (abs . round) $ ((-2.0) - 0.5)/step :: Int

          let v = fromFunction (Z :. w:. h) (\ (Z :. i :. j)-> ( (fromIntegral i)*step - 2, 1- (fromIntegral j) * step) )
          let res = Repa.map (\(x,y)->mandelbrot x y) v 
          --print (length (filter (\(x,y) -> x < 0.5) v)) 
          --print (length (filter (\(x,y,color) -> x < 0.5) res)) 
          send context $ mySquareList (toList (computeS res :: Array U DIM2 (Double,Double,Word32)):: [(Double,Double,Word32)])
          return ()
             
