{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank
import qualified Data.Vector.Unboxed as V
import Data.Complex
import qualified Data.Text as Text

mySquare x y color = do

 save()
 translate((x*300)+800,(y*300)+500)
 fillStyle  color
 fillRect(0,0,2,2)
 restore()
 

--intToHex n = (['0'..'9'] ++ ['A'..'F']) !! ((n `div` 16) ) 
intToHex n = (['A','A','B','B','C','C','D','D','E','E','F','F']) !! (if (n `div` 12) >= 12 then 11 else (n`div`12) ) 

mandelbrot:: Double -> Double -> ((Double, Double),Int)
mandelbrot x y = let val = x :+ y
                      zs = take 255 $ iterate (\z -> z^2 +val) 0
                      iter = length $ takeWhile (\intermediate -> magnitude intermediate  < 2 ) $ zs
                  in ((x,y),iter)

 
main :: IO ()
main = blankCanvas 3001 $ \ context -> do
          let (w,h) = (width context, height context)
          print ("got size " :: String, (w,h))
          let v =  [(x,y)| y<-[1,0.995 .. -1], x <-[-2, -1.995 .. 0.5]]
          let res = map (\(x,y)->mandelbrot x y) $ v :: [((Double,Double),Int)]
       --   print res
          send context $ do
                sequence_ $ map (\((x,y),color)-> mySquare x y (Text.pack ("#"++(replicate 3 (intToHex color))) )) res
                mySquare 10 10 "#F00"    
                
