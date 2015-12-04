{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank
import qualified Data.Vector.Unboxed as V
import Data.Complex
import qualified Data.Text as Text
import Control.Concurrent


mySquare x y color = do

 save()
 translate((x*300)+800,(y*300)+500)
 fillStyle  color
 fillRect(0,0,2,2)
 restore()
 
redYellow = ["F00","F10","F20","F30","F40","F50","F60","F70","F80","F90","FA0","FB0"]
blackGreen = ["000","010","020","030","040","050","060","070","080","090","0A0","0B0"]
blueGreen = ["0F0","0E1","0D2","0C3","0B4","0A5","096","087","078","069","05A","04B"]
yellowBlue = ["FF0","EE1","DD2","CC3","BB4","AA5","996","887","778","669","55A","44B"]
yellowRedBlue = ["FF0","FE0","FC0","FA0","F84","A75","096","087","078","069","05A","04B"]
blackGreenBlue = ["000","0A0","0A0","0C0","0F0","0C5","0A6","087","078","069","05A","04B"]


--intToHex n = (['0'..'9'] ++ ['A'..'F']) !! ((n `div` 16) ) 
--intToHex n = (['A','A','B','B','C','C','D','D','E','E','F','F']) !! (if (n `div` 12) > 12 then 11 else (n`div`12) ) 
intToHex n =  blackGreen !! (if (n `div` 12) >= 12 then 11 else (n`div`12) ) 

mandelbrot:: Double -> Double -> ((Double, Double),Int)
mandelbrot x y = let val = x :+ y
                     zs = take 255 $ iterate (\z -> z^2 +val) 0
                     iter = length $ takeWhile (\intermediate -> magnitude intermediate  < 2 ) $ zs
                  in ((x,y),iter)

 
main :: IO ()
main = blankCanvas 3000 { events = ["mousedown"] } $ \ context -> do
          let (w,h) = (width context, height context)
          print ("got size " :: String, (w,h))
          let v =  [(x,y)| y<-[1,0.9975 .. -1], x <-[-2, -1.9975 .. 0.5]]
        --  let res = map (\(x,y)->mandelbrot x y) $ v3 
          sequence $ map (forkIO . (\v-> let res = map (\(x,y)-> mandelbrot x y) v 
                      --         in send context $ sequence_ $ map (\((x,y),color)-> mySquare x y (Text.pack ("#"++(replicate 3 (intToHex color))) )) res)) vs 
                               in send context $ sequence_ $ map (\((x,y),color)-> mySquare x y (Text.pack ("#"++(intToHex color)) )) res)) vs 
          return ()
  --        send context $ do
  --              sequence_ $ map (\((x,y),color)-> mySquare x y (Text.pack ("#"++(replicate 3 (intToHex color))) )) res
  --              mySquare 10 10 "#F00"    
             
