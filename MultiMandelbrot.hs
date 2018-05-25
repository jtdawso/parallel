--File: MultiMandelbrot.hs
--Author: Justin Dawson
--Not complete, startings of cloud haskell implementation

{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure

import Control.Monad
import Text.Printf
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import Graphics.Blank
import qualified Data.Vector.Unboxed as V
import Data.Complex
import qualified Data.Text as Text

import DistribUtils

-- <<Message
data Message = Ping (SendPort ProcessId)
  deriving (Typeable, Generic)

instance Binary Message
-- >>

-- <<pingServer
pingServer :: Process ()
pingServer = do
  Ping chan <- expect
  say $ printf "ping received from %s" (show chan)
  mypid <- getSelfPid
  sendChan chan mypid
-- >>

-- <<remotable
remotable ['pingServer]
-- >>

-- <<master
master :: [NodeId] -> Process ()
master peers = do

  ps <- forM peers $ \nid -> do
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)

  mapM_ monitor ps

  ports <- forM ps $ \pid -> do
    say $ printf "pinging %s" (show pid)
    (sendport,recvport) <- newChan      -- <1>
    send pid (Ping sendport)            -- <2>
    return recvport

  forM_ ports $ \port -> do             -- Wait for responses from each child in turn
     _ <- receiveChan port
     return ()

  say "All pongs successfully received"
  terminate
-- >>

-- <<main
main :: IO ()
main = distribMain master Main.__remoteTable
-- >>



mySquare x y color = do

 save()
 translate((x*300)+800,(y*300)+500)
 fillStyle  color
 fillRect(0,0,2,2)
 restore()
 

--intToHex n = (['0'..'9'] ++ ['A'..'F']) !! ((n `div` 16) ) 
intToHex n = (['A','A','B','B','C','C','D','D','E','E','F','F']) !! (if (n `div` 12) >= 12 then 11 else (n`div`12) ) 

mandelbrot2:: Double -> Double -> ((Double, Double),Int)
mandelbrot2 x y = let val = x :+ y
                      zs = take 255 $ iterate (\z -> z^2 +val) 0
                      iter = length $ takeWhile (\intermediate -> magnitude intermediate  < 2 ) $ zs
                  in ((x,y),iter)

main :: IO ()
main = blankCanvas 3001 { events = ["mousedown"] } $ \ context -> do
          let (w,h) = (width context, height context)
          print ("got size " :: String, (w,h))
          let v =  [(x,y)| y<-[1,0.995 .. -1], x <-[-2, -1.995 .. 0.5]]
          let res = map (\(x,y)->mandelbrot2 x y) $ v :: [((Double,Double),Int)]
       --   print res
          send context $ do
                sequence_ $ map (\((x,y),color)-> mySquare x y (Text.pack ("#"++(replicate 3 (intToHex color))) )) res 
                mySquare 10 10 "#F00"    

