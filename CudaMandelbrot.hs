{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Array.Accelerate.CUDA (run)
import Data.Array.Accelerate as A
import Prelude as P
import Data.Array.Accelerate.Data.Complex
import Data.Time
import Graphics.Blank
import qualified Data.ByteString as BS
import Data.Bits as Bits
import qualified Data.Vector.Unboxed as V

step =  0.0025

mandelbrot
    ::  forall a. (Elt a, IsFloating a) => (Exp a,Exp a,Exp a, Exp a)
    -> Int
    -> Acc (Array DIM2 Int32)
mandelbrot (xmin,ymin,xmax,ymax) depth =
  generate (constant (Z:.screenY:.screenX))
           (\ix -> let c = initial ix
                       iter = A.snd $ A.while (\zi -> A.snd zi A.<* lIMIT &&* dot (A.fst zi) A.<* 4) (\zi -> lift1 (next c) zi) (lift (c, constant 0))
                    in  iter
           ) 
                                       
  where
    -- The view plane
    stepA   :: Exp a          = constant (A.fromIntegral step)
    sizex   :: Exp a          =  stepA
    sizey   :: Exp a          =  stepA
    screenX :: Int          =  P.fromIntegral $ ((1+) . abs . A.round) $ (xmax - xmin) / stepA
    screenY :: Int          =  P.fromIntegral $ ((1+) . abs . A.round) $ (ymin -ymax) / stepA
    -- initial conditions for a given pixel in the window, translated to the
    -- corresponding point in the complex plane
    initial ::  Exp DIM2 -> Exp (Complex a)
    initial ix = lift ( (xmin + (x * sizex)) :+ (ymin + (y * sizey)) )
      where
        pr :: Exp (Int, Int)= unindex2 ix
        x :: Exp a = A.fromIntegral (A.snd pr :: Exp Int)
        y :: Exp a = A.fromIntegral (A.fst pr :: Exp Int)

    -- take a single step of the iteration
    next :: Exp (Complex a) -> (Exp (Complex a), Exp Int32) -> (Exp (Complex a), Exp Int32)
    next c (z, i) = (c + (z * z), i+1)

    dot c = let r :+ i = unlift c
            in  r*r + i*i

    lIMIT = P.fromIntegral 255

main :: IO()
main = blankCanvas 3000 {middleware=[]} $ \ context -> do
          putStrLn "Start Request"
          start <- getCurrentTime
          let res =  mandelbrot (A.constant (-2)::Exp Double,A.constant (-1) :: Exp Double ,A.constant 0.5:: Exp Double,A.constant 1:: Exp Double) 255 
          let h =  ((1+) . abs . P.round) $ ((-1) -1) / (step)
          let w = ((1+) . abs . P.round) $ (0.5 -(-2)) / (step)
          let ans = run res 
          let iters = P.map (\v -> let word = P.fromIntegral v 
                                 in  BS.foldl (\acc x -> (acc `Bits.shiftL` 8) .|. (P.fromIntegral x)) zeroBits $ BS.pack $ [word,0,word])  $ toList ans
          send context $ putImageData (ImageData w h (V.fromList (iters)), [0,0])
 
          stop <- getCurrentTime
          print $ diffUTCTime stop start
