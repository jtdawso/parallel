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

convert :: Word32 -> [Word8]
convert x = let words = [ P.fromIntegral (x `Bits.shiftR` 16)
                        , P.fromIntegral (x `Bits.shiftR` 8)
                        , P.fromIntegral x
                        , 255]
             in words



mandelbrot
    ::  forall a. (Elt a, IsFloating a)
    => Exp a
    -> (Exp a,Exp a,Exp a, Exp a)
    -> Int
    -> Acc (Array DIM2 Word8)
mandelbrot stepA (xmin,ymin,xmax,ymax) depth =
  generate ((index2 screenY screenX))
           (\ix -> let c = initial ix
                       iter = A.snd $ A.while (\zi -> A.snd zi A.<* lIMIT &&* dot (A.fst zi) A.<* 4) (\zi -> lift1 (next c) zi) (lift (c, constant 0))
                    in  (A.fromIntegral iter)
           ) 
                                       
  where
    -- The view plane
    sizex   :: Exp a          =  stepA
    sizey   :: Exp a          =  stepA
    screenX :: Exp Int        =  (abs . A.round) $ (xmax - xmin) / stepA
    screenY :: Exp Int        =  (abs . A.round) $ (ymin -ymax) / stepA
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
          let step = 0.000625
          let (xmin,ymin,xmax,ymax) = (-2,-1,0.5,1)
          let res =  mandelbrot (A.constant step) (A.constant xmin::Exp Double,A.constant ymin :: Exp Double ,A.constant xmax:: Exp Double,A.constant ymax:: Exp Double) 255 
          let h :: Int =  (abs . P.round) $ (ymax - ymin) / ( step )
          let w ::Int= (abs . P.round) $ (xmax - xmin) / ( step ) 
          let ans = run res 
          let iters ::[Word32]= P.map (\v -> let word = P.fromIntegral v 
                                 in  BS.foldl (\acc x -> (acc `Bits.shiftL` 8) .|. (P.fromIntegral x)) zeroBits $ BS.pack $ [word,word,word]) $ toList ans
          send context $ putImageData (ImageData (w) (h) (V.fromList (P.concatMap convert iters)), [0,0])
 
          stop <- getCurrentTime
          print $ diffUTCTime stop start
