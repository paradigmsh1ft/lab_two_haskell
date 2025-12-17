{- cabal:
    build-depends: base, parallel
    ghc-options: -O2 -threaded -rtsopts
-}

module Main where

import Control.Parallel.Strategies (parMap, rseq)
import System.Environment (getArgs)
import Text.Printf (printf)


myFunc :: Double -> Double
myFunc x = 4.0 / (1.0 + x * x)


trapezoid :: (Double -> Double) -> Double -> Double -> Double
trapezoid f a b = (f a + f b) * (b - a) / 2

adaptiveIntegrate :: (Double -> Double) -> Double -> Double -> Double -> Double
adaptiveIntegrate f a b eps =
    let mid = (a + b) / 2
        whole = trapezoid f a b
        left  = trapezoid f a mid
        right = trapezoid f mid b
    in if abs (whole - (left + right)) < eps
       then left + right
       else adaptiveIntegrate f a mid (eps/2) + adaptiveIntegrate f mid b (eps/2)

parallelIntegrate :: (Double -> Double) -> Double -> Double -> Double -> Int -> Double
parallelIntegrate f a b eps numChunks = sum results
  where
    step = (b - a) / fromIntegral numChunks
    intervals = [ (a + i*step, a + (i+1)*step) | i <- [0 .. fromIntegral numChunks - 1] ]
    results = parMap rseq (\(start, end) -> adaptiveIntegrate f start end (eps / fromIntegral numChunks)) intervals

main :: IO ()
main = do
    let a = 0.0
    let b = 1.0
    let epsilon = 1e-9   
    let chunks = 2000   


    
    let result = parallelIntegrate myFunc a b epsilon chunks
    printf "Result: %.10f\n" result