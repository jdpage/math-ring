{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Numeric.Ring.Finite.Modulo
    ( Zn()
    , modulo
    , makeZn
    , toIntegral
    , module Numeric.Ring
    ) where

import Text.Printf
import Numeric.Ring

data Zn a = Zn a a deriving (Eq)

modulo :: (Integral a) => a -> a -> a
modulo a b = ((a `mod` b) + b) `mod` b

makeZn :: (Integral a) => a -> a -> Zn a
makeZn n r = Zn n $ r `modulo` n

toIntegral :: (Integral a) => (Zn a) -> a
toIntegral (Zn _ r) = r

instance (Integral a) => Ring a (Zn a) where
    info (Zn n _) = n
    zero n = makeZn n 0
    unity n = makeZn n 1
    negate' (Zn n r) = makeZn n (-r)
    (Zn n1 r1) +: (Zn n2 r2)
        | n1 == n2   = makeZn n1 $ r1 + r2
        | otherwise  = error "moduli must be equal"
    (Zn n1 r1) *: (Zn n2 r2)
        | n1 == n2   = makeZn n1 $ r1 * r2
        | otherwise  = error "moduli must be equal"

instance (Show a, Integral a) => Show (Zn a) where
    show (Zn n r) = printf "%v (mod %v)" (show r) (show n)

