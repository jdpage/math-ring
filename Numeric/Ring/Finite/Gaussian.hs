{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Numeric.Ring.Finite.Gaussian
    ( Zni()
    , makeZni
    , onex, ix
    , modulus
    , re, im, re', im'
    , conjugate
    , magnitude
    , argument
    , enumerateZni
    , showPolar
    , module Numeric.Ring
    , module Numeric.Ring.Finite.Modulo
    ) where

import Text.Printf
import Numeric.Ring
import Numeric.Ring.Finite.Modulo

data I = I
type Zni a = Adjoined a (Zn a) I

instance (Show a, Integral a) => Show (Zni a) where
    show z =
        let rr = re z in
        let n = info rr in
        let r = toIntegral rr in
        let i = toIntegral $ im z in
        printf "%v + %vi (mod %v)" (show r) (show i) (show n)

instance Adjoinable I where
    r `apow` p
        | p `modulo` 4 == 0   = r `unsafeApow` 0
        | p `modulo` 4 == 1   = r `unsafeApow` 1
        | p `modulo` 4 == 2   = (negate' r) `unsafeApow` 0
        | otherwise           = (negate' r) `unsafeApow` 1

onex :: (Integral a) => Zn a -> Zni a
onex r = r `apow` 0

ix :: (Integral a) => Zn a -> Zni a
ix i = i `apow` 1

makeZni :: (Integral a) => a -> a -> a -> Zni a
makeZni n r i = (onex $ makeZn n r) +: (ix $ makeZn n i)

modulus :: (Integral a) => Zni a -> a
modulus z = info z

re :: (Integral a) => Zni a -> Zn a
re z = z `coeff` 0

im :: (Integral a) => Zni a -> Zn a
im z = z `coeff` 1

re' :: (Integral a) => Zni a -> Zni a
re' = onex . re

im' :: (Integral a) => Zni a -> Zni a
im' = ix . im

conjugate :: (Integral a) => Zni a -> Zni a
conjugate z = re' z -: im' z

magnitude :: (RealFloat a) => Zni Integer -> a
magnitude z =
    let r = toIntegral $ re z in
    let i = toIntegral $ im z in
    (fromInteger $ r * r + i * i) ** 0.5

argument :: (RealFloat a) => Zni Integer -> a
argument z =
    let r = toIntegral $ re z in
    let i = toIntegral $ im z in
    atan2 (fromInteger $ r) (fromInteger $ i)

enumerateZni :: (Integral a) => a -> [Zni a]
enumerateZni n = let ns = [0..(n - 1)] in [makeZni n r i | r <- ns, i <- ns]

showPolar :: Zni Integer -> String
showPolar z = printf "%.2f,%.2f;" (magnitude z :: Float) (argument z :: Float)

