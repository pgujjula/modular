{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, GADTs, Rank2Types, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Numeric.Modular
    ( withMod
    , mkMod
    , Mod
    ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (Nat, KnownNat, type (*), type (+), natVal)

data Mod (n :: Nat) = Mod Integer

reifyInteger :: Integer -> (forall n. (KnownNat n) => Proxy n -> w) -> w
reifyInteger 0 f = f (Proxy :: Proxy 0)
reifyInteger n f
    | even n    = reifyInteger (n `div` 2) (\(Proxy :: Proxy n) -> f (Proxy :: Proxy (n * 2)))
    | otherwise = reifyInteger (n - 1)     (\(Proxy :: Proxy n) -> f (Proxy :: Proxy (n + 1)))

getM :: forall m. (KnownNat m) => Mod m -> Integer
getM _ = natVal (Proxy :: Proxy m)

getV :: forall m. (KnownNat m) => Mod m -> Integer
getV (Mod k) = k

wm :: forall m. KnownNat m => (forall n. (KnownNat n) => Mod n) -> Proxy m -> Integer
wm k modProxy = (getV (k :: Mod m)) `rem` (natVal modProxy)

withMod :: Integer -> (forall n. (KnownNat n) => Mod n) -> Integer
withMod k m = reifyInteger k (wm m)

instance Eq (Mod m) where
    (==) (Mod a) (Mod b) = a == b

instance (KnownNat m) => Show (Mod m) where
    show k@(Mod a) = show a ++ " (mod " ++ show m ++ ")"
        where m = getM k

instance (KnownNat m) => Num (Mod m) where
    (+)    k@(Mod a) (Mod b) = Mod $ (a + b) `rem` (getM k)
    (*)    k@(Mod a) (Mod b) = Mod $ (a * b) `rem` (getM k)
    (-)    k@(Mod a) (Mod b) = Mod $ (a - b) `rem` (getM k)
    negate k@(Mod a) = Mod $ (negate a) `rem` (getM k)
    abs = id
    signum _ = 1
    fromInteger = mkMod

mkMod :: forall m. (KnownNat m) => Integer -> Mod m
mkMod n = Mod (n `rem` (natVal (Proxy :: Proxy m)))
