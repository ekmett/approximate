{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Approximate.Type
  ( Approximate(Approximate)
  , HasApproximate(..)
  , exact
  , zero
  , one
  , withMin, withMax
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Data.Binary as Binary
import Data.Bytes.Serial as Bytes
import Data.Copointed
import Data.Data
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
#endif
import Data.Functor.Apply
import Data.Hashable (Hashable(..))
import Data.Hashable.Lifted (Hashable1(..))
import Data.Monoid
import Data.Pointed
import Data.SafeCopy
import Data.Serialize as Serialize
import Data.Vector.Generic as G
import Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed as U
import GHC.Generics
import Numeric.Log

-- | An approximate number, with a likely interval, an expected value and a lower bound on the @log@ of probability that the answer falls in the interval.
--
-- /NB:/ The probabilities associated with confidence are stored in the @log@ domain.
data Approximate a = Approximate
  { _confidence :: {-# UNPACK #-} !(Log Double)
  , _lo, _estimate, _hi :: a
  } deriving (Eq,Show,Read,Typeable,Data,Generic)

makeClassy ''Approximate

instance Binary a => Binary (Approximate a) where
  put (Approximate p l m h) = Binary.put p >> Binary.put l >> Binary.put m >> Binary.put h
  get = Approximate <$> Binary.get <*> Binary.get <*> Binary.get <*> Binary.get

instance Serialize a => Serialize (Approximate a) where
  put (Approximate p l m h) = Serialize.put p >> Serialize.put l >> Serialize.put m >> Serialize.put h
  get = Approximate <$> Serialize.get <*> Serialize.get <*> Serialize.get <*> Serialize.get

instance Serialize a => SafeCopy (Approximate a)

instance Hashable a => Hashable (Approximate a)
instance Hashable1 Approximate where
    liftHashWithSalt h s (Approximate c low est high) =
        hashWithSalt s c `h` low `h` est `h` high

instance Serial a => Serial (Approximate a)

instance Serial1 Approximate where
  serializeWith f (Approximate p l m h) = serialize p >> f l >> f m >> f h
  deserializeWith m = Approximate <$> deserialize <*> m <*> m <*> m

-- instance Storable a => Storable (Approximate a) where
--  sizeOf _ = sizeOf (undefined :: Double) + sizeOf (undefined :: a) * 3 --?

instance Unbox a => Unbox (Approximate a)

newtype instance U.MVector s (Approximate a) = MV_Approximate (U.MVector s (Log Double,a,a,a))
newtype instance U.Vector (Approximate a) = V_Approximate (U.Vector (Log Double,a,a,a))

instance Unbox a => M.MVector U.MVector (Approximate a) where
  basicLength (MV_Approximate v) = M.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_Approximate v) = MV_Approximate $ M.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Approximate v1) (MV_Approximate v2) = M.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Approximate `liftM` M.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate n (Approximate p l m h) = MV_Approximate `liftM` M.basicUnsafeReplicate n (p,l,m,h)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Approximate v) i = (\(p,l,m,h) -> Approximate p l m h) `liftM` M.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Approximate v) i (Approximate p l m h) = M.basicUnsafeWrite v i (p,l,m,h)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Approximate v) = M.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_Approximate v) (Approximate p l m h) = M.basicSet v (p,l,m,h)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Approximate v1) (MV_Approximate v2) = M.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Approximate v1) (MV_Approximate v2) = M.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Approximate v) n = MV_Approximate `liftM` M.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Approximate v) = M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

instance Unbox a => G.Vector U.Vector (Approximate a) where
  basicUnsafeFreeze (MV_Approximate v) = V_Approximate `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Approximate v) = MV_Approximate `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Approximate v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_Approximate v) = V_Approximate $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Approximate v) i
                = (\(p,l,m,h) -> Approximate p l m h) `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Approximate mv) (V_Approximate v) = G.basicUnsafeCopy mv v
  {-# INLINE basicUnsafeCopy #-}
  elemseq _ (Approximate p l m h) z
     = G.elemseq (undefined :: U.Vector (Log Double)) p
     $ G.elemseq (undefined :: U.Vector a) l
     $ G.elemseq (undefined :: U.Vector a) m
     $ G.elemseq (undefined :: U.Vector a) h z
  {-# INLINE elemseq #-}

instance NFData a => NFData (Approximate a) where
  rnf (Approximate _ l m h) = rnf l `seq` rnf m `seq` rnf h `seq` ()

instance Functor Approximate where
  fmap f (Approximate p l m h) = Approximate p (f l) (f m) (f h)
  {-# INLINE fmap #-}

instance Foldable Approximate where
  foldMap f (Approximate _ l m h) = f l `mappend` f m `mappend` f h
  {-# INLINE foldMap #-}

instance Traversable Approximate where
  traverse f (Approximate p l m h) = Approximate p <$> f l <*> f m <*> f h
  {-# INLINE traverse #-}

instance Copointed Approximate where
  copoint (Approximate _ _ a _) = a
  {-# INLINE copoint #-}

instance Pointed Approximate where
  point a = Approximate 1 a a a
  {-# INLINE point #-}

instance Apply Approximate where
  Approximate p lf mf hf <.> Approximate q la ma ha = Approximate (p * q) (lf la) (mf ma) (hf ha)
  {-# INLINE (<.>) #-}

instance Applicative Approximate where
  pure a = Approximate 1 a a a
  {-# INLINE pure #-}
  Approximate p lf mf hf <*> Approximate q la ma ha = Approximate (p * q) (lf la) (mf ma) (hf ha)
  {-# INLINE (<*>) #-}

withMin :: Ord a => a -> Approximate a -> Approximate a
withMin b r@(Approximate p l m h)
  | b <= l    = r
  | otherwise = Approximate p b (max b m) (max b h)
{-# INLINE withMin #-}

withMax :: Ord a => a -> Approximate a -> Approximate a
withMax b r@(Approximate p l m h)
  | h <= b = r
  | otherwise = Approximate p (min l b) (min m b) b
{-# INLINE withMax #-}

instance (Ord a, Num a) => Num (Approximate a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  m * n
    | is zero n || is one m = m
    | is zero m || is one n = n
    | otherwise = Approximate (m^.confidence * n^.confidence) (Prelude.minimum extrema) (m^.estimate * n^.estimate) (Prelude.maximum extrema) where
      extrema = (*) <$> [m^.lo,m^.hi] <*> [n^.lo,n^.hi]
  {-# INLINE (*) #-}
  negate (Approximate p l m h) = Approximate p (-h) (-m) (-l)
  {-# INLINE negate #-}
  Approximate p la ma ha - Approximate q lb mb hb = Approximate (p * q) (la - hb) (ma - mb) (ha - lb)
  {-# INLINE (-) #-}
  abs (Approximate p la ma ha) = Approximate p (min lb hb) (abs ma) (max lb hb) where
    lb = abs la
    hb = abs ha
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

exact :: Eq a => Prism' (Approximate a) a
exact = prism pure $ \ s -> case s of
  Approximate (Exp lp) a b c | lp == 0 && a == c -> Right b
  _ -> Left s
{-# INLINE exact #-}

zero :: (Num a, Eq a) => Prism' (Approximate a) ()
zero = exact.only 0
{-# INLINE zero #-}

one :: (Num a, Eq a) => Prism' (Approximate a) ()
one = exact.only 1
{-# INLINE one #-}

is :: Getting Any s a -> s -> Bool
is = has
{-# INLINE is #-}
