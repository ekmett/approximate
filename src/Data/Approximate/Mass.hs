{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Approximate.Mass
  ( Mass(..)
  , (|?), (&?), (^?)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Comonad
import Control.DeepSeq
import Control.Monad
import Data.Binary as Binary
import Data.Bytes.Serial as Bytes
import Data.Copointed
import Data.Data
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
#endif
import Data.Functor.Bind
import Data.Functor.Extend
import Data.Hashable (Hashable(..))
import Data.Hashable.Lifted (Hashable1(..))
import Data.Pointed
import Data.SafeCopy
import Data.Semigroup
import Data.Serialize as Serialize
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable
#endif
import Data.Vector.Generic as G
import Data.Vector.Generic.Mutable as M
import Data.Vector.Unboxed as U
import GHC.Generics
import Numeric.Log

-- | A quantity with a lower-bound on its probability mass. This represents
-- a 'probable value' as a 'Monad' that you can use to calculate progressively
-- less likely consequences.
--
-- /NB:/ These probabilities are all stored in the log domain. This enables us
-- to retain accuracy despite very long multiplication chains. We never add
-- these probabilities so the additional overhead of working in the log domain
-- is never incurred, except on transitioning in and out.
--
-- This is most useful for discrete types, such as
-- small 'Integral' instances or a 'Bounded' 'Enum' like
-- 'Bool'.
--
-- Also note that @('&?')@ and @('|?')@ are able to use knowledge about the
-- function to get better precision on their results than naively using
-- @'liftA2' ('&&')@
data Mass a = Mass {-# UNPACK #-} !(Log Double) a
  deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

instance Binary a => Binary (Mass a) where
  put (Mass p a) = Binary.put p >> Binary.put a
  get = Mass <$> Binary.get <*> Binary.get

instance Serialize a => Serialize (Mass a) where
  put (Mass p a) = Serialize.put p >> Serialize.put a
  get = Mass <$> Serialize.get <*> Serialize.get

instance Serialize a => SafeCopy (Mass a)

instance Hashable a => Hashable (Mass a)
instance Hashable1 Mass where
    liftHashWithSalt h s (Mass m x) = hashWithSalt s m `h` x

instance Serial1 Mass where
  serializeWith f (Mass p a) = serialize p >> f a
  deserializeWith m = Mass <$> deserialize <*> m

instance Serial a => Serial (Mass a) where
  serialize (Mass p a) = serialize p >> serialize a
  deserialize = Mass <$> deserialize <*> deserialize

instance Functor Mass where
  fmap f (Mass p a) = Mass p (f a)
  {-# INLINE fmap #-}

instance Foldable Mass where
  foldMap f (Mass _ a) = f a
  {-# INLINE foldMap #-}

newtype instance U.MVector s (Mass a) = MV_Mass (U.MVector s (Log Double,a))
newtype instance U.Vector (Mass a) = V_Mass (U.Vector (Log Double,a))

instance Unbox a => M.MVector U.MVector (Mass a) where
  basicLength (MV_Mass v) = M.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_Mass v) = MV_Mass $ M.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Mass v1) (MV_Mass v2) = M.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Mass `liftM` M.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate n (Mass p a) = MV_Mass `liftM` M.basicUnsafeReplicate n (p,a)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Mass v) i = uncurry Mass `liftM` M.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Mass v) i (Mass p a) = M.basicUnsafeWrite v i (p,a)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Mass v) = M.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_Mass v) (Mass p a) = M.basicSet v (p,a)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Mass v1) (MV_Mass v2) = M.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Mass v1) (MV_Mass v2) = M.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Mass v) n = MV_Mass `liftM` M.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}
#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_Mass v) = M.basicInitialize v
  {-# INLINE basicInitialize #-}
#endif

instance Unbox a => G.Vector U.Vector (Mass a) where
  basicUnsafeFreeze (MV_Mass v) = V_Mass `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Mass v) = MV_Mass `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Mass v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_Mass v) = V_Mass $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Mass v) i
                = uncurry Mass `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Mass mv) (V_Mass v) = G.basicUnsafeCopy mv v
  {-# INLINE basicUnsafeCopy #-}
  elemseq _ (Mass p a) z
     = G.elemseq (undefined :: U.Vector (Log Double)) p
     $ G.elemseq (undefined :: U.Vector a) a z
  {-# INLINE elemseq #-}

instance NFData a => NFData (Mass a) where
  rnf (Mass _ a) = rnf a `seq` ()
  {-# INLINE rnf #-}

instance Traversable Mass where
  traverse f (Mass p a) = Mass p <$> f a
  {-# INLINE traverse #-}

instance Apply Mass where
  (<.>) = (<*>)
  {-# INLINE (<.>) #-}

instance Pointed Mass where
  point = Mass 1
  {-# INLINE point #-}

instance Copointed Mass where
  copoint (Mass _ a) = a
  {-# INLINE copoint #-}

instance Applicative Mass where
  pure = Mass 1
  {-# INLINE pure #-}
  Mass p f <*> Mass q a = Mass (p * q) (f a)
  {-# INLINE (<*>) #-}

instance Monoid a => Monoid (Mass a) where
  mempty = Mass 1 mempty
  {-# INLINE mempty #-}
  mappend (Mass p a) (Mass q b) = Mass (p * q) (mappend a b)
  {-# INLINE mappend #-}

instance Semigroup a => Semigroup (Mass a) where
  Mass p a <> Mass q b = Mass (p * q) (a <> b)
  {-# INLINE (<>) #-}

instance Bind Mass where
  Mass p a >>- f = case f a of
    Mass q b -> Mass (p * q) b
  {-# INLINE (>>-) #-}

instance Monad Mass where
  return = pure
  {-# INLINE return #-}
  Mass p a >>= f = case f a of
    Mass q b -> Mass (p * q) b
  {-# INLINE (>>=) #-}

instance Extend Mass where
  duplicated (Mass n a) = Mass n (Mass n a)
  {-# INLINE duplicated #-}
  extended f w@(Mass n _) = Mass n (f w)
  {-# INLINE extended #-}

instance Comonad Mass where
  extract (Mass _ a) = a
  {-# INLINE extract #-}
  duplicate (Mass n a) = Mass n (Mass n a)
  {-# INLINE duplicate #-}
  extend f w@(Mass n _) = Mass n (f w)
  {-# INLINE extend #-}

instance ComonadApply Mass where
  (<@>)  = (<*>)
  {-# INLINE (<@>) #-}

infixl 6 ^?
infixr 3 &?
infixr 2 |?

-- | Calculate the logical @and@ of two booleans with confidence lower bounds.
(&?) :: Mass Bool -> Mass Bool -> Mass Bool
Mass p False &? Mass q False = Mass (max p q) False
Mass p False &? Mass _ True  = Mass p False
Mass _ True  &? Mass q False = Mass q False
Mass p True  &? Mass q True  = Mass (p * q) True
{-# INLINE (&?) #-}

-- | Calculate the logical @or@ of two booleans with confidence lower bounds.
(|?) :: Mass Bool -> Mass Bool -> Mass Bool
Mass p False |? Mass q False = Mass (p * q) False
Mass _ False |? Mass q True  = Mass q True
Mass p True  |? Mass _ False = Mass p True
Mass p True  |? Mass q True  = Mass (max p q) True
{-# INLINE (|?) #-}

-- | Calculate the exclusive @or@ of two booleans with confidence lower bounds.
(^?) :: Mass Bool -> Mass Bool -> Mass Bool
Mass p a ^? Mass q b = Mass (p * q) (xor a b) where
  xor True  True  = False
  xor False True  = True
  xor True  False = True
  xor False False = False
  {-# INLINE xor #-}
{-# INLINE (^?) #-}
