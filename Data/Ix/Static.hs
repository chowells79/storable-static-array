{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-|

'IxStatic' is a class that uses type-level constraints to generate the
values used by an 'Ix' instance.

This module contains instances of 'IxStatic' for types of kind 'Nat',
types of the promoted kind \'['Nat'], and promoted tuples of 'Nat' up
to 5 elements. This is the largest size of tuple that has an 'Ix'
instance.

There are also data types provided to simulate promoted tuples and
lists. These are less syntactically pleasant to use, but are sometimes
helpful. In particular, the single @'@ used by promoted types can
interfere with @CPP@ operation, so alternate means of specifying
multiple dimensions are provided.

-}
module Data.Ix.Static
       ( IxStatic(..)
       , fromNat
       , (:.)
       , Nil
       , D2
       , D3
       , D4
       , D5
       ) where

import GHC.TypeLits

import Data.Ix

import Data.Proxy

-- | A conversion function for converting type-level naturals to
-- value-level. This is being exposed to aid in the creation of
-- additional 'IxStatic' instances for those who might desire to do
-- so.
--
-- Haddock is currently eating the important qualification that the
-- type variable @n@ must have the kind 'Nat'. The 'KnownNat' instance is
-- automatically fulfilled for all types of kind 'Nat'. Its explicit
-- presence in the signature is an artifact of how GHC implements
-- dictionary passing and type erasure.
fromNat :: KnownNat n => proxy n -> Int
fromNat = fromInteger . natVal

-- | This class connects dimension description types with 'Ix'
-- index types and values.
class Ix (Index d) => IxStatic d where
    -- | The index type for this dimension description
    type Index d :: *
    -- | The concrete bounds for an array of this
    -- dimensionality, tagged with the dimensionality.
    bounds :: proxy d -> (Index d, Index d)

instance KnownNat a => IxStatic (a :: Nat) where
    type Index a = Int
    bounds _ = (0, fromNat (Proxy :: Proxy a) - 1)

instance KnownNat n => IxStatic ('[n] :: [Nat]) where
    type Index ('[n]) = Int
    bounds _ = (0, fromNat (Proxy :: Proxy n) - 1)

instance (KnownNat n, IxStatic (n2 ': ns)) =>
          IxStatic ((n ': n2 ': ns) :: [Nat]) where
    type Index (n ': n2 ': ns) = (Int, Index (n2 ': ns))
    bounds _ = ((0, b0), (fromNat (Proxy :: Proxy n) - 1, bn))
      where
        (b0, bn) = bounds (Proxy :: Proxy (n2 ': ns))

instance (KnownNat a, KnownNat b) => IxStatic ('(a, b) :: (Nat, Nat)) where
    type Index '(a, b) = (Int, Int)
    bounds _ = ((0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1))

instance (KnownNat a, KnownNat b, KnownNat c) =>
         IxStatic ('(a, b, c) :: (Nat, Nat, Nat)) where
    type Index '(a, b, c) = (Int, Int, Int)
    bounds _ = ((0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1))

instance (KnownNat a, KnownNat b, KnownNat c, KnownNat d) =>
         IxStatic ('(a, b, c, d) :: (Nat, Nat, Nat, Nat)) where
    type Index '(a, b, c, d) = (Int, Int, Int, Int)
    bounds _ = ((0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1))

instance (KnownNat a, KnownNat b, KnownNat c, KnownNat d, KnownNat e) =>
         IxStatic ('(a, b, c, d, e) :: (Nat, Nat, Nat, Nat, Nat)) where
    type Index '(a, b, c, d, e) = (Int, Int, Int, Int, Int)
    bounds _ = ((0, 0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1,
                 fromNat (Proxy :: Proxy e) - 1))

-- | ':.' is provided as an alternative means of constructing a
-- type-level list of dimensions. @DataKinds@-promoted lists are also
-- supported and easier to use in almost all cases. The exception is
-- when @CPP@ is involved, when a single @'@ on a line causes @CPP@ to
-- fail.
--
-- With @TypeOperators@ and @DataKinds@ enabled, @'StaticArray'
-- 'UArray' (2:.10:.25:.'Nil') 'Int'@ is equivalent to @'StaticArray'
-- 'UArray' \'[2,10,25] 'Int'@ and both wrap a @'UArray'
-- ('Int',('Int','Int')) 'Int'@ with bounds @((0,(0,0)),(1,(9,24)))@.
--
-- Neither promoted lists nor this approach support creating
-- 0-dimensional arrays, because they make no sense with
-- 'Foreign.Storable.Storable'.
data a :. b
infixr 3 :.

-- | 'Nil' is the terminator for type-level lists created with ':.'
data Nil

instance KnownNat n => IxStatic ((n :: Nat) :. Nil) where
    type Index (n :. Nil) = Int
    bounds _ = (0, fromNat (Proxy :: Proxy n) - 1)

instance (KnownNat n, IxStatic (n2 :. ns)) =>
          IxStatic ((n :: Nat) :. n2 :. ns) where
    type Index (n :. n2 :. ns) = (Int, Index (n2 :. ns))
    bounds _ = ((0, b0), (fromNat (Proxy :: Proxy n) - 1, bn))
      where
        (b0, bn) = bounds (Proxy :: Proxy (n2 :. ns))

-- | An alternative dimension type to promoted pairs, provided for
-- syntactic compatibility with @CPP@.
data D2 (a :: Nat) (b :: Nat)
instance (KnownNat a, KnownNat b) => IxStatic (D2 a b) where
    type Index (D2 a b) = (Int, Int)
    bounds _ = ((0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1))

-- | An alternative dimension type to promoted triples, provided for
-- syntactic compatibility with @CPP@.
data D3 (a :: Nat) (b :: Nat) (c :: Nat)
instance (KnownNat a, KnownNat b, KnownNat c) => IxStatic (D3 a b c) where
    type Index (D3 a b c) = (Int, Int, Int)
    bounds _ = ((0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1))

-- | An alternative dimension type to promoted 4-tuples, provided for
-- syntactic compatibility with @CPP@.
data D4 (a :: Nat) (b :: Nat) (c :: Nat) (d :: Nat)
instance (KnownNat a, KnownNat b, KnownNat c, KnownNat d) =>
         IxStatic (D4 a b c d) where
    type Index (D4 a b c d) = (Int, Int, Int, Int)
    bounds _ = ((0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1))

-- | An alternative dimension type to promoted 5-tuples, provided for
-- syntactic compatibility with @CPP@.
data D5 (a :: Nat) (b :: Nat) (c :: Nat) (d :: Nat) (e :: Nat)
instance (KnownNat a, KnownNat b, KnownNat c, KnownNat d, KnownNat e) =>
         IxStatic (D5 a b c d e) where
    type Index (D5 a b c d e) = (Int, Int, Int, Int, Int)
    bounds _ = ((0, 0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1,
                 fromNat (Proxy :: Proxy e) - 1))
