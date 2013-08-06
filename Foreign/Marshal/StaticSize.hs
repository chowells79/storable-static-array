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

This module contains instances of 'StaticSize' for types of kind
'Nat', types of the promoted kind \'['Nat'], and promoted tuples of
'Nat' up to 13 elements. For instances not relying on promoted data
types, see the "Foreign.Marshal.StaticArray.Unpromoted" module.

-}
module Foreign.Marshal.StaticSize where

import GHC.TypeLits

import Data.Ix

import Data.Proxy
import Data.Tagged

-- | A conversion function for converting type-level naturals to
-- value-level. This is being exposed to aid in the creation of
-- additional 'StaticSize' instances for those who might desire to do
-- so.
--
-- Haddock is currently eating the important qualification that the
-- type variable @n@ must have the kind 'Nat'. The 'SingI' instance is
-- automatically fulfilled for all types of kind 'Nat'. Its explicit
-- presence in the signature is an artifact of how GHC implements
-- dictionary passing and type erasure.
fromNat :: forall (proxy :: Nat -> *) (n :: Nat). SingI n => proxy n -> Int
fromNat _ = fromInteger $ fromSing (sing :: Sing n)

-- | This class connects dimension description types with 'IArray'
-- index types and values.
class Ix (Bound d) => StaticSize d where
    -- | The bounding type for this dimension description
    type Bound d :: *
    -- | The concrete bounds for an array of this
    -- dimensionality, tagged with the dimensionality.
    extent :: Tagged d (Bound d, Bound d)

instance SingI n => StaticSize ('[n] :: [Nat]) where
    type Bound ('[n]) = Int
    extent = Tagged (0, fromNat (Proxy :: Proxy n) - 1)

instance (SingI n, StaticSize (n2 ': ns)) =>
          StaticSize ((n ': n2 ': ns) :: [Nat]) where
    type Bound (n ': n2 ': ns) = (Int, Bound (n2 ': ns))
    extent = Tagged ((0, b0), (fromNat (Proxy :: Proxy n) - 1, bn))
      where
        (b0, bn) = untag (extent :: Tagged (n2 ': ns)
                                           (Bound (n2 ': ns),
                                            Bound (n2 ': ns)))

instance SingI a => StaticSize (a :: Nat) where
    type Bound a = Int
    extent = Tagged (0, fromNat (Proxy :: Proxy a) - 1)

instance (SingI a, SingI b) => StaticSize ('(a, b) :: (Nat, Nat)) where
    type Bound '(a, b) = (Int, Int)
    extent = Tagged ((0, 0),
                     (fromNat (Proxy :: Proxy a) - 1,
                     fromNat (Proxy :: Proxy b) - 1))

instance (SingI a, SingI b, SingI c) =>
         StaticSize ('(a, b, c) :: (Nat, Nat, Nat)) where
    type Bound '(a, b, c) = (Int, Int, Int)
    extent = Tagged ((0, 0, 0),
                     (fromNat (Proxy :: Proxy a) - 1,
                      fromNat (Proxy :: Proxy b) - 1,
                      fromNat (Proxy :: Proxy c) - 1))

instance (SingI a, SingI b, SingI c, SingI d) =>
         StaticSize ('(a, b, c, d) :: (Nat, Nat, Nat, Nat)) where
    type Bound '(a, b, c, d) = (Int, Int, Int, Int)
    extent = Tagged ((0, 0, 0, 0),
                     (fromNat (Proxy :: Proxy a) - 1,
                      fromNat (Proxy :: Proxy b) - 1,
                      fromNat (Proxy :: Proxy c) - 1,
                      fromNat (Proxy :: Proxy d) - 1))

instance (SingI a, SingI b, SingI c, SingI d, SingI e) =>
         StaticSize ('(a, b, c, d, e) :: (Nat, Nat, Nat, Nat, Nat)) where
    type Bound '(a, b, c, d, e) = (Int, Int, Int, Int, Int)
    extent = Tagged ((0, 0, 0, 0, 0),
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

instance SingI n => StaticSize ((n :: Nat) :. Nil) where
    type Bound (n :. Nil) = Int
    extent = Tagged (0, fromNat (Proxy :: Proxy n) - 1)

instance (SingI n, StaticSize (n2 :. ns)) =>
          StaticSize ((n :: Nat) :. n2 :. ns) where
    type Bound (n :. n2 :. ns) = (Int, Bound (n2 :. ns))
    extent = Tagged ((0, b0), (fromNat (Proxy :: Proxy n) - 1, bn))
      where
        (b0, bn) = untag (extent :: Tagged (n2 :. ns)
                                    (Bound (n2 :. ns), Bound (n2 :. ns)))

-- | An alternative dimension type to promoted pairs, provided for
-- syntactic compatibility with @CPP@.
data D2 (a :: Nat) (b :: Nat)
instance (SingI a, SingI b) => StaticSize (D2 a b) where
    type Bound (D2 a b) = (Int, Int)
    extent = Tagged ((0, 0),
                     (fromNat (Proxy :: Proxy a) - 1,
                      fromNat (Proxy :: Proxy b) - 1))

-- | An alternative dimension type to promoted triples, provided for
-- syntactic compatibility with @CPP@.
data D3 (a :: Nat) (b :: Nat) (c :: Nat)
instance (SingI a, SingI b, SingI c) => StaticSize (D3 a b c) where
    type Bound (D3 a b c) = (Int, Int, Int)
    extent = Tagged ((0, 0, 0),
                     (fromNat (Proxy :: Proxy a) - 1,
                      fromNat (Proxy :: Proxy b) - 1,
                      fromNat (Proxy :: Proxy c) - 1))

-- | An alternative dimension type to promoted 4-tuples, provided for
-- syntactic compatibility with @CPP@.
data D4 (a :: Nat) (b :: Nat) (c :: Nat) (d :: Nat)
instance (SingI a, SingI b, SingI c, SingI d) => StaticSize (D4 a b c d) where
    type Bound (D4 a b c d) = (Int, Int, Int, Int)
    extent = Tagged ((0, 0, 0, 0),
                     (fromNat (Proxy :: Proxy a) - 1,
                      fromNat (Proxy :: Proxy b) - 1,
                      fromNat (Proxy :: Proxy c) - 1,
                      fromNat (Proxy :: Proxy d) - 1))

-- | An alternative dimension type to promoted 5-tuples, provided for
-- syntactic compatibility with @CPP@.
data D5 (a :: Nat) (b :: Nat) (c :: Nat) (d :: Nat) (e :: Nat)
instance (SingI a, SingI b, SingI c, SingI d, SingI e) =>
         StaticSize (D5 a b c d e) where
    type Bound (D5 a b c d e) = (Int, Int, Int, Int, Int)
    extent = Tagged ((0, 0, 0, 0, 0),
                     (fromNat (Proxy :: Proxy a) - 1,
                      fromNat (Proxy :: Proxy b) - 1,
                      fromNat (Proxy :: Proxy c) - 1,
                      fromNat (Proxy :: Proxy d) - 1,
                      fromNat (Proxy :: Proxy e) - 1))
