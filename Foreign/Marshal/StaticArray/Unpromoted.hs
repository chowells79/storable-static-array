{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|

This module contains a collection of data types for describing
dimensions of 'StaticArray' that do not depend on promoted lists and
tuples.

For convenience, it re-exports all of "Foreign.Marshal.StaticArray".

-}
module Foreign.Marshal.StaticArray.Unpromoted
       ( module Foreign.Marshal.StaticArray
       , (:.)
       , Nil
       , D2
       , D3
       , D4
       , D5
       , D6
       , D7
       , D8
       , D9
       , D10
       , D11
       , D12
       , D13
       ) where

import Data.Proxy

import GHC.TypeLits

import Foreign.Marshal.StaticArray

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
    extent _ = (0, fromNat (Proxy :: Proxy n) - 1)

instance (SingI n, StaticSize (n2 :. ns)) =>
          StaticSize ((n :: Nat) :. n2 :. ns) where
    type Bound (n :. n2 :. ns) = (Int, Bound (n2 :. ns))
    extent _ = ((0, b0), (fromNat (Proxy :: Proxy n) - 1, bn))
      where
        (b0, bn) = extent (undefined :: StaticArray a (n2 :. ns) b)

-- | An alternative dimension type to promoted pairs, provided for
-- syntactic compatibility with @CPP@.
data D2 (a :: Nat) (b :: Nat)
instance (SingI a, SingI b) => StaticSize (D2 a b) where
    type Bound (D2 a b) = (Int, Int)
    extent _ = ((0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1))

-- | An alternative dimension type to promoted triples, provided for
-- syntactic compatibility with @CPP@.
data D3 (a :: Nat) (b :: Nat) (c :: Nat)
instance (SingI a, SingI b, SingI c) => StaticSize (D3 a b c) where
    type Bound (D3 a b c) = (Int, Int, Int)
    extent _ = ((0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1))

-- | An alternative dimension type to promoted 4-tuples, provided for
-- syntactic compatibility with @CPP@.
data D4 (a :: Nat) (b :: Nat) (c :: Nat) (d :: Nat)
instance (SingI a, SingI b, SingI c, SingI d) => StaticSize (D4 a b c d) where
    type Bound (D4 a b c d) = (Int, Int, Int, Int)
    extent _ = ((0, 0, 0, 0),
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
    extent _ = ((0, 0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1,
                 fromNat (Proxy :: Proxy e) - 1))

-- | An alternative dimension type to promoted 6-tuples, provided for
-- syntactic compatibility with @CPP@.
data D6 (a :: Nat) (b :: Nat) (c :: Nat) (d :: Nat) (e :: Nat) (f :: Nat)
instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f) =>
         StaticSize (D6 a b c d e f) where
    type Bound (D6 a b c d e f) = (Int, Int, Int, Int, Int, Int)
    extent _ = ((0, 0, 0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1,
                 fromNat (Proxy :: Proxy e) - 1,
                 fromNat (Proxy :: Proxy f) - 1))

-- | An alternative dimension type to promoted 7-tuples, provided for
-- syntactic compatibility with @CPP@.
data D7 (a :: Nat) (b :: Nat) (c :: Nat) (d :: Nat) (e :: Nat) (f :: Nat)
     (g :: Nat)
instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g) =>
         StaticSize (D7 a b c d e f g) where
    type Bound (D7 a b c d e f g) = (Int, Int, Int, Int, Int, Int, Int)
    extent _ = ((0, 0, 0, 0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1,
                 fromNat (Proxy :: Proxy e) - 1,
                 fromNat (Proxy :: Proxy f) - 1,
                 fromNat (Proxy :: Proxy g) - 1))

-- | An alternative dimension type to promoted 8-tuples, provided for
-- syntactic compatibility with @CPP@.
data D8 (a :: Nat) (b :: Nat) (c :: Nat) (d :: Nat) (e :: Nat) (f :: Nat)
     (g :: Nat) (h :: Nat)
instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h) =>
         StaticSize (D8 a b c d e f g h) where
    type Bound (D8 a b c d e f g h) = (Int, Int, Int, Int, Int, Int, Int, Int)
    extent _ = ((0, 0, 0, 0, 0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1,
                 fromNat (Proxy :: Proxy e) - 1,
                 fromNat (Proxy :: Proxy f) - 1,
                 fromNat (Proxy :: Proxy g) - 1,
                 fromNat (Proxy :: Proxy h) - 1))

-- | An alternative dimension type to promoted 9-tuples, provided for
-- syntactic compatibility with @CPP@.
data D9 (a :: Nat) (b :: Nat) (c :: Nat) (d :: Nat) (e :: Nat) (f :: Nat)
     (g :: Nat) (h :: Nat) (i :: Nat)
instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h, SingI i) =>
         StaticSize (D9 a b c d e f g h i) where
    type Bound (D9 a b c d e f g h i) =
        (Int, Int, Int, Int, Int, Int, Int, Int, Int)
    extent _ = ((0, 0, 0, 0, 0, 0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1,
                 fromNat (Proxy :: Proxy e) - 1,
                 fromNat (Proxy :: Proxy f) - 1,
                 fromNat (Proxy :: Proxy g) - 1,
                 fromNat (Proxy :: Proxy h) - 1,
                 fromNat (Proxy :: Proxy i) - 1))

-- | An alternative dimension type to promoted 10-tuples, provided for
-- syntactic compatibility with @CPP@.
data D10 (a :: Nat) (b :: Nat) (c :: Nat) (d :: Nat) (e :: Nat) (f :: Nat)
     (g :: Nat) (h :: Nat) (i :: Nat) (j :: Nat)
instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h, SingI i, SingI j) =>
         StaticSize (D10 a b c d e f g h i j) where
    type Bound (D10 a b c d e f g h i j) =
        (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
    extent _ = ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1,
                 fromNat (Proxy :: Proxy e) - 1,
                 fromNat (Proxy :: Proxy f) - 1,
                 fromNat (Proxy :: Proxy g) - 1,
                 fromNat (Proxy :: Proxy h) - 1,
                 fromNat (Proxy :: Proxy i) - 1,
                 fromNat (Proxy :: Proxy j) - 1))

-- | An alternative dimension type to promoted 11-tuples, provided for
-- syntactic compatibility with @CPP@.
data D11 (a :: Nat) (b :: Nat) (c :: Nat) (d :: Nat) (e :: Nat) (f :: Nat)
     (g :: Nat) (h :: Nat) (i :: Nat) (j :: Nat) (k :: Nat)
instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h, SingI i, SingI j, SingI k) =>
         StaticSize (D11 a b c d e f g h i j k) where
    type Bound (D11 a b c d e f g h i j k) =
        (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
    extent _ = ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1,
                 fromNat (Proxy :: Proxy e) - 1,
                 fromNat (Proxy :: Proxy f) - 1,
                 fromNat (Proxy :: Proxy g) - 1,
                 fromNat (Proxy :: Proxy h) - 1,
                 fromNat (Proxy :: Proxy i) - 1,
                 fromNat (Proxy :: Proxy j) - 1,
                 fromNat (Proxy :: Proxy k) - 1))

-- | An alternative dimension type to promoted 12-tuples, provided for
-- syntactic compatibility with @CPP@.
data D12 (a :: Nat) (b :: Nat) (c :: Nat) (d :: Nat) (e :: Nat) (f :: Nat)
     (g :: Nat) (h :: Nat) (i :: Nat) (j :: Nat) (k :: Nat) (l :: Nat)
instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h, SingI i, SingI j, SingI k, SingI l) =>
         StaticSize (D12 a b c d e f g h i j k l) where
    type Bound (D12 a b c d e f g h i j k l) =
        (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
    extent _ = ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1,
                 fromNat (Proxy :: Proxy e) - 1,
                 fromNat (Proxy :: Proxy f) - 1,
                 fromNat (Proxy :: Proxy g) - 1,
                 fromNat (Proxy :: Proxy h) - 1,
                 fromNat (Proxy :: Proxy i) - 1,
                 fromNat (Proxy :: Proxy j) - 1,
                 fromNat (Proxy :: Proxy k) - 1,
                 fromNat (Proxy :: Proxy l) - 1))

-- | An alternative dimension type to promoted 13-tuples, provided for
-- syntactic compatibility with @CPP@.
data D13 (a :: Nat) (b :: Nat) (c :: Nat) (d :: Nat) (e :: Nat) (f :: Nat)
     (g :: Nat) (h :: Nat) (i :: Nat) (j :: Nat) (k :: Nat) (l :: Nat)
     (m :: Nat)
instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h, SingI i, SingI j, SingI k, SingI l, SingI m) =>
         StaticSize (D13 a b c d e f g h i j k l m) where
    type Bound (D13 a b c d e f g h i j k l m) =
        (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
    extent _ = ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1,
                 fromNat (Proxy :: Proxy e) - 1,
                 fromNat (Proxy :: Proxy f) - 1,
                 fromNat (Proxy :: Proxy g) - 1,
                 fromNat (Proxy :: Proxy h) - 1,
                 fromNat (Proxy :: Proxy i) - 1,
                 fromNat (Proxy :: Proxy j) - 1,
                 fromNat (Proxy :: Proxy k) - 1,
                 fromNat (Proxy :: Proxy l) - 1,
                 fromNat (Proxy :: Proxy m) - 1))
