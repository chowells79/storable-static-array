{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-|

This module defines 'StaticArray', a simple wrapper around instances
of 'IArray' with its dimensions encoded in the type. 'StaticArray'
provides a 'Storable' instance that uses the type-level dimensions,
and significantly eases writing FFI bindings to fixed-size native
arrays. For example, @'StaticArray' 'UArray' 10 CInt@ has a 'Storable'
instance that is directly compatible with @int foo[10]@ in native
code.

Multidimensional native arrays are also supported. @'StaticArray'
'UArray' \'(10,20,100) CUChar@ is compatible with @unsigned char
foo[10][20][100]@. Note the leading @\'@ before the tuple containing
the dimensions. It marks it as a @DataKinds@ promoted tuple, necessary
to store the dimensions.

To operate on the contents of a 'StaticArray', use
'toArray'. 'toArray' returns the backing array with the correct type
and index values already in place. For example, the result of
'toArray' on a @'StaticArray' 'UArray' \'(10,20,100) CUChar@ is a
@'UArray' (Int, Int, Int) CUChar@ with its bounds set to
@((0,0,0),(9,19,99))@.

-}
module Foreign.Marshal.StaticArray
       ( -- * Basic interface
         StaticArray
       , toArray
       , staticBounds
       , staticArray
       , listStaticArray
         -- * Adding new Storable instances
         -- $NewStorable
       , sizeOf'
       , alignment'
       , poke'
       , peek'
         -- * Adding new StaticSize instances
         -- $NewStaticSize
       , fromNat
       , StaticSize(..)
       ) where

import GHC.TypeLits

import Control.Monad

import Data.Array            (Array)
import Data.Array.Base
import Data.Array.IO  hiding (unsafeFreeze)
import Data.Functor          ((<$>))
import Data.Proxy
import Data.Tagged

import Foreign.Storable      (Storable(..))
import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr           (Ptr, castPtr)


-- | A minimal array wrapper that encodes the full dimensions of the
-- array in the type. Intended for interfacing with
-- (possibly-)multidimensional arrays of fixed size in native code.
--
-- The constructor is not exported to prevent creating a 'StaticArray'
-- with a size that doesn't match its dimensions.
newtype StaticArray backing dimensions (elements :: *) =
    StaticArray {
        -- | Returns the backing 'Array' of this 'StaticArray'.
        toArray :: backing (Bound dimensions) elements
        }
    deriving Eq

instance (Ix (Bound d), Show e) => Show (StaticArray Array d e) where
    show = ("listStaticArray " ++) . show . elems . toArray

instance (IArray UArray e, Ix (Bound d), Show e) =>
         Show (StaticArray UArray d e) where
    show = ("listStaticArray " ++) . show . elems . toArray

-- | Get the compile-time bounds from a 'StaticArray'. Does not examine its
-- argument.
{-# INLINEABLE staticBounds #-}
staticBounds :: forall b d e. StaticSize d =>
                StaticArray b d e -> (Bound d, Bound d)
staticBounds _ = untag (extent :: Tagged d (Bound d, Bound d))

-- | Create a new 'StaticArray' from a list of indices and
-- elements. This has all the semantic caveats of 'array', except that
-- the bounds are as good as those provided by the 'StaticSize'
-- instance.
{-# INLINEABLE staticArray #-}
staticArray :: (Ix (Bound d), IArray b e, StaticSize d) =>
               [(Bound d, e)] -> StaticArray b d e
staticArray ls = let a = StaticArray $ array (staticBounds a) ls in a

-- | Create a new 'StaticArray' from a list of elements in index
-- order. Implemented in terms of 'listArray'.
{-# INLINEABLE listStaticArray #-}
listStaticArray :: (StaticSize d, Ix (Bound d), IArray b e) =>
                   [e] -> StaticArray b d e
listStaticArray ls = let a = StaticArray $ listArray (staticBounds a) ls in a


------------------------------------------------------------------------
-- $NewStorable
--
-- This module only has 'Storable' instances for 'UArray' and 'Array'
-- as backing types. This is the result of ensuring that 'peek' is not
-- implemented with an additional copy. The mutable temporary array
-- needs to have a representation compatible with that of the result
-- array to avoid that extra copy.
--
-- The following functions provide a minimum complete, correct
-- 'Storable' implementation for 'StaticArray'. The helper function
-- required by 'peek'' is the part necessary for efficient
-- implementations which prevent creation of a fully polymorphic
-- instance.

-- | Get the size, in bytes, of the native representation of this
-- 'StaticArray'.
{-# INLINEABLE sizeOf' #-}
sizeOf' :: forall b d e. (StaticSize d, Ix (Bound d), Storable e) =>
           StaticArray b d e -> Int
sizeOf' a = sizeOf (undefined :: e) * rangeSize (staticBounds a)

-- | Get the alignment, in bytes, of the native representation of this
-- 'StaticArray'
{-# INLINEABLE alignment' #-}
alignment' :: forall b d e. Storable e => StaticArray b d e -> Int
alignment' _ = alignment (undefined :: e)

-- | Write the contents of this 'StaticArray' to the given location in
-- memory.
{-# INLINEABLE poke' #-}
poke' :: forall b d e. (Ix (Bound d), IArray b e, Storable e) =>
         Ptr (StaticArray b d e) -> StaticArray b d e -> IO ()
poke' dst' arr = do
        let a = toArray arr
            b = bounds a
            dst = castPtr dst'
        forM_ [0 .. rangeSize b - 1] $ \i ->
            poke (advancePtr dst i) $ unsafeAt a i

-- | Create a new 'StaticArray' from the contents of the given
-- location in memory. Uses a temporary mutable array to build up the
-- result, then freezes it. The first argument is the
-- freezing function. Non-copying implementations of 'unsafeFreeze'
-- are safe as this argument, and preferred.
{-# INLINEABLE peek' #-}
peek' :: forall b d e m . (StaticSize d, Ix (Bound d), Storable e,
                           IArray b e, MArray m e IO) =>
         (m (Bound d) e -> IO (b (Bound d) e)) ->
         Ptr (StaticArray b d e) ->
         IO (StaticArray b d e)
peek' freeze' src' = do
    rec let b = staticBounds arr
        m <- newArray_ b

        let src = castPtr src'
        forM_ [0 .. rangeSize b - 1] $ \i -> do
            x <- peek $ advancePtr src i
            unsafeWrite m i x

        arr <- StaticArray <$> freeze' m
    return arr

instance (StaticSize d, Ix (Bound d), Storable e,
          IArray UArray e, MArray IOUArray e IO) =>
         Storable (StaticArray UArray d e) where
    {-# INLINEABLE sizeOf#-}
    sizeOf = sizeOf'
    {-# INLINEABLE alignment #-}
    alignment = alignment'
    {-# INLINEABLE poke #-}
    poke = poke'
    {-# INLINEABLE peek #-}
    peek = peek' (unsafeFreeze :: IOUArray (Bound d) e ->
                                  IO (UArray (Bound d) e))

instance (StaticSize d, Ix (Bound d), Storable e) =>
         Storable (StaticArray Array d e) where
    {-# INLINEABLE sizeOf #-}
    sizeOf = sizeOf'
    {-# INLINEABLE alignment #-}
    alignment = alignment'
    {-# INLINEABLE poke #-}
    poke = poke'
    {-# INLINEABLE peek #-}
    peek = peek' (unsafeFreeze :: IOArray (Bound d) e ->
                                  IO (Array (Bound d) e))


------------------------------------------------------------------------
-- $NewStaticSize
--
-- This module contains instances of 'StaticSize' for types of kind
-- 'Nat', types of the promoted kind \'['Nat'], and promoted tuples of
-- 'Nat' up to 13 elements. For instances not relying on promoted data
-- types, see the "Foreign.Marshal.StaticArray.Unpromoted" module.

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
class StaticSize d where
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

instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f) =>
         StaticSize ('(a, b, c, d, e, f) ::
                     (Nat, Nat, Nat, Nat, Nat, Nat)) where
    type Bound '(a, b, c, d, e, f) = (Int, Int, Int, Int, Int, Int)
    extent = Tagged ((0, 0, 0, 0, 0, 0),
                     (fromNat (Proxy :: Proxy a) - 1,
                      fromNat (Proxy :: Proxy b) - 1,
                      fromNat (Proxy :: Proxy c) - 1,
                      fromNat (Proxy :: Proxy d) - 1,
                      fromNat (Proxy :: Proxy e) - 1,
                      fromNat (Proxy :: Proxy f) - 1))

instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g) =>
         StaticSize ('(a, b, c, d, e, f, g) ::
                     (Nat, Nat, Nat, Nat, Nat, Nat, Nat)) where
    type Bound '(a, b, c, d, e, f, g) = (Int, Int, Int, Int, Int, Int, Int)
    extent = Tagged ((0, 0, 0, 0, 0, 0, 0),
                     (fromNat (Proxy :: Proxy a) - 1,
                      fromNat (Proxy :: Proxy b) - 1,
                      fromNat (Proxy :: Proxy c) - 1,
                      fromNat (Proxy :: Proxy d) - 1,
                      fromNat (Proxy :: Proxy e) - 1,
                      fromNat (Proxy :: Proxy f) - 1,
                      fromNat (Proxy :: Proxy g) - 1))

instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h) =>
         StaticSize ('(a, b, c, d, e, f, g, h) ::
                     (Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat)) where
    type Bound '(a, b, c, d, e, f, g, h) =
        (Int, Int, Int, Int, Int, Int, Int, Int)
    extent = Tagged ((0, 0, 0, 0, 0, 0, 0, 0),
                     (fromNat (Proxy :: Proxy a) - 1,
                      fromNat (Proxy :: Proxy b) - 1,
                      fromNat (Proxy :: Proxy c) - 1,
                      fromNat (Proxy :: Proxy d) - 1,
                      fromNat (Proxy :: Proxy e) - 1,
                      fromNat (Proxy :: Proxy f) - 1,
                      fromNat (Proxy :: Proxy g) - 1,
                      fromNat (Proxy :: Proxy h) - 1))

instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h, SingI i) =>
         StaticSize ('(a, b, c, d, e, f, g, h, i) ::
                     (Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat)) where
    type Bound '(a, b, c, d, e, f, g, h, i) =
        (Int, Int, Int, Int, Int, Int, Int, Int, Int)
    extent = Tagged ((0, 0, 0, 0, 0, 0, 0, 0, 0),
                     (fromNat (Proxy :: Proxy a) - 1,
                      fromNat (Proxy :: Proxy b) - 1,
                      fromNat (Proxy :: Proxy c) - 1,
                      fromNat (Proxy :: Proxy d) - 1,
                      fromNat (Proxy :: Proxy e) - 1,
                      fromNat (Proxy :: Proxy f) - 1,
                      fromNat (Proxy :: Proxy g) - 1,
                      fromNat (Proxy :: Proxy h) - 1,
                      fromNat (Proxy :: Proxy i) - 1))

instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h, SingI i, SingI j) =>
         StaticSize ('(a, b, c, d, e, f, g, h, i, j) ::
                     (Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat)) where
    type Bound '(a, b, c, d, e, f, g, h, i, j) =
        (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
    extent = Tagged ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
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

instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h, SingI i, SingI j, SingI k) =>
         StaticSize ('(a, b, c, d, e, f, g, h, i, j, k) ::
                     (Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat))
      where
    type Bound '(a, b, c, d, e, f, g, h, i, j, k) =
        (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
    extent = Tagged ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
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

instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h, SingI i, SingI j, SingI k, SingI l) =>
         StaticSize ('(a, b, c, d, e, f, g, h, i, j, k, l) ::
                     (Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat,
                      Nat)) where
    type Bound '(a, b, c, d, e, f, g, h, i, j, k, l) =
        (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
    extent = Tagged ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
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

instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h, SingI i, SingI j, SingI k, SingI l, SingI m) =>
         StaticSize ('(a, b, c, d, e, f, g, h, i, j, k, l, m) ::
                     (Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat,
                      Nat, Nat)) where
    type Bound '(a, b, c, d, e, f, g, h, i, j, k, l, m) =
        (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
    extent = Tagged ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
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
