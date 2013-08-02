{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
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
the dimensions. It marks it as a @DataKinds@ lifted tuple, necessary
to store the dimensions.

To operate on the contents of a 'StaticArray', use
'toArray'. 'toArray' returns the backing array with the correct type
and index values already in place. For example, the result of
'toArray' on a @'StaticArray' 'UArray' \'(10,20,100) CUChar@ is a
@'UArray' (Int, Int, Int) CUChar@ with its bounds set to
@((0,0,0),(9,19,99))@.

-}
module Foreign.Marshal.StaticArray
       ( Mutable
       , StaticArray
       , staticArray
       , listStaticArray
       , toArray
       , StaticSize(..)
       , fromNat
       ) where

import GHC.TypeLits

import Control.Monad

import Data.Array.Base
import Data.Array            (Array)
import Data.Array.IO  hiding (unsafeFreeze)
import Data.Functor          ((<$>))
import Data.Proxy            (Proxy(..))

import Foreign.Storable      (Storable(..))
import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr           (castPtr)


-- | A minimal wrapper for instances of 'IArray' that encodes the full
-- dimensions of the array in the type. Intended for interfacing with
-- (possibly-)multidimensional arrays of fixed size in native code.
--
-- The constructor is not exported to prevent creating a StaticArray
-- with a size that doesn't match its dimensions.
newtype StaticArray backing dimensions elements =
    StaticArray {
        -- | Returns the backing 'Array' of this 'StaticArray'.
        toArray :: backing (Bound dimensions) elements
        }
    deriving (Eq, Show)

-- | This class connects dimension description types with 'IArray'
-- index types and values. Instances are provided for up to 13
-- dimensions as tuples. Additionally, there is support for unlimited
-- dimensions via a list of dimensions. This results in nested pairs
-- for the index type.
class StaticSize d where
    -- | The bounding type for this dimension description
    type Bound d :: *
    -- | The concrete bounds for an array of this
    -- dimensionality. Implementations of this function should not
    -- examine their argument in any way.
    extent :: StaticArray b d e -> (Bound d, Bound d)

-- | Create a new 'StaticArray' from a list of indices and
-- elements. This has all the semantic caveats of 'array', except that
-- the bounds are as good as those provided by the 'StaticSize'
-- instance.
staticArray :: (Ix (Bound d), IArray b e, StaticSize d) =>
               [(Bound d, e)] -> StaticArray b d e
staticArray ls = let a = StaticArray $ array (extent a) ls in a

-- | Create a new 'StaticArray' from a list of elements in index
-- order. Implemented in terms of 'listArray'.
listStaticArray :: (StaticSize d, Ix (Bound d), IArray b e) =>
                   [e] -> StaticArray b d e
listStaticArray ls = let a = StaticArray $ listArray (extent a) ls in a


-- | The 'Mutable' type family is used to associate instances of
-- 'IArray' with instances of 'MArray' that give non-copying
-- 'unsafeFreeze'. This is used to increase the efficiency of 'peek'
-- in 'StaticArray' 's 'Storable' instance.
--
-- If you're somehow using an instance of 'IArray' other than 'Array'
-- or 'UArray', you'll need to add a type instance for your type. If
-- it supports non-copying 'unsafeFreeze', the type instance should
-- return the type constructor for the appropriate 'MArray'
-- instance. Otherwise, just have it return 'IOArray'
type family Mutable (a :: * -> * -> *) :: * -> * -> *
type instance Mutable Array = IOArray
type instance Mutable UArray = IOUArray

instance (StaticSize d, Ix (Bound d), Storable e, IArray b e,
          MArray (Mutable b) e IO) =>
         Storable (StaticArray b d e) where
    sizeOf a = sizeOf (undefined :: e) * rangeSize (extent a)
    alignment _ = alignment (undefined :: e)
    peek src' = do
        rec let b = extent arr
            m <- newArray_ b :: IO ((Mutable b) (Bound d) e)

            let src = castPtr src'
            forM_ [0 .. rangeSize b - 1] $ \i -> do
                x <- peek $ advancePtr src i
                unsafeWrite m i x

            arr <- StaticArray <$> unsafeFreeze m
        return arr

    poke dst' arr = do
        let a = toArray arr
            b = bounds a
            dst = castPtr dst'
        forM_ [0 .. rangeSize b - 1] $ \i ->
            poke (advancePtr dst i) $ unsafeAt a i


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

----------------------------------------------------------------------------
-- StaticSize instances. More can be written, trivially - it's just a matter
-- of whether they'll ever actually be used.

instance SingI a => StaticSize (a :: Nat) where
    type Bound a = Int
    extent _ = (0, fromNat (Proxy :: Proxy a) - 1)

instance (SingI a, SingI b) => StaticSize ('(a, b) :: (Nat, Nat)) where
    type Bound '(a, b) = (Int, Int)
    extent _ = ((0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1))

instance (SingI a, SingI b, SingI c) =>
         StaticSize ('(a, b, c) :: (Nat, Nat, Nat)) where
    type Bound '(a, b, c) = (Int, Int, Int)
    extent _ = ((0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1))

instance (SingI a, SingI b, SingI c, SingI d) =>
         StaticSize ('(a, b, c, d) :: (Nat, Nat, Nat, Nat)) where
    type Bound '(a, b, c, d) = (Int, Int, Int, Int)
    extent _ = ((0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1))

instance (SingI a, SingI b, SingI c, SingI d, SingI e) =>
         StaticSize ('(a, b, c, d, e) :: (Nat, Nat, Nat, Nat, Nat)) where
    type Bound '(a, b, c, d, e) = (Int, Int, Int, Int, Int)
    extent _ = ((0, 0, 0, 0, 0),
                (fromNat (Proxy :: Proxy a) - 1,
                 fromNat (Proxy :: Proxy b) - 1,
                 fromNat (Proxy :: Proxy c) - 1,
                 fromNat (Proxy :: Proxy d) - 1,
                 fromNat (Proxy :: Proxy e) - 1))

instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f) =>
         StaticSize ('(a, b, c, d, e, f) ::
                     (Nat, Nat, Nat, Nat, Nat, Nat)) where
    type Bound '(a, b, c, d, e, f) = (Int, Int, Int, Int, Int, Int)
    extent _ = ((0, 0, 0, 0, 0, 0),
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
    extent _ = ((0, 0, 0, 0, 0, 0, 0),
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
    extent _ = ((0, 0, 0, 0, 0, 0, 0, 0),
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

instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h, SingI i, SingI j) =>
         StaticSize ('(a, b, c, d, e, f, g, h, i, j) ::
                     (Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat)) where
    type Bound '(a, b, c, d, e, f, g, h, i, j) =
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

instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h, SingI i, SingI j, SingI k) =>
         StaticSize ('(a, b, c, d, e, f, g, h, i, j, k) ::
                     (Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat))
      where
    type Bound '(a, b, c, d, e, f, g, h, i, j, k) =
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

instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h, SingI i, SingI j, SingI k, SingI l) =>
         StaticSize ('(a, b, c, d, e, f, g, h, i, j, k, l) ::
                     (Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat,
                      Nat)) where
    type Bound '(a, b, c, d, e, f, g, h, i, j, k, l) =
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

instance (SingI a, SingI b, SingI c, SingI d, SingI e, SingI f, SingI g,
          SingI h, SingI i, SingI j, SingI k, SingI l, SingI m) =>
         StaticSize ('(a, b, c, d, e, f, g, h, i, j, k, l, m) ::
                     (Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat, Nat,
                      Nat, Nat)) where
    type Bound '(a, b, c, d, e, f, g, h, i, j, k, l, m) =
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

instance SingI n => StaticSize ('[n] :: [Nat]) where
    type Bound ('[n]) = Int
    extent _ = (0, fromNat (Proxy :: Proxy n) - 1)

instance (SingI n, StaticSize (n2 ': ns)) =>
          StaticSize ((n ': n2 ': ns) :: [Nat]) where
    type Bound (n ': n2 ': ns) = (Int, Bound (n2 ': ns))
    extent _ = ((0, b0), (fromNat (Proxy :: Proxy n) - 1, bn))
      where
        (b0, bn) = extent (undefined :: StaticArray a (n2 ': ns) ())
