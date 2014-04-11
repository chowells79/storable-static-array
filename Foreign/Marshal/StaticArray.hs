{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-|

This module defines 'StaticArray', a simple wrapper around arrays with
their dimensions in the type. 'StaticArray' provides 'Storable'
instances using the type-level dimensions. This eases writing FFI
bindings to fixed-size native arrays. For example, @'StaticArray'
'UArray' 10 CInt@ has a 'Storable' instance that is directly
compatible with @int foo[10]@ in native code.

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
       , pattern StaticArray
       , staticBounds
       , staticArray
       , listStaticArray
       ) where

import Control.Monad
import Data.Functor ((<$>))

import Data.Array.Base as A
import Data.Array.IO

import Data.Ix.Static as IS

import Data.Proxy

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array


-- | A minimal array wrapper that encodes the full dimensions of the
-- array in the type. Intended for interfacing with
-- (possibly-)multidimensional arrays of fixed size in native code.
newtype StaticArray backing dimensions (elements :: *) =
    SA {
        -- | Returns the backing value of this 'StaticArray'.
        toArray :: backing (Index dimensions) elements
        }
    deriving Eq

instance (IArray b e, IxStatic d, Show e) => Show (StaticArray b d e) where
    show = ("listStaticArray " ++) . show . elems . toArray

-- | A convenience pattern for extracting the backing value of a
-- 'StaticArray' without exposing the real constructor
pattern StaticArray x <- SA x

-- | Get the compile-time bounds from a 'StaticArray'. Does not examine its
-- argument.
{-# INLINEABLE staticBounds #-}
staticBounds :: forall b d e. IxStatic d =>
                StaticArray b d e -> (Index d, Index d)
staticBounds _ = IS.bounds (Proxy :: Proxy d)

-- | Create a new 'StaticArray' from a list of indices and
-- elements. This has all the semantic caveats of 'array', except that
-- the bounds are as good as those provided by the 'IxStatic'
-- instance.
{-# INLINEABLE staticArray #-}
staticArray :: (IArray b e, IxStatic d) => [(Index d, e)] -> StaticArray b d e
staticArray ls = let a = SA $ array (staticBounds a) ls in a

-- | Create a new 'StaticArray' from a list of elements in index
-- order. Implemented in terms of 'listArray', with the same caveats.
{-# INLINEABLE listStaticArray #-}
listStaticArray :: (IxStatic d, IArray b e) => [e] -> StaticArray b d e
listStaticArray ls = let a = SA $ listArray (staticBounds a) ls in a

instance (IxStatic d, Storable e, IArray b e) =>
         Storable (StaticArray b d e) where
    {-# INLINEABLE sizeOf #-}
    sizeOf a = sizeOf (undefined :: e) * rangeSize (staticBounds a)
    {-# INLINEABLE alignment #-}
    alignment _ = alignment (undefined :: e)
    {-# INLINEABLE poke #-}
    poke dst' (StaticArray a) = do
        let upper = rangeSize (A.bounds a) - 1
            dst = castPtr dst'
        forM_ [0..upper] $ \i -> poke (advancePtr dst i) $ unsafeAt a i
    {-# INLINEABLE peek #-}
    peek src' = do
        rec let b = staticBounds arr
            m <- newArray_ b :: IO (IOArray (Index d) e)

            let src = castPtr src'
            forM_ [0 .. rangeSize b - 1] $ \i -> do
                x <- peek $ advancePtr src i
                unsafeWrite m i x

            arr <- SA <$> unsafeFreeze m
        return arr

instance (IxStatic d, Storable e, IArray UArray e, MArray IOUArray e IO) =>
         Storable (StaticArray UArray d e) where
    {-# INLINEABLE sizeOf #-}
    sizeOf a = sizeOf (undefined :: e) * rangeSize (staticBounds a)
    {-# INLINEABLE alignment #-}
    alignment _ = alignment (undefined :: e)
    {-# INLINEABLE poke #-}
    poke dst' (StaticArray a) = do
        let upper = rangeSize (A.bounds a) - 1
            dst = castPtr dst'
        forM_ [0..upper] $ \i -> poke (advancePtr dst i) $ unsafeAt a i
    {-# INLINEABLE peek #-}
    peek src' = do
        rec let b = staticBounds arr
            m <- newArray_ b :: IO (IOUArray (Index d) e)

            let src = castPtr src'
            forM_ [0 .. rangeSize b - 1] $ \i -> do
                x <- peek $ advancePtr src i
                unsafeWrite m i x

            arr <- SA <$> unsafeFreeze m
        return arr
