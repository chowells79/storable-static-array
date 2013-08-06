{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
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
       , staticBounds
       , staticArray
       , listStaticArray
         -- * Adding new Storable instances
         -- $NewStorable
       , sizeOf'
       , alignment'
       , poke'
       , peek'
       ) where

import Control.Monad
import Data.Functor ((<$>))

import Data.Array (Array)
import Data.Array.Base
import Data.Array.IO hiding (unsafeFreeze)

import Data.Ix.Static

import Data.Tagged

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array


-- | A minimal array wrapper that encodes the full dimensions of the
-- array in the type. Intended for interfacing with
-- (possibly-)multidimensional arrays of fixed size in native code.
--
-- The constructor is not exported to prevent creating a 'StaticArray'
-- with a size that doesn't match its dimensions.
newtype StaticArray backing dimensions (elements :: *) =
    StaticArray {
        -- | Returns the backing value of this 'StaticArray'.
        toArray :: backing (Index dimensions) elements
        }
    deriving Eq

instance (IArray b e, IxStatic d, Show e) => Show (StaticArray b d e) where
    show = ("listStaticArray " ++) . show . elems . toArray

-- | Get the compile-time bounds from a 'StaticArray'. Does not examine its
-- argument.
{-# INLINEABLE staticBounds #-}
staticBounds :: forall b d e. IxStatic d =>
                StaticArray b d e -> (Index d, Index d)
staticBounds _ = untag (taggedBounds :: Tagged d (Index d, Index d))

-- | Create a new 'StaticArray' from a list of indices and
-- elements. This has all the semantic caveats of 'array', except that
-- the bounds are as good as those provided by the 'IxStatic'
-- instance.
{-# INLINEABLE staticArray #-}
staticArray :: (IArray b e, IxStatic d) => [(Index d, e)] -> StaticArray b d e
staticArray ls = let a = StaticArray $ array (staticBounds a) ls in a

-- | Create a new 'StaticArray' from a list of elements in index
-- order. Implemented in terms of 'listArray', with the same caveats.
{-# INLINEABLE listStaticArray #-}
listStaticArray :: (IxStatic d, IArray b e) => [e] -> StaticArray b d e
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
-- 'Storable' implementation for 'StaticArray'. They can be used to
-- add more instances of 'Storable', if required. The helper function
-- required by 'peek'' is the part necessary for efficient
-- implementations which prevent creation of a fully polymorphic
-- instance.

-- | Get the size, in bytes, of the native representation of this
-- 'StaticArray'.
{-# INLINEABLE sizeOf' #-}
sizeOf' :: forall b d e. (IxStatic d, Storable e) =>
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
poke' :: forall b d e. (IxStatic d, IArray b e, Storable e) =>
         Ptr (StaticArray b d e) -> StaticArray b d e -> IO ()
poke' dst' (StaticArray a) = do
    let upper = rangeSize (bounds a) - 1
        dst = castPtr dst'
    forM_ [0..upper] $ \i -> poke (advancePtr dst i) $ unsafeAt a i

-- | Create a new 'StaticArray' from the contents of the given
-- location in memory. Uses a temporary mutable array to build up the
-- result, then freezes it. The first argument is the freezing
-- function. Non-copying implementations of 'unsafeFreeze' are safe as
-- this argument, and preferred.
{-# INLINEABLE peek' #-}
peek' :: forall b d e m. (IxStatic d, Storable e, IArray b e,
                          MArray m e IO) =>
         (m (Index d) e -> IO (b (Index d) e)) ->
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

instance (IxStatic d, Storable e, IArray UArray e, MArray IOUArray e IO) =>
         Storable (StaticArray UArray d e) where
    {-# INLINEABLE sizeOf#-}
    sizeOf = sizeOf'
    {-# INLINEABLE alignment #-}
    alignment = alignment'
    {-# INLINEABLE poke #-}
    poke = poke'
    {-# INLINEABLE peek #-}
    peek = peek' (unsafeFreeze :: IOUArray (Index d) e ->
                                  IO (UArray (Index d) e))

instance (IxStatic d, Storable e) => Storable (StaticArray Array d e) where
    {-# INLINEABLE sizeOf #-}
    sizeOf = sizeOf'
    {-# INLINEABLE alignment #-}
    alignment = alignment'
    {-# INLINEABLE poke #-}
    poke = poke'
    {-# INLINEABLE peek #-}
    peek = peek' (unsafeFreeze :: IOArray (Index d) e ->
                                  IO (Array (Index d) e))
