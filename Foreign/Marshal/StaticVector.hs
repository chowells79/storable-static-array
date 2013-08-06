{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-|

This module defines 'StaticVector', a simple wrapper around
'VG.Vector' with the dimensions in the type. 'StaticVector' provides a
'Storable' instance using the type-level dimensions. This eases
writing FFI bindings to fixed-size native arrays.

Support for interop with multi-dimensional native arrays is provided
via the 'IxStatic' class. This results in the slightly unnatural case
where you might need to convert 'Ix' coordinates to `VG.Vector`
indices, but it felt like an acceptable tradeoff when interfacing with
multi-dimensional native arrays.

-}
module Foreign.Marshal.StaticVector
       ( StaticVector
       , toVector
       , staticBounds
       , staticSize
       , fromList
       ) where

import Control.Monad
import Data.Functor ((<$>))

import Data.Ix
import Data.Ix.Static

import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM

import Data.Tagged

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array


-- | A minimal 'VG.Vector' wrapper that encodes the full dimensions of
-- the array in the type. Intended for interfacing with
-- (possibly-)multidimensional arrays of fixed size in native code.
--
-- If this is used with multidimensional arrays, it will be up to
-- users to deal with converting 'Ix' coordinates to internal
-- 'VG.Vector' indices.
--
-- The constructor is not exported to prevent creating a
-- 'StaticVector' with a size that doesn't match its dimensions.
newtype StaticVector backing dimensions (elements :: *) =
    StaticVector {
        -- | Returns the backing value of this 'StaticVector'.
        toVector :: backing elements
        }
    deriving (Eq)

instance (VG.Vector b e, Show e) => Show (StaticVector b d e) where
    show = ("fromList " ++) . show . VG.toList . toVector

-- | Get the compile-time bounds from a 'StaticVector'. Does not
-- examine its argument.
{-# INLINEABLE staticBounds #-}
staticBounds :: forall b d e. IxStatic d =>
                StaticVector b d e -> (Index d, Index d)
staticBounds _ = untag (taggedBounds :: Tagged d (Index d, Index d))

-- | Get the compile-time size from a 'StaticVector'. Does not examine
-- its argument.
{-# INLINEABLE staticSize #-}
staticSize :: IxStatic d => StaticVector b d e -> Int
staticSize = rangeSize . staticBounds

-- | Create a new 'StaticVector' with the contents of the list. If the
-- list passed in contains too many elements, the result will be
-- truncated. If it contains too few elements, they will be cycled to
-- pad out the remaining space. If it contains 0 elements, this will
-- result in an error.
{-# INLINEABLE fromList #-}
fromList :: (VG.Vector b e, IxStatic d) => [e] -> StaticVector b d e
fromList els | null els = error "empty input to fromList"
             | otherwise = sv
  where
    size = staticSize sv
    sv = StaticVector . VG.fromListN size . cycle $ els

instance (IxStatic d, Storable e, VG.Vector b e) =>
         Storable (StaticVector b d e) where
    {-# INLINEABLE sizeOf #-}
    sizeOf a = sizeOf (undefined :: e) * rangeSize (staticBounds a)
    {-# INLINEABLE alignment #-}
    alignment _ = alignment (undefined :: e)
    {-# INLINEABLE poke #-}
    poke dst' sv@(StaticVector v) = do
        let upper = staticSize sv - 1
            dst = castPtr dst'
        forM_ [0..upper] $ \i -> poke (advancePtr dst i) $ VG.unsafeIndex v i
    {-# INLINEABLE peek #-}
    peek src' = do
        rec let size = staticSize sv
            v <- VGM.unsafeNew size

            let src = castPtr src'
            forM_ [0 .. size - 1] $ \i -> do
                x <- peek $ advancePtr src i
                VGM.unsafeWrite v i x

            sv <- StaticVector <$> VG.unsafeFreeze v
        return sv
