{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-|

X

-}
module Foreign.Marshal.StaticVector where

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


newtype StaticVector backing dimensions (elements :: *) =
    StaticVector {
        -- | Returns the backing of this 'StaticVector'.
        toVector :: backing elements
        }
    deriving (Eq)

instance (VG.Vector b e, Show e) => Show (StaticVector b d e) where
    show = ("fromList " ++) . show . VG.toList . toVector

-- | Get the compile-time bounds from a 'StaticVector'. Does not examine its
-- argument.
{-# INLINEABLE staticBounds #-}
staticBounds :: forall b d e. IxStatic d =>
                StaticVector b d e -> (Bound d, Bound d)
staticBounds _ = untag (taggedBounds :: Tagged d (Bound d, Bound d))

{-# INLINEABLE staticSize #-}
staticSize :: IxStatic d => StaticVector b d e -> Int
staticSize = rangeSize . staticBounds

{-# INLINEABLE fromList #-}
fromList :: (VG.Vector b e, IxStatic d) => [e] -> StaticVector b d e
fromList els | len == size = sv
             | otherwise =
                 error $ "wrong size list provided to fromList; expected " ++
                 show size ++ ", got " ++ show len
  where
    len = length els
    size = staticSize sv
    sv = StaticVector $ VG.fromListN size els

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
