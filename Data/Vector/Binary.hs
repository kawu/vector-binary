{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Vector.Binary () where

import Data.Binary
import System.IO.Unsafe
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- Both binary instances below originate from the vector-binary-instances
-- library.  To avoid the overlapping instances problem the instances
-- are restricted to monomorphic vector types.

instance (U.Unbox e, Binary e) => Binary (U.Vector e) where
    put v = do
        put (G.length v)
        mapM_ put (G.toList v)

    -- this is morally sound, if very awkward.
    -- all effects are contained, and can't escape the unsafeFreeze
    {-# INLINE get #-}
    get = do
        n <- get

        -- new unitinialized array
        mv <- lift $ M.new n

        let fill i
                | i < n = do
                    x <- get
                    (unsafePerformIO $ M.unsafeWrite mv i x) `seq` return ()
                    fill (i+1)

                | otherwise = return ()

        fill 0

        lift $ G.unsafeFreeze mv

instance Binary e => Binary (V.Vector e) where
    put v = do
        put (G.length v)
        mapM_ put (G.toList v)

    -- this is morally sound, if very awkward.
    -- all effects are contained, and can't escape the unsafeFreeze
    {-# INLINE get #-}
    get = do
        n <- get

        -- new unitinialized array
        mv <- lift $ M.new n

        let fill i
                | i < n = do
                    x <- get
                    (unsafePerformIO $ M.unsafeWrite mv i x) `seq` return ()
                    fill (i+1)

                | otherwise = return ()

        fill 0

        lift $ G.unsafeFreeze mv

lift :: IO b -> Get b
lift = return .unsafePerformIO
