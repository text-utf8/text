{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface, MagicHash, Rank2Types,
    RecordWildCards, UnboxedTuples, UnliftedFFITypes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- |
-- Module      : Data.Text.Array
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Packed, unboxed, heap-resident arrays.  Suitable for performance
-- critical use, both in terms of large data quantities and high
-- speed.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import qualified Data.Text.Array as A
--
-- The names in this module resemble those in the 'Data.Array' family
-- of modules, but are shorter due to the assumption of qualified
-- naming.
module Data.Text.Array
    (
    -- * Types
      Array(Array, aBA)
    , MArray(MArray, maBA)

    -- * Functions
    , copyM
    , copyI
    , copyToPtr
    , copyFromPtr

    , empty
    , equal
    , cmp
#if defined(ASSERTS)
    , length
#endif
    , run
    , run2
    , toList
    , unsafeFreeze
    , unsafeIndex
    , unsafeIndex32
    , unsafeIndex64
    , new
    , unsafeWrite
    , unsafeWrite32
    , unsafeWrite64
    ) where

#if defined(ASSERTS)
-- This fugly hack is brought by GHC's apparent reluctance to deal
-- with MagicHash and UnboxedTuples when inferring types. Eek!
# define CHECK_BOUNDS(_func_,_len_,_k_) \
if (_k_) < 0 || (_k_) >= (_len_) then error ("Data.Text.Array." ++ (_func_) ++ ": bounds error, offset " ++ show (_k_) ++ ", length " ++ show (_len_)) else
#else
# define CHECK_BOUNDS(_func_,_len_,_k_)
#endif

#include "MachDeps.h"

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
#if __GLASGOW_HASKELL__ >= 702
import Control.Monad.ST.Unsafe (unsafeIOToST)
#else
import Control.Monad.ST (unsafeIOToST)
#endif
import Data.Bits ((.&.), xor)
import Data.Text.Internal.Unsafe (inlinePerformIO)
import Data.Text.Internal.Unsafe.Shift (shiftL, shiftR)
import Foreign.Ptr (Ptr)
#if __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types (CInt(CInt), CSize(CSize))
#else
import Foreign.C.Types (CInt, CSize)
#endif
import GHC.Base (IO(..), RealWorld, ByteArray#, MutableByteArray#, Int(..), (-#),
                 indexWord8Array#, indexWord32Array#, indexWord64Array#, newByteArray#, plusAddr#,
                 unsafeFreezeByteArray#, writeWord8Array#, writeWord32Array#, writeWord64Array#,
                 copyByteArray#, copyMutableByteArray#, copyByteArrayToAddr#,
                 copyAddrToByteArray#)
import GHC.Exts (Ptr(..))
import GHC.ST (ST(..), runST)
import GHC.Word (Word8(..), Word32(..), Word64(..))
import Prelude hiding (length, read)

-- | Immutable array type.
data Array = Array {
      aBA :: ByteArray#
#if defined(ASSERTS)
    , aLen :: {-# UNPACK #-} !Int -- length in bytes
#endif
    }

-- | Mutable array type, for use in the ST monad.
data MArray s = MArray {
      maBA :: MutableByteArray# s
#if defined(ASSERTS)
    , maLen :: {-# UNPACK #-} !Int -- length in bytes
#endif
    }

#if defined(ASSERTS)
-- | Operations supported by all arrays.
class IArray a where
    -- | Return the length of an array.
    length :: a -> Int

instance IArray Array where
    length = aLen
    {-# INLINE length #-}

instance IArray (MArray s) where
    length = maLen
    {-# INLINE length #-}
#endif

-- | Create an uninitialized mutable array.
new :: forall s. Int -> ST s (MArray s)
new n
  | n < 0 || n .&. highBit /= 0 = array_size_error
  | otherwise = ST $ \s1# ->
       case newByteArray# len# s1# of
         (# s2#, marr# #) -> (# s2#, MArray marr#
#if defined(ASSERTS)
                                n
#endif
                                #)
  where !(I# len#) = bytesInArray n
        highBit    = maxBound `xor` (maxBound `shiftR` 1)
{-# INLINE new #-}

array_size_error :: a
array_size_error = error "Data.Text.Array.new: size overflow"

-- | Freeze a mutable array. Do not mutate the 'MArray' afterwards!
unsafeFreeze :: MArray s -> ST s Array
unsafeFreeze MArray{..} = ST $ \s1# ->
    case unsafeFreezeByteArray# maBA s1# of
        (# s2#, ba# #) -> (# s2#, Array ba#
#if defined(ASSERTS)
                             maLen
#endif
                             #)
{-# INLINE unsafeFreeze #-}

-- | Indicate how many bytes would be used for an array of the given
-- size.
bytesInArray :: Int -> Int
bytesInArray n = n
{-# INLINE bytesInArray #-}

-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndex :: Array -> Int -> Word8
unsafeIndex Array{..} i@(I# i#) =
  CHECK_BOUNDS("unsafeIndex",aLen,i)
    case indexWord8Array# aBA i# of r# -> (W8# r#)
{-# INLINE unsafeIndex #-}

-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndex32 :: Array -> Int -> Word32
unsafeIndex32 Array{..} i@(I# i#) =
  CHECK_BOUNDS("unsafeIndex32",aLen `quot` 4,i)
    case indexWord32Array# aBA i# of r# -> (W32# r#)
{-# INLINE unsafeIndex32 #-}

-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndex64 :: Array -> Int -> Word64
unsafeIndex64 Array{..} i@(I# i#) =
  CHECK_BOUNDS("unsafeIndex64",aLen `quot` 8,i)
    case indexWord64Array# aBA i# of r# -> (W64# r#)
{-# INLINE unsafeIndex64 #-}

-- | Unchecked write of a mutable array.  May return garbage or crash
-- on an out-of-bounds access.
unsafeWrite :: MArray s -> Int -> Word8 -> ST s ()
unsafeWrite MArray{..} i@(I# i#) (W8# e#) = ST $ \s1# ->
  CHECK_BOUNDS("unsafeWrite",maLen,i)
  case writeWord8Array# maBA i# e# s1# of
    s2# -> (# s2#, () #)
{-# INLINE unsafeWrite #-}

-- | Unchecked write of a mutable array.  May return garbage or crash
-- on an out-of-bounds access.
unsafeWrite32 :: MArray s -> Int -> Word32 -> ST s ()
unsafeWrite32 MArray{..} i@(I# i#) (W32# e#) = ST $ \s1# ->
  CHECK_BOUNDS("unsafeWrite32",maLen `quot` 4,i)
  case writeWord32Array# maBA i# e# s1# of
    s2# -> (# s2#, () #)
{-# INLINE unsafeWrite32 #-}

-- | Unchecked write of a mutable array.  May return garbage or crash
-- on an out-of-bounds access.
unsafeWrite64 :: MArray s -> Int -> Word64 -> ST s ()
unsafeWrite64 MArray{..} i@(I# i#) (W64# e#) = ST $ \s1# ->
  CHECK_BOUNDS("unsafeWrite64",maLen `quot` 8,i)
  case writeWord64Array# maBA i# e# s1# of
    s2# -> (# s2#, () #)
{-# INLINE unsafeWrite64 #-}

-- | Convert an immutable array to a list.
toList :: Array -> Int -> Int -> [Word8]
toList ary off len = loop 0
    where loop i | i < len   = unsafeIndex ary (off+i) : loop (i+1)
                 | otherwise = []

-- | An empty immutable array.
empty :: Array
empty = runST (new 0 >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result.
run :: (forall s. ST s (MArray s)) -> Array
run k = runST (k >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result paired with whatever else the action returns.
run2 :: (forall s. ST s (MArray s, a)) -> (Array, a)
run2 k = runST (do
                 (marr,b) <- k
                 arr <- unsafeFreeze marr
                 return (arr,b))
{-# INLINE run2 #-}

-- | Copy some elements of a mutable array.
copyM :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> MArray s               -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ Count
      -> ST s ()
copyM dest didx@(I# didx#) src sidx@(I# sidx#) count@(I# count#)
    | count <= 0 = return ()
    | otherwise =
#if defined(ASSERTS)
    assert (sidx + count <= length src) .
    assert (didx + count <= length dest) .
#endif
    ST $ \s ->
           case copyMutableByteArray# (maBA src) sidx# (maBA dest) didx# count# s of
             s' -> (# s', () #)
{-# INLINE copyM #-}

-- | Copy some elements of an immutable array.
copyI :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> Array                  -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ First offset in destination /not/ to
                                -- copy (i.e. /not/ length)
      -> ST s ()
copyI dest i0@(I# i0#) src _j0@(I# j0#) top@(I# top#)
    | i0 >= top = return ()
    | otherwise = ST $ \s ->
                         case copyByteArray# (aBA src) j0# (maBA dest) i0# (top# -# i0#) s of
                           s' -> (# s', () #)
{-# INLINE copyI #-}

-- | Compare portions of two arrays for equality.  No bounds checking
-- is performed.
equal :: Array                  -- ^ First
      -> Int                    -- ^ Offset into first
      -> Array                  -- ^ Second
      -> Int                    -- ^ Offset into second
      -> Int                    -- ^ Count
      -> Bool
equal arrA offA arrB offB count = cmp arrA offA arrB offB count == EQ
{-# INLINE equal #-}

-- | Compare portions of two arrays for equality.  No bounds checking
-- is performed.
cmp :: Array                  -- ^ First
    -> Int                    -- ^ Offset into first
    -> Array                  -- ^ Second
    -> Int                    -- ^ Offset into second
    -> Int                    -- ^ Count
    -> Ordering
cmp arrA offA arrB offB count = inlinePerformIO $ do
  i <- memcmp (aBA arrA) (fromIntegral offA)
                     (aBA arrB) (fromIntegral offB) (fromIntegral count)
  return $ compare i 0
{-# INLINE cmp #-}

foreign import ccall unsafe "_hs_text_memcmp" memcmp
    :: ByteArray# -> CSize -> ByteArray# -> CSize -> CSize -> IO CInt

-- | Copy some elements of an immutable array to a pointer
copyToPtr :: Ptr Word8               -- ^ Destination
          -> Int                     -- ^ Destination offset
          -> Array                   -- ^ Source
          -> Int                     -- ^ Source offset
          -> Int                     -- ^ First offset in destination /not/ to
                                     -- copy (i.e. /not/ length)
          -> IO ()
copyToPtr dest@(Ptr dest#) i0@(I# i0#) src j0@(I# j0#) top@(I# top#)
    | i0 >= top = return ()
    | otherwise =
        IO $ \s -> case copyByteArrayToAddr# (aBA src) j0# (plusAddr# dest# i0#) (top# -# i0#) s of
                     s' -> (# s', () #)
{-# INLINE copyToPtr #-}

copyFromPtr :: MArray s          -- ^ Destination
            -> Int               -- ^ Destination offset
            -> Ptr Word8         -- ^ Source
            -> Int               -- ^ Source offset
            -> Int               -- ^ Count
            -> ST s ()
copyFromPtr dest i0@(I# i0#) src@(Ptr src#) j0@(I# j0#) count@(I# count#)
  | count <= 0 = return ()
  | otherwise =
    ST $ \s -> case copyAddrToByteArray# (plusAddr# src# i0#) (maBA dest) j0# count# s of
                 s' -> (# s', () #)
{-# INLINE copyFromPtr #-}
