{-# LANGUAGE CPP, MagicHash #-}

-- |
-- Module      : Data.Text.Internal.Unsafe.Char
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Fast character manipulation functions.
module Data.Text.Internal.Unsafe.Char
    (
      ord
    , unsafeChr
    , unsafeChr8
    , unsafeChr32
    , unsafeWrite
    -- , unsafeWriteRev
    ) where

#ifdef ASSERTS
import Control.Exception (assert)
#endif
import Control.Monad.ST (ST)
import Data.Bits ((.&.))
import Data.Text.Internal.Unsafe.Shift (shiftR)
import GHC.Exts (Char(..), Int(..), chr#, ord#, word2Int#)
import GHC.Word (Word8(..), Word16(..), Word32(..))
import qualified Data.Text.Array as A

ord :: Char -> Int
ord (C# c#) = I# (ord# c#)
{-# INLINE ord #-}

unsafeChr :: Word16 -> Char
unsafeChr (W16# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChr #-}

unsafeChr8 :: Word8 -> Char
unsafeChr8 (W8# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChr8 #-}

unsafeChr32 :: Word32 -> Char
unsafeChr32 (W32# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChr32 #-}

-- | Write a character into the array at the given offset.  Returns
-- the number of bytes written.
unsafeWrite :: A.MArray s -> Int -> Char -> ST s Int
unsafeWrite marr i c
    -- One-byte character
    | n < 0x80 = do
#if defined(ASSERTS)
        assert (i >= 0) . assert (i < A.length marr) $ return ()
#endif
        writeAt i n
        return 1

    -- Two-byte character
    | n < 0x0800 = do
#if defined(ASSERTS)
        assert (i >= 0) . assert (i + 1 < A.length marr) $ return ()
#endif
        writeAt i       $ (n `shiftR` 6) + 0xC0
        writeAt (i + 1) $ (n .&. 0x3F)   + 0x80
        return 2

    -- Three-byte character
    | n < 0x10000 = do
#if defined(ASSERTS)
        assert (i >= 0) . assert (i + 2 < A.length marr) $ return ()
#endif
        writeAt i       $ (n `shiftR` 12)           + 0xE0
        writeAt (i + 1) $ ((n `shiftR` 6) .&. 0x3F) + 0x80
        writeAt (i + 2) $ (n .&. 0x3F)              + 0x80
        return 3

    -- Four-byte character
    | otherwise = do
#if defined(ASSERTS)
        assert (i >= 0) . assert (i + 3 < A.length marr) $ return ()
#endif
        writeAt i       $ (n `shiftR` 18)            + 0xF0
        writeAt (i + 1) $ ((n `shiftR` 12) .&. 0x3F) + 0x80
        writeAt (i + 2) $ ((n `shiftR` 6)  .&. 0x3F) + 0x80
        writeAt (i + 3) $ (n .&. 0x3F)               + 0x80
        return 4
  where 
    n = ord c
    writeAt i' n' = A.unsafeWrite marr i' (fromIntegral n')
    {-# INLINE writeAt #-}
{-# INLINE unsafeWrite #-}

{-
unsafeWriteRev :: A.MArray s Word16 -> Int -> Char -> ST s Int
unsafeWriteRev marr i c
    | n < 0x10000 = do
        assert (i >= 0) . assert (i < A.length marr) $
          A.unsafeWrite marr i (fromIntegral n)
        return (i-1)
    | otherwise = do
        assert (i >= 1) . assert (i < A.length marr) $
          A.unsafeWrite marr (i-1) lo
        A.unsafeWrite marr i hi
        return (i-2)
    where n = ord c
          m = n - 0x10000
          lo = fromIntegral $ (m `shiftR` 10) + 0xD800
          hi = fromIntegral $ (m .&. 0x3FF) + 0xDC00
{-# INLINE unsafeWriteRev #-}
-}
