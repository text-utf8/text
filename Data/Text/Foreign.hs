{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Data.Text.Foreign
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- Support for using 'Text' data with native code via the Haskell
-- foreign function interface.

module Data.Text.Foreign
    (
    -- * Interoperability with native code
    -- $interop
      I8
    -- * Safe conversion functions
    , fromPtr
    , useAsPtr
    , asForeignPtr
    -- ** Encoding as UTF-8
    , peekCStringLen
    , withCStringLen
    -- * Unsafe conversion code
    , lengthWord8
    , unsafeCopyToPtr
    -- * Low-level manipulation
    -- $lowlevel
    , dropWord8
    , takeWord8
    ) where

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import qualified Data.Text.Internal.Encoding.Utf8 as U8
import Data.ByteString.Unsafe (unsafePackCStringLen, unsafeUseAsCStringLen)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Internal (Text(..), empty)
import Data.Text.Unsafe (lengthWord8)
import qualified Data.Text.Unsafe as Unsafe
import Data.Word (Word8)
import Foreign.C.String (CStringLen)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import qualified Data.Text.Array as A

-- $interop
--
-- The 'Text' type is implemented using arrays that are not guaranteed
-- to have a fixed address in the Haskell heap. All communication with
-- native code must thus occur by copying data back and forth.
--
-- The 'Text' type's internal representation is UTF-8.
-- To interoperate with native libraries that use different
-- internal representations, such as UTF-16 or UTF-32, consider using
-- the functions in the 'Data.Text.Encoding' module.

-- | A type representing a number of UTF-16 code units.
newtype I8 = I8 Int
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

-- | /O(n)/ Create a new 'Text' from a 'Ptr' 'Word8' by copying the
-- contents of the array.
fromPtr :: Ptr Word8            -- ^ source array
        -> I8                   -- ^ length of source array (in 'Word8' units)
        -> IO Text
fromPtr _   (I8 0)   = return empty
fromPtr ptr (I8 len) =
#if defined(ASSERTS)
    assert (len > 0) $
#endif
    return $! Text arr 0 len
  where
    arr = A.run (A.new len >>= copy)
    copy marr = A.copyFromPtr marr 0 ptr 0 len >> return marr

-- $lowlevel
--
-- Foreign functions that use UTF-16 internally may return indices in
-- units of 'Word8' instead of characters.  These functions may
-- safely be used with such indices, as they will adjust offsets if
-- necessary to preserve the validity of a Unicode string.

-- | /O(1)/ Return the prefix of the 'Text' of @n@ 'Word8' units in
-- length.
--
-- If @n@ would cause the 'Text' to end inside a surrogate pair, the
-- end of the prefix will be advanced by one additional 'Word8' unit
-- to maintain its validity.
takeWord8 :: I8 -> Text -> Text
takeWord8 (I8 n) t@(Text arr off len)
    | n <= 0                = empty
    | n >= len              = t
    | U8.continuationByte x = takeWord8 (I8 (n + 1)) t
    | otherwise             = Unsafe.takeWord8 n t
  where
    x = A.unsafeIndex arr (off + n)

-- | /O(1)/ Return the suffix of the 'Text', with @n@ 'Word8' units
-- dropped from its beginning.
--
-- If @n@ would cause the 'Text' to begin inside a surrogate pair, the
-- beginning of the suffix will be advanced by one additional 'Word8'
-- unit to maintain its validity.
dropWord8 :: I8 -> Text -> Text
dropWord8 (I8 n) t@(Text arr off len)
    | n <= 0                = t
    | n >= len              = empty
    | U8.continuationByte x = dropWord8 (I8 (n + 1)) t
    | otherwise             = Unsafe.dropWord8 n t
  where
    x = A.unsafeIndex arr (off + n)

-- | /O(n)/ Copy a 'Text' to an array.  The array is assumed to be big
-- enough to hold the contents of the entire 'Text'.
unsafeCopyToPtr :: Text -> Ptr Word8 -> IO ()
unsafeCopyToPtr (Text arr off len) ptr =
    A.copyToPtr ptr 0 arr off len

-- | /O(n)/ Perform an action on a temporary, mutable copy of a
-- 'Text'.  The copy is freed as soon as the action returns.
useAsPtr :: Text -> (Ptr Word8 -> I8 -> IO a) -> IO a
useAsPtr t@(Text _arr _off len) action =
  allocaBytes len $ \buf -> do
    unsafeCopyToPtr t buf
    action (castPtr buf) (fromIntegral len)

-- | /O(n)/ Make a mutable copy of a 'Text'.
asForeignPtr :: Text -> IO (ForeignPtr Word8, I8)
asForeignPtr t@(Text _arr _off len) = do
  fp <- mallocForeignPtrArray len
  withForeignPtr fp $ unsafeCopyToPtr t
  return (fp, I8 len)

-- | /O(n)/ Decode a C string with explicit length, which is assumed
-- to have been encoded as UTF-8. If decoding fails, a
-- 'UnicodeException' is thrown.
--
-- @since 1.0.0.0
peekCStringLen :: CStringLen -> IO Text
peekCStringLen cs = do
  bs <- unsafePackCStringLen cs
  return $! decodeUtf8 bs

-- | Marshal a 'Text' into a C string encoded as UTF-8 in temporary
-- storage, with explicit length information. The encoded string may
-- contain NUL bytes, and is not followed by a trailing NUL byte.
--
-- The temporary storage is freed when the subcomputation terminates
-- (either normally or via an exception), so the pointer to the
-- temporary storage must /not/ be used after this function returns.
--
-- @since 1.0.0.0
withCStringLen :: Text -> (CStringLen -> IO a) -> IO a
withCStringLen t act = unsafeUseAsCStringLen (encodeUtf8 t) act
