{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
-- |
-- Module      : Data.Text.Unsafe
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- A module containing unsafe 'Text' operations, for very very careful
-- use in heavily tested code.
module Data.Text.Unsafe
    (
      inlineInterleaveST
    , inlinePerformIO
    , unsafeDupablePerformIO
    , Iter(..)
    , iter
    , iter_
    , reverseIter
    , reverseIter_
    , unsafeHead
    , unsafeTail
    , lengthWord8
    , takeWord8
    , dropWord8
    ) where

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Data.Text.Internal.Encoding.Utf8 (decodeCharIndex, reverseDecodeCharIndex)
import Data.Text.Internal.Encoding.Utf16 (chr2)
import Data.Text.Internal (Text(..))
import Data.Text.Internal.Unsafe (inlineInterleaveST, inlinePerformIO)
import Data.Text.Internal.Unsafe.Char (unsafeChr)
import qualified Data.Text.Array as A
import GHC.IO (unsafeDupablePerformIO)

-- | /O(1)/ A variant of 'head' for non-empty 'Text'. 'unsafeHead'
-- omits the check for the empty case, so there is an obligation on
-- the programmer to provide a proof that the 'Text' is non-empty.
unsafeHead :: Text -> Char
unsafeHead (Text arr off _len) =
  decodeCharIndex (\c _ -> c) (A.unsafeIndex arr) off
{-# INLINE unsafeHead #-}

-- | /O(1)/ A variant of 'tail' for non-empty 'Text'. 'unsafeTail'
-- omits the check for the empty case, so there is an obligation on
-- the programmer to provide a proof that the 'Text' is non-empty.
unsafeTail :: Text -> Text
unsafeTail t@(Text arr off len) =
#if defined(ASSERTS)
    assert (d <= len) $
#endif
    Text arr (off+d) (len-d)
  where d = iter_ t 0
{-# INLINE unsafeTail #-}

data Iter = Iter {-# UNPACK #-} !Char {-# UNPACK #-} !Int

-- | /O(1)/ Iterate (unsafely) one step forwards through a UTF-8
-- array, returning the current character and the delta to add to give
-- the next offset to iterate at.
iter :: Text -> Int -> Iter
iter (Text arr off _len) i =
  decodeCharIndex (\c d -> Iter c d) (A.unsafeIndex arr) (off + i)
{-# INLINE iter #-}

-- | /O(1)/ Iterate one step through a UTF-8 array, returning the
-- delta to add to give the next offset to iterate at.
iter_ :: Text -> Int -> Int
iter_ (Text arr off _len) i =
  decodeCharIndex (\_ n -> n) (\x -> A.unsafeIndex arr (x + off)) i
{-# INLINE iter_ #-}

-- | /O(1)/ Iterate one step backwards through a UTF-8 array,
-- returning the current character and the delta to add (i.e. a
-- negative number) to give the next offset to iterate at.
reverseIter :: Text -> Int -> (Char,Int)
reverseIter (Text arr off _len) i =
    reverseDecodeCharIndex (\c s -> (c, -s)) idx (off + i)
  where
    idx = A.unsafeIndex arr
{-# INLINE reverseIter #-}

-- | /O(1)/ Iterate one step backwards through a UTF-8 array,
-- returning the delta to add (i.e. a negative number) to give the
-- next offset to iterate at.
reverseIter_ :: Text -> Int -> Int
reverseIter_ (Text arr off _len) i =
  reverseDecodeCharIndex (\_ n -> -n) (\x -> A.unsafeIndex arr (x + off)) i
{-# INLINE reverseIter_ #-}

-- | /O(1)/ Return the length of a 'Text' in units of 'Word16'.  This
-- is useful for sizing a target array appropriately before using
-- 'unsafeCopyToPtr'.
lengthWord8 :: Text -> Int
lengthWord8 (Text _arr _off len) = len
{-# INLINE lengthWord8 #-}

-- | /O(1)/ Unchecked take of 'k' 'Word16's from the front of a 'Text'.
takeWord8 :: Int -> Text -> Text
takeWord8 k (Text arr off _len) = Text arr off k
{-# INLINE takeWord8 #-}

-- | /O(1)/ Unchecked drop of 'k' 'Word16's from the front of a 'Text'.
dropWord8 :: Int -> Text -> Text
dropWord8 k (Text arr off len) = Text arr (off+k) (len-k)
{-# INLINE dropWord8 #-}
