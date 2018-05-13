# `text-utf8`: Fast, packed Unicode strings, using stream fusion

This package provides the Data.Text library, a library for the space-
and time-efficient manipulation of Unicode text in Haskell.

**Please refer to the [package description on Hackage](https://hackage.haskell.org/package/text#description) for more information.**

# Get involved!

Please report bugs via the
[github issue tracker](https://github.com/text-utf8/text-utf8/issues).

Master [git repository](https://github.com/text-utf8/text-utf8):

* `git clone git://github.com/text-utf8/text-utf8.git`

# Authors

The base code for this library was originally written by Tom Harper,
based on the stream fusion framework developed by Roman Leshchinskiy,
Duncan Coutts, and Don Stewart.

The core library was fleshed out, debugged, and tested by Bryan
O'Sullivan <bos@serpentine.com>.

This is a fork of `text` using UTF-8 instead of UTF-16 the internal
data representation.
