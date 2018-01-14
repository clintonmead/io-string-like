{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

{-|
The classes in this library roughly encapulates things you can read or write
through a "handle", typically writing something "string like" to a file,
but the interface is very flexible.

Currently only 'String', 'ByteString' and 'Text' in their strict/lazy varieties
and builders are implemented.

However, the interface has the flexibility to write non-character data.
For example an implementation for a @[Int]@ could just write all the 'Int's in
binary to the file and read them back into a @[Int]@.

Also the interface can be defined for "handles" other than standard files.
Currently the only such instance that does not use 'Handle' is for 'hGetContents',
where one can call directly:

> hGetContents fileName

In this case, "handle" is a 'FilePath'.

The initial motivation of to avoid having to change all your function
calls if you change your string type, but it's actually more general than that now.

The point of this library is to be able to read and write raw data to the disk
(and perhaps other places) quickly without thinking too much about it.

It's not intended to be a replacement for say 'Binary'. It's intended to be lower
level. 'Binary' internally creates lazy bytestrings for the user to write to the
disk, this library is instead just about directly writing raw data.

Note that we currently only have classes for

Most of the functions should be fairly self explanatory having the same meaning
as in "Prelude" or "System.IO" but more general.
-}
module System.IO.StringLike.Impl (
  CanGetContents, CanGetContentsClass (hGetContents), getContents, readFile,
  CanGetLine, CanGetLineClass (hGetLine), getLine,
  CanPutStr, CanPutStrClass (hPutStr), putStr, writeFile, appendFile,
  interact,
  CanPutStrLn, CanPutStrLnClass (hPutStrLn), putStrLn,
  CanProxyT, CanProxyTo (canProxyTo), CanProxyFrom (canProxyFrom)
  ) where

import Prelude hiding (
  readFile,
  writeFile,
  appendFile,
  getContents,
  putStr,
  interact,
  getLine,
  putStrLn
  )

import qualified System.IO as IO
import System.IO (Handle, IOMode(ReadMode))
import Data.Semigroup ((<>))

import qualified Data.ByteString.Char8 as BSS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Builder as BSB

import qualified Data.Text as TS
import qualified Data.Text.IO as TS

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import qualified Data.Text.Lazy.Builder as TB

--------------------------
-- CanGetContents Class --
--------------------------

{-|
Type synonym, for the common case of an ordinary 'Handle' in the 'IO'.
-}
type CanGetContents t = CanGetContentsClass IO Handle t

class Monad m => CanGetContentsClass m handleT t where
  {-| Generalised 'System.IO.hGetContents' -}
  hGetContents :: handleT -> m t
  default hGetContents :: (CanProxyFrom t, CanGetContentsClass m handleT (CanProxyT t)) => handleT -> m t
  hGetContents = (canProxyFrom <$>) . hGetContents

getContents :: CanGetContents t => IO t
getContents = hGetContents IO.stdin

readFile :: CanGetContents t => FilePath -> IO t
readFile filePath = IO.withFile filePath ReadMode hGetContents

instance (m ~ IO, CanGetContentsClass m Handle t) => CanGetContentsClass m FilePath t where
  hGetContents = readFile

----------------------
-- CanGetLine class --
----------------------

{-| Type synonym for 'CanGetLineClass'. See 'CanGetContents' for more details. -}
type CanGetLine t = CanGetLineClass IO Handle t

class CanGetContentsClass m handleT t => CanGetLineClass m handleT t where
  {-| Generalised 'System.IO.hGetLine' -}
  hGetLine :: handleT -> m t
  default hGetLine :: (CanProxyFrom t, CanGetLineClass m handleT (CanProxyT t)) => handleT -> m t
  hGetLine = (canProxyFrom <$>) . hGetLine

getLine :: CanGetLine t => IO t
getLine = hGetLine IO.stdin

------------------
-- CanPutStr class --
------------------

{-| Type synonym for 'CanPutStrClass'. See 'CanGetContents' for more details. -}
type CanPutStr t = CanPutStrClass IO Handle t

class Monad m => CanPutStrClass m handleT t where
  {-| Generalised 'System.IO.hPutStr' -}
  hPutStr :: handleT -> t -> m ()
  default hPutStr :: (CanProxyTo t, CanPutStrClass m handleT (CanProxyT t)) => handleT -> t -> m ()
  hPutStr handle str = hPutStr handle (canProxyTo str)

putStr :: CanPutStr t => t -> IO ()
putStr = hPutStr IO.stdout

writeFile :: CanPutStr t => FilePath -> t -> IO ()
writeFile f txt = IO.withFile f IO.WriteMode (\hdl -> hPutStr hdl txt)

appendFile :: CanPutStr t => FilePath -> t -> IO ()
appendFile f txt = IO.withFile f IO.AppendMode (\hdl -> hPutStr hdl txt)

interact :: (CanGetContents t, CanPutStr t) => (t -> t) -> IO ()
interact f = getContents >>= (putStr . f)

instance (m ~ IO, CanPutStrClass m Handle t) => CanPutStrClass m FilePath t where
  hPutStr = appendFile

-------------------------
-- CanPutStrLine class --
-------------------------

{-| Effective type synonym for 'CanPutStrLnClass'. See 'CanGetContents' for more details. -}
type CanPutStrLn t = CanPutStrLnClass IO Handle t

class CanPutStrClass m handleT t => CanPutStrLnClass m handleT t where
  {-| Generalised 'System.IO.hPutStrLn' -}
  hPutStrLn :: handleT -> t -> m ()
  default hPutStrLn :: (CanProxyTo t, CanPutStrLnClass m handleT (CanProxyT t)) => handleT -> t -> m ()
  hPutStrLn handle str = hPutStrLn handle (canProxyTo str)

putStrLn :: CanPutStrLn t => t -> IO ()
putStrLn s = hPutStrLn IO.stdout s

{-|
If you have a data structure where in many cases the simplest way to read or write
to it is just convert it to another you should define an instance of this class.

For example, the simple way to write @ByteString@ 'BSB.Builder's is just to
convert them to and from lazy 'ByteString's.

Defining classese 'CanProxyTo' and 'CanProxyFrom' will define default methods
for many of the other classes in this library.
These can still be overriden if desired but it will save you a lot
of boilerplate if you just which to convert your structure through some other.
-}
type family CanProxyT t

class CanProxyTo t where
  {-| How to convert to the type you will attempt to store -}
  canProxyTo :: t -> CanProxyT t

class CanProxyFrom t where
  {-| How to convert from the type you will attempt to store -}
  canProxyFrom :: CanProxyT t -> t

----------------------
-- String Instances --
----------------------

instance (m ~ IO) => CanGetContentsClass m Handle String where
  hGetContents = IO.hGetContents
instance (m ~ IO) => CanGetLineClass m Handle String where
  hGetLine = IO.hGetLine
instance (m ~ IO) => CanPutStrLnClass m Handle String where
  hPutStrLn = IO.hPutStrLn
instance (m ~ IO) => CanPutStrClass m Handle String where
  hPutStr = IO.hPutStr

---------------------------------
-- Strict Bytestring Instances --
---------------------------------

instance (m ~ IO) => CanGetContentsClass m Handle BSS.ByteString where
  hGetContents = BSS.hGetContents
instance (m ~ IO) => CanGetLineClass m Handle BSS.ByteString where
  hGetLine = BSS.hGetLine

instance (m ~ IO) => CanPutStrClass m Handle BSS.ByteString where
  hPutStr = BSS.hPutStr
instance (m ~ IO) => CanPutStrLnClass m Handle BSS.ByteString where
  hPutStrLn = BSS.hPutStrLn

-------------------------------
-- Lazy Bytestring Instances --
-------------------------------

instance (m ~ IO) => CanGetContentsClass m Handle BSL.ByteString where
  hGetContents = BSL.hGetContents
instance (m ~ IO) => CanGetLineClass m Handle BSL.ByteString where
  hGetLine = (BSL.fromStrict <$>) . BSS.hGetLine

instance (m ~ IO) => CanPutStrClass m Handle BSL.ByteString where
  hPutStr = BSL.hPutStr
instance (m ~ IO) => CanPutStrLnClass m Handle BSL.ByteString where
  hPutStrLn = BSL.hPutStrLn

----------------------------------
-- Bytestring Builder Instances --
----------------------------------

instance (Monad m, CanGetContentsClass m Handle BSL.ByteString) => CanGetContentsClass m Handle BSB.Builder where
instance CanGetLineClass m Handle BSL.ByteString => CanGetLineClass m Handle BSB.Builder

instance (m ~ IO) => CanPutStrClass m Handle BSB.Builder where
  hPutStr = BSB.hPutBuilder
instance CanPutStrClass m Handle BSB.Builder => CanPutStrLnClass m Handle BSB.Builder where
  hPutStrLn handle builder = hPutStr handle (builder <> BSB.char8 '\n')

type instance CanProxyT BSB.Builder = BSL.ByteString

instance CanProxyTo BSB.Builder where
  canProxyTo = BSB.toLazyByteString

instance CanProxyFrom BSB.Builder where
  canProxyFrom = BSB.lazyByteString

---------------------------
-- Strict Text Instances --
---------------------------

instance (m ~ IO) => CanGetContentsClass m Handle TS.Text where
  hGetContents = TS.hGetContents

instance (m ~ IO) => CanGetLineClass m Handle TS.Text where
  hGetLine = TS.hGetLine

instance (m ~ IO) => CanPutStrClass m Handle TS.Text where
  hPutStr = TS.hPutStr

instance (m ~ IO) => CanPutStrLnClass m Handle TS.Text where
  hPutStrLn = TS.hPutStrLn

-------------------------
-- Lazy Text Instances --
-------------------------

instance (m ~ IO) => CanGetContentsClass m Handle TL.Text where
  hGetContents = TL.hGetContents

instance (m ~ IO) => CanGetLineClass m Handle TL.Text where
  hGetLine = TL.hGetLine

instance (m ~ IO) => CanPutStrClass m Handle TL.Text where
  hPutStr = TL.hPutStr

instance (m ~ IO) => CanPutStrLnClass m Handle TL.Text where
  hPutStrLn = TL.hPutStrLn

----------------------------
-- Text Builder Instances --
----------------------------

instance (Monad m, CanGetContentsClass m Handle TL.Text) => CanGetContentsClass m Handle TB.Builder
instance CanGetLineClass m Handle TL.Text => CanGetLineClass m Handle TB.Builder

instance (Monad m, CanPutStrClass m Handle TL.Text) => CanPutStrClass m Handle TB.Builder
instance CanPutStrLnClass m Handle TL.Text => CanPutStrLnClass m Handle TB.Builder

type instance CanProxyT TB.Builder = TL.Text

instance CanProxyTo TB.Builder where
  canProxyTo = TB.toLazyText

instance CanProxyFrom TB.Builder where
  canProxyFrom = TB.fromLazyText
