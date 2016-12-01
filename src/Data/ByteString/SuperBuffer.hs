{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Data.ByteString.SuperBuffer
    ( SuperBuffer
    , newBuffer, destroyBuffer, appendBuffer, readBuffer
    , withBuffer
    )
where

import Foreign
import Foreign.C
import Control.Exception

import qualified Data.ByteString as BS

newtype SuperBuffer
    = SuperBuffer SuperBufferP

withBuffer :: Int64 -> (SuperBuffer -> IO a) -> IO a
withBuffer size = bracket (newBuffer size) destroyBuffer
{-# INLINE withBuffer #-}

newBuffer :: Int64 -> IO SuperBuffer
newBuffer size = SuperBuffer <$> new_sbuf (fromIntegral size)
{-# INLINE newBuffer #-}

destroyBuffer :: SuperBuffer -> IO ()
destroyBuffer (SuperBuffer ptr) = destroy_sbuf ptr
{-# INLINE destroyBuffer #-}

appendBuffer :: SuperBuffer -> BS.ByteString -> IO ()
appendBuffer (SuperBuffer ptr) bs =
    BS.unsafeUseAsCStringLen bs $ \(cstr, len) ->
    append_sbuf ptr cstr (fromIntegral len)
{-# INLINE appendBuffer #-}

readBuffer :: SuperBuffer -> IO BS.ByteString
readBuffer (SuperBuffer ptr) =
    do (cstr, size) <- readLocal
       BS.packCStringLen (cstr, fromIntegral size)
    where
      readLocal =
          alloca $ \sizePtr ->
          do cstr <- read_sbuf ptr sizePtr
             size <- peek sizePtr
             pure (cstr, size)
{-# INLINE readBuffer #-}

data SBuf
type SuperBufferP = Ptr SBuf

foreign import ccall unsafe "new_sbuf" new_sbuf :: CSize -> IO SuperBufferP
foreign import ccall unsafe "append_sbuf" append_sbuf :: SuperBufferP -> CString -> CSize -> IO ()
foreign import ccall unsafe "destroy_sbuf" destroy_sbuf :: SuperBufferP -> IO ()
foreign import ccall unsafe "read_sbuf" read_sbuf :: SuperBufferP -> Ptr CSize -> IO CString
