{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.ByteString.SuperBuffer
    ( SuperBuffer, appendBuffer, withBuffer
    )
where

import Foreign
import Foreign.C
import Control.Exception

import Data.Coerce
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

newtype SuperBuffer
    = SuperBuffer SuperBufferP

withBuffer :: Int64 -> (SuperBuffer -> IO ()) -> IO BS.ByteString
withBuffer size action =
    bracket (newBuffer size) destroyBuffer $ \buf ->
    do ok <- try (action buf)
       case ok of
         Left (exception :: SomeException) ->
             do destroyBufferContents buf
                throwIO exception
         Right () ->
             readBuffer buf -- if something goes to shit here, we could be in trouble...
{-# INLINE withBuffer #-}

newBuffer :: Int64 -> IO SuperBuffer
newBuffer size = SuperBuffer <$> new_sbuf (fromIntegral size)
{-# INLINE newBuffer #-}

appendBuffer :: SuperBuffer -> BS.ByteString -> IO ()
appendBuffer (SuperBuffer ptr) bs =
    BS.unsafeUseAsCStringLen bs $ \(cstr, len) ->
    append_sbuf ptr cstr (fromIntegral len)
{-# INLINE appendBuffer #-}

destroyBuffer :: SuperBuffer -> IO ()
destroyBuffer (SuperBuffer ptr) = destroy_sbuf ptr
{-# INLINE destroyBuffer #-}

destroyBufferContents :: SuperBuffer -> IO ()
destroyBufferContents (SuperBuffer ptr) = destroyContents_sbuf ptr
{-# INLINE destroyBufferContents #-}

-- | Read the final buffer contents. This must only
-- be called once
readBuffer :: SuperBuffer -> IO BS.ByteString
readBuffer (SuperBuffer ptr) =
    do (cstr, size) <- readLocal
       BS.unsafePackCStringFinalizer (coerce cstr) (fromIntegral size) (free cstr)
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
foreign import ccall unsafe "read_sbuf" read_sbuf :: SuperBufferP -> Ptr CSize -> IO CString
foreign import ccall unsafe "destroy_sbuf" destroy_sbuf :: SuperBufferP -> IO ()
foreign import ccall unsafe "destroyContents_sbuf" destroyContents_sbuf :: SuperBufferP -> IO ()
