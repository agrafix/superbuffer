{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.ByteString.SuperBuffer
    ( SuperBuffer, withBuffer, appendBuffer
    )
where

import Foreign
import Foreign.C
import Control.Exception

import Data.Coerce
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

-- | The buffer. Internally only a pointer to a C struct. Don't worry,
-- this module attempts to make usage of the SuperBuffer as safe as possible  in
-- terms of memory leaks (even when exceptions occur).
newtype SuperBuffer
    = SuperBuffer SuperBufferP

-- | Allocate a new buffer with a given initial size. The perfect starting point
-- depends on the expected total size and the average size for a single chunk
-- written with 'appendBuffer'. You can always start with 1024 and optimize from
-- there with benchmarks. Please note that the SuperBuffer will no longer be
-- valid after this function terminates, so do NOT pass it to some other
-- thread without waiting for it to finish in the action.
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


-- | Write a bytestring to the buffer and grow the buffer if needed. Note that only
-- one thread at any given time may call this function, so if you are sharing the
-- 'SuperBuffer' between threads make sure you place some type of guarding/locking around
-- this function.
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
