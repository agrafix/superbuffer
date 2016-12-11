{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Async
import Control.Monad
import Data.ByteString.SuperBuffer
import Data.Int
import qualified Data.ByteString as BS
import qualified Data.ByteString.SuperBuffer.Pure as P

import Test.Framework
import Test.QuickCheck.Monadic

main :: IO ()
main = htfMain htf_thisModulesTests

test_basic :: IO ()
test_basic =
    do bs <- fillBuf
       assertEqual bs expected
    where
      expected =
          "hello world! Welcome to S U P E R B U F F E R"
      fillBuf =
          withBuffer 8 $ \buf ->
          do appendBuffer buf "hello"
             appendBuffer buf " world"
             appendBuffer buf "!"
             appendBuffer buf " Welcome"
             appendBuffer buf " to"
             appendBuffer buf " S U P E R B U F F E R"

test_nullContained :: IO ()
test_nullContained =
    do bs <- fillBuf
       assertEqual bs expected
    where
      expected =
          "hello\0world"
      fillBuf =
          withBuffer 8 $ \buf ->
          do appendBuffer buf "hello"
             appendBuffer buf "\0world"

test_threaded :: IO ()
test_threaded =
    do bs <- fillBuf
       assertEqual bs expected
    where
      expected =
          "hello world! Welcome to S U P E R B U F F E R"
      fillBuf =
          withBuffer 8 $ \buf ->
          forConcurrently_ ["hello", " world", "!", " Welcome", " to", " S U P E R B U F F E R"] $
          appendBufferT buf

newtype BufferChunks
    = BufferChunks { unBufferChunks :: (Int64, [BS.ByteString]) }
    deriving (Show, Eq)

instance Arbitrary BufferChunks where
    arbitrary = -- 5000 * 200 000 = 1 GB max
        do listSize <- choose (1, 5000)
           chunks <-
               replicateM listSize $
               do bsSize <- choose (0, 200000)
                  pure $ BS.replicate bsSize 84
           bufSize <- choose (1, 1024 * 1024 * 1024)
           pure $ BufferChunks (bufSize, chunks)

prop_appendingWorks :: BufferChunks -> Property
prop_appendingWorks (BufferChunks (bufSize, chunks)) =
    monadicIO $
    do out <- run chunkAction
       assert $ out == BS.concat chunks
    where
      chunkAction =
          withBuffer bufSize $ \buf ->
          forM_ chunks $ appendBuffer buf

test_basicPure :: IO ()
test_basicPure =
    do bs <- fillBuf
       assertEqual bs expected
    where
      expected =
          "hello world! Welcome to S U P E R B U F F E R"
      fillBuf =
          P.withBuffer 8 $ \buf ->
          do P.appendBuffer buf "hello"
             P.appendBuffer buf " world"
             P.appendBuffer buf "!"
             P.appendBuffer buf " Welcome"
             P.appendBuffer buf " to"
             P.appendBuffer buf " S U P E R B U F F E R"

test_nullContainedPure :: IO ()
test_nullContainedPure =
    do bs <- fillBuf
       assertEqual bs expected
    where
      expected =
          "hello\0world"
      fillBuf =
          P.withBuffer 8 $ \buf ->
          do P.appendBuffer buf "hello"
             P.appendBuffer buf "\0world"

test_threadedPure :: IO ()
test_threadedPure =
    do bs <- fillBuf
       assertEqual bs expected
    where
      expected =
          "hello world! Welcome to S U P E R B U F F E R"
      fillBuf =
          P.withBuffer 8 $ \buf ->
          forConcurrently_ ["hello", " world", "!", " Welcome", " to", " S U P E R B U F F E R"] $
          P.appendBufferT buf

prop_appendingWorksPure :: BufferChunks -> Property
prop_appendingWorksPure (BufferChunks (bufSize, chunks)) =
    monadicIO $
    do out <- run chunkAction
       assert $ out == BS.concat chunks
    where
      chunkAction =
          P.withBuffer (fromIntegral bufSize) $ \buf ->
          forM_ chunks $ P.appendBuffer buf
