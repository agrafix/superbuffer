module Main where

import Control.Monad
import Data.ByteString.SuperBuffer
import Data.Int
import qualified Data.BufferBuilder as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB

import Criterion
import Criterion.Main

main :: IO ()
main =
    defaultMain
    [ bgroup "main"
      [ mkGroup "small" 5000 4000
      , mkGroup "med" 500 40000
      , mkGroup "large" 50 400000
      ]
    ]

mkGroup :: String -> Int -> Int -> Benchmark
mkGroup name steps chunkSize =
    bgroup name
    [ bench (bufName iBufSize128) $ nfIO $ BS.reverse <$> buildBuf iBufSize128 steps chunkSize
    , bench (bufName iBufSize) $ nfIO $ BS.reverse <$> buildBuf iBufSize steps chunkSize
    , bench (bufName iBufSize2) $ nfIO $ BS.reverse <$> buildBuf iBufSize2 steps chunkSize
    , bench (bufName iBufSize4) $ nfIO $ BS.reverse <$> buildBuf iBufSize4 steps chunkSize
    , bench (bufName iBufSizeAll) $ nfIO $ BS.reverse <$> buildBuf iBufSizeAll steps chunkSize
    , bench (bufBBName iBufSize128) $ nfIO $ BS.reverse <$> buildBufBB iBufSize128 steps chunkSize
    , bench "bytestring builder" $ nfIO $ BS.reverse <$> buildBufBuilder steps chunkSize
    , bench "bytestring fromChunks" $ nfIO $ BS.reverse <$> buildBufChunks steps chunkSize
    , bench "bytestring concat" $ nfIO $ BS.reverse <$> buildBufConcat steps chunkSize
    ]
    where
      bufBBName is = "buffer-builder (init=" ++ show is ++ " bytes, trim=yes)"
      bufName is = "superbuffer (init=" ++ show is ++ " bytes)"
      iBufSize128 = 128
      iBufSize = fromIntegral chunkSize
      iBufSize2 = 2 * fromIntegral chunkSize
      iBufSize4 = 4 * fromIntegral chunkSize
      iBufSizeAll = fromIntegral $ steps * chunkSize

mkChunk :: Int -> Int -> BS.ByteString
mkChunk step chunkSize =
    BS.replicate chunkSize (fromIntegral $ (step `mod` 100) + 50)
{-# INLINE mkChunk #-}

buildBuf :: Int64 -> Int -> Int -> IO BS.ByteString
buildBuf bufSize steps chunkSize =
    withBuffer bufSize $ \buf ->
    forM_ [0..steps] $ \step ->
    appendBuffer buf (mkChunk step chunkSize)

buildBufBB :: Int64 -> Int -> Int -> IO BS.ByteString
buildBufBB bufSize steps chunkSize =
    pure $ BB.runBufferBuilderWithOptions opts $
    forM_ [0..steps] $ \step ->
    BB.appendBS (mkChunk step chunkSize)
    where
      opts =
          BB.Options
          { BB.initialCapacity = fromIntegral bufSize
          , BB.trimFinalBuffer = True
          }

buildBufBuilder :: Int -> Int -> IO BS.ByteString
buildBufBuilder steps chunkSize =
    BSL.toStrict . BSB.toLazyByteString <$>
    foldM (\b a ->  pure $ b `mappend` BSB.byteString (mkChunk a chunkSize)) mempty [0..steps]

buildBufChunks :: Int -> Int -> IO BS.ByteString
buildBufChunks steps chunkSize =
    BSL.toStrict . BSL.fromChunks <$> (
    forM [0..steps] $ \step ->
            pure (mkChunk step chunkSize))


buildBufConcat :: Int -> Int -> IO BS.ByteString
buildBufConcat steps chunkSize =
    BS.concat <$> (
    forM [0..steps] $ \step ->
            pure (mkChunk step chunkSize))
