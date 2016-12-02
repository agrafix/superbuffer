module Main where

import Control.Concurrent.Async
import Control.Monad
import Criterion
import Criterion.Main
import Data.ByteString.SuperBuffer
import Data.Int
import qualified Data.BufferBuilder as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL

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
    bgroup name $
    mkSizedGroup steps chunkSize bufName buildBuf
    ++ mkSizedGroup steps chunkSize bufNameT buildBufT
    ++ mkSizedGroup steps chunkSize bufBBName buildBufBB
    ++
    [ bench "bytestring builder" $ nfIO $ BS.reverse <$> buildBufBuilder steps chunkSize
    , bench "bytestring fromChunks" $ nfIO $ BS.reverse <$> buildBufChunks steps chunkSize
    , bench "bytestring concat" $ nfIO $ BS.reverse <$> buildBufConcat steps chunkSize
    ]
    where
      bufBBName is = "buffer-builder (init=" ++ show is ++ " bytes, trim=yes)"
      bufName is = "superbuffer (init=" ++ show is ++ " bytes)"
      bufNameT is = "superbuffer (init=" ++ show is ++ " bytes, threadsafe, 2 concurrent writes)"

mkSizedGroup ::
    Int -> Int -> (Int64 -> String) -> (Int64 -> Int -> Int -> IO BS.ByteString) -> [Benchmark]
mkSizedGroup steps chunkSize bufName builder =
    [ bench (bufName iBufSize128) $ nfIO $ BS.reverse <$> builder iBufSize128 steps chunkSize
    , bench (bufName iBufSize) $ nfIO $ BS.reverse <$> builder iBufSize steps chunkSize
    , bench (bufName iBufSize2) $ nfIO $ BS.reverse <$> builder iBufSize2 steps chunkSize
    , bench (bufName iBufSize4) $ nfIO $ BS.reverse <$> builder iBufSize4 steps chunkSize
    , bench (bufName iBufSizeAll) $ nfIO $ BS.reverse <$> builder iBufSizeAll steps chunkSize
    ]
    where
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

buildBufT :: Int64 -> Int -> Int -> IO BS.ByteString
buildBufT bufSize steps chunkSize =
    withBuffer bufSize $ \buf ->
    forM_ [0..(ceiling halfSteps)] $ \step ->
    concurrently_
       (appendBufferT buf (mkChunk step chunkSize))
       (appendBufferT buf (mkChunk step chunkSize))
    where
      halfSteps :: Double
      halfSteps = fromIntegral steps / 2.0

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
