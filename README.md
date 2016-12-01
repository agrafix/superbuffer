# Haskell SuperBuffer

[![CircleCI](https://circleci.com/gh/agrafix/superbuffer.svg?style=svg)](https://circleci.com/gh/agrafix/superbuffer)
[![Hackage](https://img.shields.io/hackage/v/superbuffer.svg)](http://hackage.haskell.org/package/superbuffer)

The `superbuffer` packages was designed to efficiently build up bytestrings from `IO` actions producing
smaller chunks. The goal was to reduce memory overhead as much as possible while still being as fast as possible.
In our use case, it reduced total memory usage of the program from `350 MB` (`bytestring` builder) to `50 MB` (`superbuffer`).
For speed see benchmarks below. Note that the speed heavily depends on a good choice of the initial buffer size and
the size of the chunks written. For small chunks the `superbuffer` outperforms the `bytestring` alternatives consistently. Note
that the library is currently not thread-safe, but it seems that it could be added (from outside) easily by making sure only one thread
at a time can call `appendBuffer` (making the call slower).

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Example where

import Data.ByteString.SuperBuffer
import qualified Data.ByteString as BS

myBS :: IO BS.ByteString
myBS =
    -- note: performance of superbuffer heavily depends on a
    -- smart choice of the initial buffer size. Benchmark to
    -- find what suits your needs.
    withBuffer 1024 $ \buf ->
    do appendBuffer buf "Hello "
       appendBuffer buf "World!"
```

## Benchmarks

```
benchmarking main/small/superbuffer (init=128 bytes)
time                 22.20 ms   (20.81 ms .. 24.69 ms)
                     0.973 R²   (0.946 R² .. 0.992 R²)
mean                 22.47 ms   (21.70 ms .. 23.45 ms)
std dev              1.905 ms   (1.515 ms .. 2.685 ms)
variance introduced by outliers: 38% (moderately inflated)

benchmarking main/small/superbuffer (init=4000 bytes)
time                 19.92 ms   (19.28 ms .. 20.58 ms)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 22.30 ms   (21.56 ms .. 23.60 ms)
std dev              2.230 ms   (1.324 ms .. 3.614 ms)
variance introduced by outliers: 46% (moderately inflated)

benchmarking main/small/superbuffer (init=8000 bytes)
time                 22.72 ms   (21.85 ms .. 23.92 ms)
                     0.994 R²   (0.989 R² .. 0.997 R²)
mean                 23.25 ms   (22.73 ms .. 23.95 ms)
std dev              1.496 ms   (1.007 ms .. 2.375 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking main/small/superbuffer (init=16000 bytes)
time                 23.28 ms   (21.77 ms .. 25.14 ms)
                     0.978 R²   (0.953 R² .. 0.994 R²)
mean                 23.41 ms   (22.79 ms .. 24.34 ms)
std dev              1.727 ms   (1.209 ms .. 2.455 ms)
variance introduced by outliers: 29% (moderately inflated)

benchmarking main/small/superbuffer (init=20000000 bytes)
time                 35.78 ms   (34.57 ms .. 37.21 ms)
                     0.989 R²   (0.972 R² .. 0.998 R²)
mean                 36.45 ms   (35.25 ms .. 37.34 ms)
std dev              2.167 ms   (1.281 ms .. 3.677 ms)
variance introduced by outliers: 18% (moderately inflated)

benchmarking main/small/bytestring builder
time                 31.34 ms   (28.98 ms .. 33.54 ms)
                     0.987 R²   (0.974 R² .. 0.999 R²)
mean                 30.73 ms   (29.54 ms .. 31.59 ms)
std dev              2.205 ms   (1.699 ms .. 2.841 ms)
variance introduced by outliers: 28% (moderately inflated)

benchmarking main/small/bytestring fromChunks
time                 26.35 ms   (25.53 ms .. 27.40 ms)
                     0.996 R²   (0.991 R² .. 0.999 R²)
mean                 26.35 ms   (25.88 ms .. 27.79 ms)
std dev              1.685 ms   (675.3 μs .. 3.068 ms)
variance introduced by outliers: 25% (moderately inflated)

benchmarking main/small/bytestring concat
time                 30.54 ms   (29.83 ms .. 31.13 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 31.45 ms   (30.94 ms .. 33.55 ms)
std dev              1.658 ms   (563.0 μs .. 3.317 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking main/med/superbuffer (init=128 bytes)
time                 20.98 ms   (20.11 ms .. 21.97 ms)
                     0.992 R²   (0.983 R² .. 0.997 R²)
mean                 23.11 ms   (22.53 ms .. 24.12 ms)
std dev              1.686 ms   (1.020 ms .. 2.914 ms)
variance introduced by outliers: 29% (moderately inflated)

benchmarking main/med/superbuffer (init=40000 bytes)
time                 21.62 ms   (20.92 ms .. 22.43 ms)
                     0.996 R²   (0.991 R² .. 0.998 R²)
mean                 23.65 ms   (23.06 ms .. 24.72 ms)
std dev              1.735 ms   (1.044 ms .. 2.732 ms)
variance introduced by outliers: 29% (moderately inflated)

benchmarking main/med/superbuffer (init=80000 bytes)
time                 20.65 ms   (19.91 ms .. 21.35 ms)
                     0.994 R²   (0.985 R² .. 0.998 R²)
mean                 22.64 ms   (22.09 ms .. 23.84 ms)
std dev              1.767 ms   (971.0 μs .. 3.008 ms)
variance introduced by outliers: 33% (moderately inflated)

benchmarking main/med/superbuffer (init=160000 bytes)
time                 23.35 ms   (22.15 ms .. 24.56 ms)
                     0.992 R²   (0.987 R² .. 0.998 R²)
mean                 22.50 ms   (22.15 ms .. 23.08 ms)
std dev              1.038 ms   (639.2 μs .. 1.502 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/med/superbuffer (init=20000000 bytes)
time                 35.02 ms   (31.20 ms .. 37.83 ms)
                     0.983 R²   (0.970 R² .. 0.998 R²)
mean                 32.02 ms   (31.12 ms .. 33.32 ms)
std dev              2.159 ms   (1.456 ms .. 3.304 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking main/med/bytestring builder
time                 22.03 ms   (21.53 ms .. 22.48 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 22.50 ms   (22.06 ms .. 23.71 ms)
std dev              1.614 ms   (467.3 μs .. 3.159 ms)
variance introduced by outliers: 29% (moderately inflated)

benchmarking main/med/bytestring fromChunks
time                 23.88 ms   (21.62 ms .. 26.14 ms)
                     0.975 R²   (0.954 R² .. 0.998 R²)
mean                 22.81 ms   (22.25 ms .. 23.95 ms)
std dev              1.812 ms   (784.9 μs .. 2.692 ms)
variance introduced by outliers: 33% (moderately inflated)

benchmarking main/med/bytestring concat
time                 21.79 ms   (21.28 ms .. 22.23 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 22.52 ms   (22.05 ms .. 23.70 ms)
std dev              1.646 ms   (554.1 μs .. 3.042 ms)
variance introduced by outliers: 29% (moderately inflated)

benchmarking main/large/superbuffer (init=128 bytes)
time                 25.97 ms   (25.22 ms .. 26.62 ms)
                     0.994 R²   (0.984 R² .. 0.999 R²)
mean                 26.01 ms   (25.18 ms .. 26.90 ms)
std dev              1.943 ms   (1.274 ms .. 3.107 ms)
variance introduced by outliers: 30% (moderately inflated)

benchmarking main/large/superbuffer (init=400000 bytes)
time                 21.51 ms   (21.13 ms .. 21.88 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 22.14 ms   (21.84 ms .. 22.75 ms)
std dev              939.7 μs   (407.0 μs .. 1.669 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/large/superbuffer (init=800000 bytes)
time                 21.76 ms   (21.35 ms .. 22.34 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 22.09 ms   (21.77 ms .. 22.54 ms)
std dev              858.0 μs   (532.7 μs .. 1.421 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/large/superbuffer (init=1600000 bytes)
time                 31.34 ms   (30.55 ms .. 32.36 ms)
                     0.997 R²   (0.992 R² .. 0.999 R²)
mean                 30.42 ms   (29.34 ms .. 31.35 ms)
std dev              2.043 ms   (1.383 ms .. 3.018 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking main/large/superbuffer (init=20000000 bytes)
time                 38.79 ms   (36.27 ms .. 41.72 ms)
                     0.983 R²   (0.964 R² .. 0.995 R²)
mean                 33.84 ms   (32.32 ms .. 35.51 ms)
std dev              3.268 ms   (2.816 ms .. 3.755 ms)
variance introduced by outliers: 36% (moderately inflated)

benchmarking main/large/bytestring builder
time                 21.89 ms   (21.35 ms .. 22.37 ms)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 23.02 ms   (22.46 ms .. 24.92 ms)
std dev              2.169 ms   (461.3 μs .. 4.197 ms)
variance introduced by outliers: 43% (moderately inflated)

benchmarking main/large/bytestring fromChunks
time                 21.91 ms   (21.49 ms .. 22.31 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 22.47 ms   (22.13 ms .. 23.63 ms)
std dev              1.230 ms   (383.5 μs .. 2.326 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking main/large/bytestring concat
time                 22.12 ms   (21.82 ms .. 22.42 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 22.60 ms   (22.27 ms .. 23.51 ms)
std dev              1.198 ms   (404.6 μs .. 2.244 ms)
variance introduced by outliers: 19% (moderately inflated)
```
