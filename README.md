# Haskell SuperBuffer

[![CircleCI](https://circleci.com/gh/agrafix/superbuffer.svg?style=svg)](https://circleci.com/gh/agrafix/superbuffer)
[![Hackage](https://img.shields.io/hackage/v/superbuffer.svg)](http://hackage.haskell.org/package/superbuffer)

The `superbuffer` packages was designed to efficiently build up bytestrings from `IO` actions producing
smaller chunks. The goal was to reduce memory overhead as much as possible while still being as fast as possible.
In our use case, it reduced total memory usage of the program from `350 MB` (`bytestring` builder) to `50 MB` (`superbuffer`).
For speed see benchmarks below. Note that the speed heavily depends on a good choice of the initial buffer size and
the size of the chunks written. For small chunks the `superbuffer` outperforms the `bytestring` alternatives consistently. `superbuffer`
also outperforms `buffer-builder`.
The library is currently not thread-safe, but it seems that it could be added (from outside) easily by making sure only one thread
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
time                 21.70 ms   (20.98 ms .. 22.31 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 22.28 ms   (21.90 ms .. 23.01 ms)
std dev              1.112 ms   (744.7 μs .. 1.547 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking main/small/superbuffer (init=4000 bytes)
time                 21.31 ms   (20.99 ms .. 21.87 ms)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 21.97 ms   (21.68 ms .. 22.90 ms)
std dev              1.072 ms   (438.9 μs .. 1.970 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking main/small/superbuffer (init=8000 bytes)
time                 20.85 ms   (20.43 ms .. 21.27 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 21.79 ms   (21.47 ms .. 22.32 ms)
std dev              952.4 μs   (679.5 μs .. 1.455 ms)
variance introduced by outliers: 13% (moderately inflated)

benchmarking main/small/superbuffer (init=16000 bytes)
time                 21.47 ms   (21.01 ms .. 21.92 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 22.25 ms   (21.88 ms .. 22.78 ms)
std dev              1.026 ms   (686.3 μs .. 1.402 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/small/superbuffer (init=20000000 bytes)
time                 33.07 ms   (29.53 ms .. 36.48 ms)
                     0.962 R²   (0.925 R² .. 0.983 R²)
mean                 34.18 ms   (32.02 ms .. 36.56 ms)
std dev              4.503 ms   (3.625 ms .. 6.076 ms)
variance introduced by outliers: 54% (severely inflated)

benchmarking main/small/buffer-builder (init=128 bytes, trim=yes)
time                 29.15 ms   (25.68 ms .. 32.01 ms)
                     0.948 R²   (0.884 R² .. 0.986 R²)
mean                 25.96 ms   (24.79 ms .. 28.37 ms)
std dev              3.282 ms   (1.946 ms .. 5.556 ms)
variance introduced by outliers: 54% (severely inflated)

benchmarking main/small/bytestring builder
time                 27.18 ms   (24.81 ms .. 28.88 ms)
                     0.976 R²   (0.951 R² .. 0.990 R²)
mean                 30.89 ms   (29.41 ms .. 32.26 ms)
std dev              2.819 ms   (2.413 ms .. 3.238 ms)
variance introduced by outliers: 34% (moderately inflated)

benchmarking main/small/bytestring fromChunks
time                 26.17 ms   (25.77 ms .. 26.63 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 27.01 ms   (26.57 ms .. 27.70 ms)
std dev              1.145 ms   (660.7 μs .. 2.009 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking main/small/bytestring concat
time                 25.75 ms   (25.25 ms .. 26.36 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 26.69 ms   (26.15 ms .. 27.76 ms)
std dev              1.613 ms   (803.3 μs .. 2.776 ms)
variance introduced by outliers: 21% (moderately inflated)

benchmarking main/med/superbuffer (init=128 bytes)
time                 21.49 ms   (21.19 ms .. 21.81 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 22.24 ms   (22.01 ms .. 22.61 ms)
std dev              669.6 μs   (486.0 μs .. 903.9 μs)

benchmarking main/med/superbuffer (init=40000 bytes)
time                 29.59 ms   (29.21 ms .. 30.15 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 29.81 ms   (29.59 ms .. 30.03 ms)
std dev              478.5 μs   (366.1 μs .. 660.5 μs)

benchmarking main/med/superbuffer (init=80000 bytes)
time                 21.55 ms   (21.03 ms .. 22.04 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 22.02 ms   (21.79 ms .. 22.32 ms)
std dev              648.6 μs   (487.1 μs .. 906.9 μs)

benchmarking main/med/superbuffer (init=160000 bytes)
time                 22.19 ms   (21.46 ms .. 22.94 ms)
                     0.994 R²   (0.987 R² .. 0.998 R²)
mean                 23.00 ms   (22.62 ms .. 23.53 ms)
std dev              1.043 ms   (769.7 μs .. 1.480 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/med/superbuffer (init=20000000 bytes)
time                 36.96 ms   (35.70 ms .. 38.70 ms)
                     0.994 R²   (0.984 R² .. 0.999 R²)
mean                 37.07 ms   (36.43 ms .. 37.82 ms)
std dev              1.417 ms   (1.030 ms .. 2.070 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking main/med/buffer-builder (init=128 bytes, trim=yes)
time                 30.10 ms   (24.33 ms .. 35.84 ms)
                     0.912 R²   (0.859 R² .. 0.979 R²)
mean                 24.90 ms   (23.72 ms .. 27.34 ms)
std dev              3.821 ms   (2.118 ms .. 6.560 ms)
variance introduced by outliers: 67% (severely inflated)

benchmarking main/med/bytestring builder
time                 22.82 ms   (22.29 ms .. 23.27 ms)
                     0.998 R²   (0.994 R² .. 0.999 R²)
mean                 23.33 ms   (23.04 ms .. 23.63 ms)
std dev              703.9 μs   (509.3 μs .. 981.7 μs)

benchmarking main/med/bytestring fromChunks
time                 22.86 ms   (22.53 ms .. 23.25 ms)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 22.90 ms   (22.65 ms .. 23.20 ms)
std dev              609.2 μs   (442.9 μs .. 831.6 μs)

benchmarking main/med/bytestring concat
time                 24.86 ms   (22.85 ms .. 26.74 ms)
                     0.984 R²   (0.976 R² .. 0.998 R²)
mean                 23.51 ms   (22.96 ms .. 24.31 ms)
std dev              1.570 ms   (953.6 μs .. 2.256 ms)
variance introduced by outliers: 28% (moderately inflated)

benchmarking main/large/superbuffer (init=128 bytes)
time                 23.60 ms   (22.51 ms .. 24.50 ms)
                     0.996 R²   (0.992 R² .. 0.999 R²)
mean                 24.14 ms   (23.74 ms .. 24.57 ms)
std dev              939.3 μs   (665.5 μs .. 1.360 ms)

benchmarking main/large/superbuffer (init=400000 bytes)
time                 23.55 ms   (22.64 ms .. 24.31 ms)
                     0.996 R²   (0.992 R² .. 0.998 R²)
mean                 22.62 ms   (22.36 ms .. 22.95 ms)
std dev              716.8 μs   (506.9 μs .. 980.9 μs)

benchmarking main/large/superbuffer (init=800000 bytes)
time                 24.24 ms   (22.85 ms .. 25.31 ms)
                     0.991 R²   (0.985 R² .. 0.996 R²)
mean                 23.21 ms   (22.65 ms .. 24.27 ms)
std dev              1.560 ms   (1.070 ms .. 2.485 ms)
variance introduced by outliers: 28% (moderately inflated)

benchmarking main/large/superbuffer (init=1600000 bytes)
time                 30.69 ms   (29.80 ms .. 31.81 ms)
                     0.996 R²   (0.992 R² .. 0.999 R²)
mean                 30.76 ms   (29.26 ms .. 31.58 ms)
std dev              2.243 ms   (956.5 μs .. 4.314 ms)
variance introduced by outliers: 28% (moderately inflated)

benchmarking main/large/superbuffer (init=20000000 bytes)
time                 38.13 ms   (37.06 ms .. 39.53 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 36.56 ms   (34.58 ms .. 37.69 ms)
std dev              2.833 ms   (925.6 μs .. 3.997 ms)
variance introduced by outliers: 25% (moderately inflated)

benchmarking main/large/buffer-builder (init=128 bytes, trim=yes)
time                 28.11 ms   (27.68 ms .. 28.62 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 27.91 ms   (27.62 ms .. 28.17 ms)
std dev              593.2 μs   (449.2 μs .. 787.0 μs)

benchmarking main/large/bytestring builder
time                 22.41 ms   (22.13 ms .. 22.68 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 22.72 ms   (22.47 ms .. 23.78 ms)
std dev              977.4 μs   (252.5 μs .. 1.965 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/large/bytestring fromChunks
time                 22.85 ms   (22.42 ms .. 23.29 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 23.24 ms   (22.89 ms .. 24.31 ms)
std dev              1.307 ms   (330.0 μs .. 2.558 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking main/large/bytestring concat
time                 22.56 ms   (22.16 ms .. 22.91 ms)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 23.24 ms   (22.90 ms .. 23.92 ms)
std dev              1.055 ms   (544.5 μs .. 1.855 ms)
variance introduced by outliers: 14% (moderately inflated)
```
