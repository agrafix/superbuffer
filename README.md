# Haskell SuperBuffer

[![CircleCI](https://circleci.com/gh/agrafix/superbuffer.svg?style=svg)](https://circleci.com/gh/agrafix/superbuffer)
[![Hackage](https://img.shields.io/hackage/v/superbuffer.svg)](http://hackage.haskell.org/package/superbuffer)

The `superbuffer` packages was designed to efficiently build up bytestrings from `IO` actions producing
smaller chunks. The goal was to reduce memory overhead as much as possible while still being as fast as possible.
In our use case, it reduced total memory usage of the program from `350 MB` (`bytestring` builder) to `50 MB` (`superbuffer`).
For speed see benchmarks below. Note that the speed heavily depends on a good choice of the initial buffer size and
the size of the chunks written. For small chunks the `superbuffer` outperforms the `bytestring` alternatives consistently. `superbuffer`
also outperforms `buffer-builder`.

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
time                 20.69 ms   (20.18 ms .. 21.25 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 21.79 ms   (21.35 ms .. 22.38 ms)
std dev              1.252 ms   (699.1 μs .. 1.804 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking main/small/superbuffer (init=4000 bytes)
time                 21.39 ms   (20.63 ms .. 22.61 ms)
                     0.992 R²   (0.984 R² .. 0.997 R²)
mean                 22.40 ms   (21.98 ms .. 22.86 ms)
std dev              997.0 μs   (831.8 μs .. 1.181 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/small/superbuffer (init=8000 bytes)
time                 20.94 ms   (20.45 ms .. 21.41 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 21.80 ms   (21.40 ms .. 22.64 ms)
std dev              1.249 ms   (488.7 μs .. 1.935 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking main/small/superbuffer (init=16000 bytes)
time                 21.86 ms   (21.52 ms .. 22.29 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 21.90 ms   (21.64 ms .. 22.32 ms)
std dev              718.4 μs   (447.8 μs .. 1.181 ms)

benchmarking main/small/superbuffer (init=20000000 bytes)
time                 38.11 ms   (36.81 ms .. 39.41 ms)
                     0.995 R²   (0.988 R² .. 0.998 R²)
mean                 35.36 ms   (33.46 ms .. 36.66 ms)
std dev              3.174 ms   (2.018 ms .. 4.414 ms)
variance introduced by outliers: 36% (moderately inflated)

benchmarking main/small/superbuffer (init=128 bytes, threadsafe, 2 concurrent writes)
time                 26.21 ms   (25.50 ms .. 27.02 ms)
                     0.996 R²   (0.992 R² .. 0.998 R²)
mean                 25.38 ms   (24.94 ms .. 25.83 ms)
std dev              1.015 ms   (828.2 μs .. 1.274 ms)

benchmarking main/small/superbuffer (init=4000 bytes, threadsafe, 2 concurrent writes)
time                 34.67 ms   (33.62 ms .. 35.59 ms)
                     0.996 R²   (0.990 R² .. 0.999 R²)
mean                 32.54 ms   (30.79 ms .. 33.70 ms)
std dev              2.867 ms   (1.986 ms .. 3.938 ms)
variance introduced by outliers: 36% (moderately inflated)

benchmarking main/small/superbuffer (init=8000 bytes, threadsafe, 2 concurrent writes)
time                 31.54 ms   (30.29 ms .. 32.49 ms)
                     0.994 R²   (0.988 R² .. 0.998 R²)
mean                 31.80 ms   (31.03 ms .. 32.62 ms)
std dev              1.600 ms   (1.285 ms .. 2.193 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking main/small/superbuffer (init=16000 bytes, threadsafe, 2 concurrent writes)
time                 33.81 ms   (32.98 ms .. 34.81 ms)
                     0.996 R²   (0.991 R² .. 0.999 R²)
mean                 30.72 ms   (29.28 ms .. 31.86 ms)
std dev              2.803 ms   (2.157 ms .. 3.648 ms)
variance introduced by outliers: 34% (moderately inflated)

benchmarking main/small/superbuffer (init=20000000 bytes, threadsafe, 2 concurrent writes)
time                 18.96 ms   (18.51 ms .. 19.47 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 19.32 ms   (19.05 ms .. 19.78 ms)
std dev              819.1 μs   (465.1 μs .. 1.158 ms)
variance introduced by outliers: 13% (moderately inflated)

benchmarking main/small/buffer-builder (init=128 bytes, trim=yes)
time                 28.90 ms   (27.46 ms .. 30.66 ms)
                     0.991 R²   (0.982 R² .. 0.999 R²)
mean                 28.51 ms   (27.96 ms .. 29.19 ms)
std dev              1.321 ms   (914.7 μs .. 1.826 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking main/small/buffer-builder (init=4000 bytes, trim=yes)
time                 24.84 ms   (24.26 ms .. 25.42 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 24.89 ms   (24.64 ms .. 25.13 ms)
std dev              599.3 μs   (498.5 μs .. 738.3 μs)

benchmarking main/small/buffer-builder (init=8000 bytes, trim=yes)
time                 25.31 ms   (24.89 ms .. 25.86 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 25.16 ms   (24.89 ms .. 25.40 ms)
std dev              561.7 μs   (441.5 μs .. 740.7 μs)

benchmarking main/small/buffer-builder (init=16000 bytes, trim=yes)
time                 25.16 ms   (24.64 ms .. 25.67 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 25.00 ms   (24.71 ms .. 25.29 ms)
std dev              656.4 μs   (502.2 μs .. 849.0 μs)

benchmarking main/small/buffer-builder (init=20000000 bytes, trim=yes)
time                 26.82 ms   (21.08 ms .. 36.36 ms)
                     0.808 R²   (0.744 R² .. 0.999 R²)
mean                 22.40 ms   (21.28 ms .. 26.63 ms)
std dev              4.332 ms   (512.5 μs .. 8.479 ms)
variance introduced by outliers: 72% (severely inflated)

benchmarking main/small/bytestring builder
time                 29.94 ms   (27.48 ms .. 32.70 ms)
                     0.983 R²   (0.968 R² .. 0.999 R²)
mean                 31.64 ms   (30.78 ms .. 32.14 ms)
std dev              1.435 ms   (452.0 μs .. 2.120 ms)
variance introduced by outliers: 11% (moderately inflated)

benchmarking main/small/bytestring fromChunks
time                 26.12 ms   (25.64 ms .. 26.60 ms)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 26.43 ms   (26.02 ms .. 27.23 ms)
std dev              1.229 ms   (671.2 μs .. 2.004 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking main/small/bytestring concat
time                 26.74 ms   (25.57 ms .. 28.26 ms)
                     0.993 R²   (0.988 R² .. 0.999 R²)
mean                 26.14 ms   (25.72 ms .. 26.89 ms)
std dev              1.152 ms   (450.9 μs .. 1.703 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking main/med/superbuffer (init=128 bytes)
time                 23.30 ms   (22.09 ms .. 24.39 ms)
                     0.991 R²   (0.986 R² .. 0.996 R²)
mean                 22.32 ms   (21.82 ms .. 22.98 ms)
std dev              1.315 ms   (966.3 μs .. 1.872 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking main/med/superbuffer (init=40000 bytes)
time                 24.98 ms   (24.71 ms .. 25.24 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 25.30 ms   (25.09 ms .. 25.70 ms)
std dev              609.2 μs   (346.4 μs .. 1.017 ms)

benchmarking main/med/superbuffer (init=80000 bytes)
time                 20.93 ms   (20.43 ms .. 21.26 ms)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 22.82 ms   (22.30 ms .. 23.63 ms)
std dev              1.499 ms   (1.115 ms .. 2.075 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking main/med/superbuffer (init=160000 bytes)
time                 22.90 ms   (22.48 ms .. 23.39 ms)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 23.07 ms   (22.76 ms .. 23.32 ms)
std dev              647.4 μs   (469.5 μs .. 928.6 μs)

benchmarking main/med/superbuffer (init=20000000 bytes)
time                 37.53 ms   (36.30 ms .. 39.25 ms)
                     0.995 R²   (0.990 R² .. 0.998 R²)
mean                 33.78 ms   (32.04 ms .. 35.11 ms)
std dev              3.209 ms   (2.374 ms .. 4.178 ms)
variance introduced by outliers: 36% (moderately inflated)

benchmarking main/med/superbuffer (init=128 bytes, threadsafe, 2 concurrent writes)
time                 23.38 ms   (22.62 ms .. 24.39 ms)
                     0.995 R²   (0.990 R² .. 0.999 R²)
mean                 23.25 ms   (22.88 ms .. 23.86 ms)
std dev              1.059 ms   (684.7 μs .. 1.649 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/med/superbuffer (init=40000 bytes, threadsafe, 2 concurrent writes)
time                 20.71 ms   (19.29 ms .. 21.60 ms)
                     0.984 R²   (0.965 R² .. 0.997 R²)
mean                 23.39 ms   (22.47 ms .. 24.91 ms)
std dev              2.656 ms   (1.445 ms .. 4.134 ms)
variance introduced by outliers: 53% (severely inflated)

benchmarking main/med/superbuffer (init=80000 bytes, threadsafe, 2 concurrent writes)
time                 22.01 ms   (21.56 ms .. 22.63 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 22.14 ms   (21.90 ms .. 22.51 ms)
std dev              701.7 μs   (429.9 μs .. 927.8 μs)

benchmarking main/med/superbuffer (init=160000 bytes, threadsafe, 2 concurrent writes)
time                 21.90 ms   (21.60 ms .. 22.27 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 22.06 ms   (21.86 ms .. 22.31 ms)
std dev              508.7 μs   (386.9 μs .. 661.0 μs)

benchmarking main/med/superbuffer (init=20000000 bytes, threadsafe, 2 concurrent writes)
time                 16.40 ms   (15.91 ms .. 16.82 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 16.62 ms   (16.41 ms .. 17.18 ms)
std dev              832.9 μs   (303.1 μs .. 1.620 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking main/med/buffer-builder (init=128 bytes, trim=yes)
time                 28.20 ms   (25.06 ms .. 30.59 ms)
                     0.975 R²   (0.965 R² .. 0.988 R²)
mean                 24.56 ms   (23.74 ms .. 25.91 ms)
std dev              2.181 ms   (1.301 ms .. 2.714 ms)
variance introduced by outliers: 35% (moderately inflated)

benchmarking main/med/buffer-builder (init=40000 bytes, trim=yes)
time                 23.99 ms   (23.69 ms .. 24.34 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 23.72 ms   (23.50 ms .. 23.97 ms)
std dev              531.5 μs   (405.4 μs .. 771.5 μs)

benchmarking main/med/buffer-builder (init=80000 bytes, trim=yes)
time                 21.96 ms   (21.57 ms .. 22.41 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 22.18 ms   (21.96 ms .. 22.49 ms)
std dev              639.7 μs   (406.5 μs .. 1.025 ms)

benchmarking main/med/buffer-builder (init=160000 bytes, trim=yes)
time                 22.29 ms   (21.98 ms .. 22.60 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 21.84 ms   (21.61 ms .. 22.04 ms)
std dev              461.6 μs   (367.1 μs .. 581.4 μs)

benchmarking main/med/buffer-builder (init=20000000 bytes, trim=yes)
time                 39.52 ms   (29.98 ms .. 47.38 ms)
                     0.876 R²   (0.799 R² .. 0.949 R²)
mean                 26.21 ms   (23.50 ms .. 30.85 ms)
std dev              7.684 ms   (4.775 ms .. 9.081 ms)
variance introduced by outliers: 88% (severely inflated)

benchmarking main/med/bytestring builder
time                 21.52 ms   (20.81 ms .. 22.22 ms)
                     0.996 R²   (0.993 R² .. 0.999 R²)
mean                 23.98 ms   (23.12 ms .. 25.68 ms)
std dev              2.780 ms   (1.529 ms .. 4.683 ms)
variance introduced by outliers: 53% (severely inflated)

benchmarking main/med/bytestring fromChunks
time                 24.67 ms   (23.05 ms .. 26.69 ms)
                     0.981 R²   (0.961 R² .. 0.994 R²)
mean                 23.98 ms   (23.31 ms .. 24.75 ms)
std dev              1.563 ms   (1.232 ms .. 1.893 ms)
variance introduced by outliers: 25% (moderately inflated)

benchmarking main/med/bytestring concat
time                 22.40 ms   (21.95 ms .. 22.86 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 23.13 ms   (22.80 ms .. 24.16 ms)
std dev              1.202 ms   (496.7 μs .. 2.131 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking main/large/superbuffer (init=128 bytes)
time                 25.83 ms   (25.33 ms .. 26.30 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 26.00 ms   (25.73 ms .. 26.33 ms)
std dev              662.6 μs   (483.0 μs .. 977.9 μs)

benchmarking main/large/superbuffer (init=400000 bytes)
time                 21.47 ms   (20.73 ms .. 22.38 ms)
                     0.995 R²   (0.991 R² .. 0.998 R²)
mean                 23.46 ms   (22.95 ms .. 24.10 ms)
std dev              1.282 ms   (979.1 μs .. 1.725 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking main/large/superbuffer (init=800000 bytes)
time                 25.44 ms   (25.11 ms .. 25.82 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 25.43 ms   (25.13 ms .. 25.93 ms)
std dev              816.9 μs   (415.0 μs .. 1.352 ms)

benchmarking main/large/superbuffer (init=1600000 bytes)
time                 30.79 ms   (30.24 ms .. 31.43 ms)
                     0.999 R²   (0.997 R² .. 0.999 R²)
mean                 30.44 ms   (29.56 ms .. 30.92 ms)
std dev              1.449 ms   (716.6 μs .. 2.528 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking main/large/superbuffer (init=20000000 bytes)
time                 10.63 ms   (4.936 ms .. 14.51 ms)
                     0.543 R²   (0.201 R² .. 0.768 R²)
mean                 27.28 ms   (22.58 ms .. 32.47 ms)
std dev              10.98 ms   (9.673 ms .. 12.05 ms)
variance introduced by outliers: 95% (severely inflated)

benchmarking main/large/superbuffer (init=128 bytes, threadsafe, 2 concurrent writes)
time                 21.41 ms   (20.05 ms .. 22.66 ms)
                     0.991 R²   (0.986 R² .. 0.996 R²)
mean                 24.39 ms   (23.58 ms .. 25.36 ms)
std dev              2.007 ms   (1.499 ms .. 2.498 ms)
variance introduced by outliers: 34% (moderately inflated)

benchmarking main/large/superbuffer (init=400000 bytes, threadsafe, 2 concurrent writes)
time                 22.25 ms   (21.85 ms .. 22.80 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 22.25 ms   (22.01 ms .. 22.49 ms)
std dev              553.6 μs   (427.9 μs .. 702.3 μs)

benchmarking main/large/superbuffer (init=800000 bytes, threadsafe, 2 concurrent writes)
time                 22.56 ms   (22.14 ms .. 22.95 ms)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 22.95 ms   (22.64 ms .. 23.74 ms)
std dev              1.085 ms   (445.3 μs .. 1.948 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/large/superbuffer (init=1600000 bytes, threadsafe, 2 concurrent writes)
time                 22.05 ms   (21.75 ms .. 22.50 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 22.77 ms   (22.49 ms .. 23.59 ms)
std dev              1.020 ms   (475.6 μs .. 1.879 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/large/superbuffer (init=20000000 bytes, threadsafe, 2 concurrent writes)
time                 31.35 ms   (29.61 ms .. 33.21 ms)
                     0.986 R²   (0.971 R² .. 0.995 R²)
mean                 28.83 ms   (27.87 ms .. 29.78 ms)
std dev              2.072 ms   (1.698 ms .. 2.550 ms)
variance introduced by outliers: 28% (moderately inflated)

benchmarking main/large/buffer-builder (init=128 bytes, trim=yes)
time                 27.56 ms   (26.72 ms .. 28.08 ms)
                     0.996 R²   (0.991 R² .. 0.999 R²)
mean                 29.61 ms   (28.91 ms .. 30.31 ms)
std dev              1.490 ms   (1.282 ms .. 1.830 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking main/large/buffer-builder (init=400000 bytes, trim=yes)
time                 27.73 ms   (26.91 ms .. 28.30 ms)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 27.06 ms   (26.76 ms .. 27.36 ms)
std dev              675.1 μs   (539.6 μs .. 851.8 μs)

benchmarking main/large/buffer-builder (init=800000 bytes, trim=yes)
time                 27.74 ms   (27.14 ms .. 28.16 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 27.84 ms   (27.57 ms .. 28.24 ms)
std dev              716.2 μs   (406.4 μs .. 1.278 ms)

benchmarking main/large/buffer-builder (init=1600000 bytes, trim=yes)
time                 27.90 ms   (27.30 ms .. 28.26 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 27.30 ms   (27.01 ms .. 27.57 ms)
std dev              607.3 μs   (472.2 μs .. 805.3 μs)

benchmarking main/large/buffer-builder (init=20000000 bytes, trim=yes)
time                 40.14 ms   (38.86 ms .. 41.10 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 37.02 ms   (33.52 ms .. 38.41 ms)
std dev              4.424 ms   (1.138 ms .. 7.765 ms)
variance introduced by outliers: 45% (moderately inflated)

benchmarking main/large/bytestring builder
time                 23.00 ms   (21.84 ms .. 24.15 ms)
                     0.983 R²   (0.960 R² .. 0.996 R²)
mean                 24.99 ms   (24.21 ms .. 26.16 ms)
std dev              2.223 ms   (1.753 ms .. 3.120 ms)
variance introduced by outliers: 35% (moderately inflated)

benchmarking main/large/bytestring fromChunks
time                 23.39 ms   (22.78 ms .. 23.99 ms)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 23.98 ms   (23.58 ms .. 25.05 ms)
std dev              1.402 ms   (420.3 μs .. 2.618 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking main/large/bytestring concat
time                 29.05 ms   (25.14 ms .. 32.45 ms)
                     0.960 R²   (0.941 R² .. 0.990 R²)
mean                 24.90 ms   (23.74 ms .. 26.50 ms)
std dev              2.998 ms   (1.942 ms .. 3.934 ms)
variance introduced by outliers: 51% (severely inflated)
```
