# Haskell SuperBuffer

[![CircleCI](https://circleci.com/gh/agrafix/superbuffer.svg?style=svg)](https://circleci.com/gh/agrafix/superbuffer)
[![Hackage](https://img.shields.io/hackage/v/superbuffer.svg)](http://hackage.haskell.org/package/superbuffer)

The `superbuffer` packages was designed to efficiently build up bytestrings from `IO` actions producing
smaller chunks. The goal was to reduce memory overhead as much as possible while still being as fast as possible.
In our use case, it reduced total memory usage of the program from `350 MB` (`bytestring` builder) to `50 MB` (`superbuffer`).
For speed see benchmarks below. Note that the speed heavily depends on a good choice of the initial buffer size. `superbuffer` outperforms or performs similar to the `bytestring` alternatives consistently. `superbuffer` outperforms `buffer-builder`.

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

Ran on `Intel(R) Core(TM) i7-4790K CPU @ 4.00GHz` server.

```
benchmarking main/small/superbuffer (init=128 bytes)
time                 9.643 ms   (9.581 ms .. 9.702 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 9.709 ms   (9.645 ms .. 9.819 ms)
std dev              249.9 μs   (133.1 μs .. 419.7 μs)

benchmarking main/small/superbuffer (init=4000 bytes)
time                 9.680 ms   (9.602 ms .. 9.757 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 9.675 ms   (9.628 ms .. 9.741 ms)
std dev              151.7 μs   (99.26 μs .. 250.7 μs)

benchmarking main/small/superbuffer (init=8000 bytes)
time                 9.867 ms   (9.792 ms .. 9.934 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 9.960 ms   (9.879 ms .. 10.10 ms)
std dev              279.0 μs   (158.4 μs .. 466.5 μs)

benchmarking main/small/superbuffer (init=16000 bytes)
time                 9.905 ms   (9.838 ms .. 9.980 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 9.919 ms   (9.867 ms .. 9.980 ms)
std dev              157.5 μs   (122.5 μs .. 222.7 μs)

benchmarking main/small/superbuffer (init=20000000 bytes)
time                 11.88 ms   (11.83 ms .. 11.94 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.80 ms   (11.76 ms .. 11.85 ms)
std dev              112.4 μs   (87.06 μs .. 170.0 μs)

benchmarking main/small/superbuffer (init=128 bytes, threadsafe, 2 concurrent writes)
time                 12.62 ms   (12.59 ms .. 12.65 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.64 ms   (12.63 ms .. 12.68 ms)
std dev              51.46 μs   (26.13 μs .. 85.22 μs)

benchmarking main/small/superbuffer (init=4000 bytes, threadsafe, 2 concurrent writes)
time                 9.710 ms   (9.627 ms .. 9.818 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 9.719 ms   (9.682 ms .. 9.767 ms)
std dev              118.1 μs   (95.01 μs .. 143.5 μs)

benchmarking main/small/superbuffer (init=8000 bytes, threadsafe, 2 concurrent writes)
time                 9.742 ms   (9.666 ms .. 9.813 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 9.712 ms   (9.683 ms .. 9.754 ms)
std dev              100.2 μs   (74.21 μs .. 151.2 μs)

benchmarking main/small/superbuffer (init=16000 bytes, threadsafe, 2 concurrent writes)
time                 9.727 ms   (9.658 ms .. 9.808 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 9.729 ms   (9.700 ms .. 9.761 ms)
std dev              87.81 μs   (73.29 μs .. 111.1 μs)

benchmarking main/small/superbuffer (init=20000000 bytes, threadsafe, 2 concurrent writes)
time                 13.20 ms   (13.17 ms .. 13.22 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 13.24 ms   (13.23 ms .. 13.31 ms)
std dev              68.78 μs   (15.53 μs .. 145.9 μs)

benchmarking main/small/buffer-builder (init=128 bytes, trim=yes)
time                 11.31 ms   (11.23 ms .. 11.37 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 11.38 ms   (11.33 ms .. 11.49 ms)
std dev              185.8 μs   (76.25 μs .. 331.6 μs)

benchmarking main/small/buffer-builder (init=4000 bytes, trim=yes)
time                 11.22 ms   (11.11 ms .. 11.28 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 11.34 ms   (11.28 ms .. 11.55 ms)
std dev              277.2 μs   (55.05 μs .. 557.5 μs)

benchmarking main/small/buffer-builder (init=8000 bytes, trim=yes)
time                 11.45 ms   (11.37 ms .. 11.54 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.43 ms   (11.40 ms .. 11.48 ms)
std dev              108.3 μs   (76.33 μs .. 164.0 μs)

benchmarking main/small/buffer-builder (init=16000 bytes, trim=yes)
time                 11.29 ms   (11.25 ms .. 11.34 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.31 ms   (11.28 ms .. 11.34 ms)
std dev              81.19 μs   (55.21 μs .. 114.9 μs)

benchmarking main/small/buffer-builder (init=20000000 bytes, trim=yes)
time                 11.46 ms   (11.43 ms .. 11.49 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.54 ms   (11.51 ms .. 11.60 ms)
std dev              99.22 μs   (35.35 μs .. 193.1 μs)

benchmarking main/small/bytestring builder
time                 19.25 ms   (19.18 ms .. 19.37 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 19.29 ms   (19.24 ms .. 19.32 ms)
std dev              90.68 μs   (69.32 μs .. 129.7 μs)

benchmarking main/small/bytestring fromChunks
time                 17.41 ms   (17.37 ms .. 17.45 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 17.45 ms   (17.43 ms .. 17.51 ms)
std dev              72.59 μs   (25.83 μs .. 141.6 μs)

benchmarking main/small/bytestring concat
time                 17.30 ms   (17.26 ms .. 17.34 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 17.34 ms   (17.32 ms .. 17.39 ms)
std dev              88.10 μs   (49.00 μs .. 140.9 μs)

benchmarking main/med/superbuffer (init=128 bytes)
time                 11.08 ms   (10.99 ms .. 11.17 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 11.03 ms   (10.89 ms .. 11.09 ms)
std dev              239.1 μs   (118.6 μs .. 440.6 μs)

benchmarking main/med/superbuffer (init=40000 bytes)
time                 11.15 ms   (11.06 ms .. 11.24 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 11.19 ms   (11.13 ms .. 11.27 ms)
std dev              181.1 μs   (135.1 μs .. 263.1 μs)

benchmarking main/med/superbuffer (init=80000 bytes)
time                 10.88 ms   (10.81 ms .. 10.97 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 10.71 ms   (10.50 ms .. 10.79 ms)
std dev              314.9 μs   (110.2 μs .. 651.7 μs)

benchmarking main/med/superbuffer (init=160000 bytes)
time                 10.78 ms   (10.76 ms .. 10.79 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.81 ms   (10.79 ms .. 10.84 ms)
std dev              56.39 μs   (17.14 μs .. 95.98 μs)

benchmarking main/med/superbuffer (init=20000000 bytes)
time                 11.28 ms   (11.26 ms .. 11.30 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.35 ms   (11.33 ms .. 11.40 ms)
std dev              92.07 μs   (53.07 μs .. 133.1 μs)

benchmarking main/med/superbuffer (init=128 bytes, threadsafe, 2 concurrent writes)
time                 11.52 ms   (11.49 ms .. 11.54 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.56 ms   (11.54 ms .. 11.61 ms)
std dev              86.18 μs   (26.25 μs .. 146.7 μs)

benchmarking main/med/superbuffer (init=40000 bytes, threadsafe, 2 concurrent writes)
time                 11.52 ms   (11.50 ms .. 11.54 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.57 ms   (11.55 ms .. 11.63 ms)
std dev              77.49 μs   (8.529 μs .. 158.4 μs)

benchmarking main/med/superbuffer (init=80000 bytes, threadsafe, 2 concurrent writes)
time                 10.90 ms   (10.88 ms .. 10.92 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.93 ms   (10.91 ms .. 10.96 ms)
std dev              52.31 μs   (10.02 μs .. 106.1 μs)

benchmarking main/med/superbuffer (init=160000 bytes, threadsafe, 2 concurrent writes)
time                 10.89 ms   (10.87 ms .. 10.91 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.91 ms   (10.90 ms .. 10.96 ms)
std dev              50.63 μs   (8.093 μs .. 101.2 μs)

benchmarking main/med/superbuffer (init=20000000 bytes, threadsafe, 2 concurrent writes)
time                 11.49 ms   (11.46 ms .. 11.52 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.55 ms   (11.53 ms .. 11.62 ms)
std dev              88.88 μs   (28.32 μs .. 172.5 μs)

benchmarking main/med/buffer-builder (init=128 bytes, trim=yes)
time                 11.06 ms   (11.02 ms .. 11.10 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.05 ms   (11.02 ms .. 11.14 ms)
std dev              142.0 μs   (58.52 μs .. 269.7 μs)

benchmarking main/med/buffer-builder (init=40000 bytes, trim=yes)
time                 10.11 ms   (10.10 ms .. 10.13 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.16 ms   (10.15 ms .. 10.18 ms)
std dev              49.34 μs   (31.67 μs .. 75.14 μs)

benchmarking main/med/buffer-builder (init=80000 bytes, trim=yes)
time                 10.04 ms   (10.01 ms .. 10.06 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.12 ms   (10.09 ms .. 10.17 ms)
std dev              101.0 μs   (61.65 μs .. 172.0 μs)

benchmarking main/med/buffer-builder (init=160000 bytes, trim=yes)
time                 10.14 ms   (10.09 ms .. 10.20 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.15 ms   (10.13 ms .. 10.21 ms)
std dev              75.85 μs   (31.27 μs .. 140.3 μs)

benchmarking main/med/buffer-builder (init=20000000 bytes, trim=yes)
time                 11.17 ms   (11.12 ms .. 11.23 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.20 ms   (11.17 ms .. 11.25 ms)
std dev              89.87 μs   (44.86 μs .. 155.1 μs)

benchmarking main/med/bytestring builder
time                 14.55 ms   (14.45 ms .. 14.66 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 14.73 ms   (14.66 ms .. 14.80 ms)
std dev              170.5 μs   (126.9 μs .. 218.8 μs)

benchmarking main/med/bytestring fromChunks
time                 14.83 ms   (14.65 ms .. 14.98 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 14.80 ms   (14.73 ms .. 14.86 ms)
std dev              158.7 μs   (140.6 μs .. 187.2 μs)

benchmarking main/med/bytestring concat
time                 14.76 ms   (14.57 ms .. 14.91 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 14.70 ms   (14.65 ms .. 14.76 ms)
std dev              142.9 μs   (112.9 μs .. 166.6 μs)

benchmarking main/large/superbuffer (init=128 bytes)
time                 11.05 ms   (11.03 ms .. 11.08 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.09 ms   (11.07 ms .. 11.12 ms)
std dev              48.73 μs   (27.42 μs .. 91.18 μs)

benchmarking main/large/superbuffer (init=400000 bytes)
time                 11.24 ms   (11.19 ms .. 11.30 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.27 ms   (11.24 ms .. 11.33 ms)
std dev              105.7 μs   (59.46 μs .. 181.7 μs)

benchmarking main/large/superbuffer (init=800000 bytes)
time                 11.73 ms   (11.70 ms .. 11.78 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 11.76 ms   (11.71 ms .. 11.82 ms)
std dev              139.7 μs   (84.27 μs .. 203.9 μs)

benchmarking main/large/superbuffer (init=1600000 bytes)
time                 11.70 ms   (11.60 ms .. 11.78 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.76 ms   (11.72 ms .. 11.81 ms)
std dev              103.9 μs   (60.29 μs .. 179.1 μs)

benchmarking main/large/superbuffer (init=20000000 bytes)
time                 11.63 ms   (11.60 ms .. 11.66 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.69 ms   (11.67 ms .. 11.77 ms)
std dev              113.4 μs   (23.56 μs .. 235.6 μs)

benchmarking main/large/superbuffer (init=128 bytes, threadsafe, 2 concurrent writes)
time                 11.48 ms   (11.45 ms .. 11.51 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.49 ms   (11.48 ms .. 11.52 ms)
std dev              43.75 μs   (21.82 μs .. 76.32 μs)

benchmarking main/large/superbuffer (init=400000 bytes, threadsafe, 2 concurrent writes)
time                 11.47 ms   (11.44 ms .. 11.51 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.48 ms   (11.47 ms .. 11.51 ms)
std dev              52.00 μs   (23.74 μs .. 94.73 μs)

benchmarking main/large/superbuffer (init=800000 bytes, threadsafe, 2 concurrent writes)
time                 11.93 ms   (11.91 ms .. 11.96 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.97 ms   (11.95 ms .. 12.00 ms)
std dev              65.95 μs   (46.25 μs .. 89.64 μs)

benchmarking main/large/superbuffer (init=1600000 bytes, threadsafe, 2 concurrent writes)
time                 11.91 ms   (11.87 ms .. 11.95 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.96 ms   (11.93 ms .. 12.02 ms)
std dev              105.6 μs   (41.84 μs .. 191.2 μs)

benchmarking main/large/superbuffer (init=20000000 bytes, threadsafe, 2 concurrent writes)
time                 11.98 ms   (11.93 ms .. 12.03 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.04 ms   (12.01 ms .. 12.10 ms)
std dev              110.7 μs   (85.82 μs .. 146.7 μs)

benchmarking main/large/buffer-builder (init=128 bytes, trim=yes)
time                 11.30 ms   (11.25 ms .. 11.37 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.28 ms   (11.25 ms .. 11.30 ms)
std dev              66.67 μs   (55.87 μs .. 84.78 μs)

benchmarking main/large/buffer-builder (init=400000 bytes, trim=yes)
time                 10.72 ms   (10.68 ms .. 10.75 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.73 ms   (10.71 ms .. 10.79 ms)
std dev              73.87 μs   (20.65 μs .. 154.6 μs)

benchmarking main/large/buffer-builder (init=800000 bytes, trim=yes)
time                 10.68 ms   (10.64 ms .. 10.72 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.73 ms   (10.71 ms .. 10.80 ms)
std dev              77.80 μs   (24.84 μs .. 162.7 μs)

benchmarking main/large/buffer-builder (init=1600000 bytes, trim=yes)
time                 10.49 ms   (10.43 ms .. 10.54 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.53 ms   (10.50 ms .. 10.65 ms)
std dev              135.6 μs   (19.69 μs .. 294.4 μs)

benchmarking main/large/buffer-builder (init=20000000 bytes, trim=yes)
time                 11.50 ms   (11.47 ms .. 11.53 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.60 ms   (11.56 ms .. 11.67 ms)
std dev              114.9 μs   (57.42 μs .. 213.9 μs)

benchmarking main/large/bytestring builder
time                 15.07 ms   (15.03 ms .. 15.10 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 15.18 ms   (15.14 ms .. 15.25 ms)
std dev              131.1 μs   (90.12 μs .. 168.4 μs)

benchmarking main/large/bytestring fromChunks
time                 15.27 ms   (15.24 ms .. 15.32 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 15.13 ms   (15.06 ms .. 15.18 ms)
std dev              145.5 μs   (93.42 μs .. 190.6 μs)

benchmarking main/large/bytestring concat
time                 15.06 ms   (15.00 ms .. 15.10 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 15.20 ms   (15.15 ms .. 15.26 ms)
std dev              142.6 μs   (110.6 μs .. 211.1 μs)
```
