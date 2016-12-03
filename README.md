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

```
benchmarking main/small/superbuffer (init=128 bytes)
time                 20.22 ms   (19.77 ms .. 20.76 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 20.15 ms   (19.91 ms .. 20.40 ms)
std dev              575.0 μs   (436.3 μs .. 739.6 μs)

benchmarking main/small/superbuffer (init=4000 bytes)
time                 20.08 ms   (19.56 ms .. 20.79 ms)
                     0.996 R²   (0.993 R² .. 0.999 R²)
mean                 20.17 ms   (19.85 ms .. 20.66 ms)
std dev              905.1 μs   (625.2 μs .. 1.335 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/small/superbuffer (init=8000 bytes)
time                 23.72 ms   (23.21 ms .. 24.34 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 23.92 ms   (23.55 ms .. 24.52 ms)
std dev              1.001 ms   (673.0 μs .. 1.587 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking main/small/superbuffer (init=16000 bytes)
time                 24.15 ms   (23.79 ms .. 24.58 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 23.98 ms   (23.69 ms .. 24.55 ms)
std dev              863.6 μs   (487.1 μs .. 1.517 ms)

benchmarking main/small/superbuffer (init=20000000 bytes)
time                 11.48 ms   (6.634 ms .. 14.10 ms)
                     0.645 R²   (0.346 R² .. 0.854 R²)
mean                 23.80 ms   (20.34 ms .. 27.33 ms)
std dev              8.484 ms   (7.385 ms .. 9.868 ms)
variance introduced by outliers: 95% (severely inflated)

benchmarking main/small/superbuffer (init=128 bytes, threadsafe, 2 concurrent writes)
time                 23.15 ms   (22.24 ms .. 24.28 ms)
                     0.993 R²   (0.987 R² .. 0.997 R²)
mean                 24.41 ms   (23.96 ms .. 24.85 ms)
std dev              1.012 ms   (809.8 μs .. 1.434 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking main/small/superbuffer (init=4000 bytes, threadsafe, 2 concurrent writes)
time                 24.99 ms   (23.79 ms .. 26.02 ms)
                     0.994 R²   (0.990 R² .. 0.997 R²)
mean                 24.02 ms   (23.60 ms .. 24.48 ms)
std dev              1.004 ms   (831.7 μs .. 1.228 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking main/small/superbuffer (init=8000 bytes, threadsafe, 2 concurrent writes)
time                 25.30 ms   (24.78 ms .. 26.00 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 25.37 ms   (24.98 ms .. 26.10 ms)
std dev              1.148 ms   (621.8 μs .. 1.891 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking main/small/superbuffer (init=16000 bytes, threadsafe, 2 concurrent writes)
time                 25.45 ms   (24.73 ms .. 26.36 ms)
                     0.996 R²   (0.992 R² .. 0.998 R²)
mean                 24.24 ms   (23.80 ms .. 24.72 ms)
std dev              1.020 ms   (825.0 μs .. 1.335 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking main/small/superbuffer (init=20000000 bytes, threadsafe, 2 concurrent writes)
time                 31.89 ms   (30.17 ms .. 34.05 ms)
                     0.987 R²   (0.979 R² .. 0.994 R²)
mean                 30.33 ms   (29.35 ms .. 31.28 ms)
std dev              2.038 ms   (1.752 ms .. 2.381 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking main/small/buffer-builder (init=128 bytes, trim=yes)
time                 27.32 ms   (26.65 ms .. 28.22 ms)
                     0.996 R²   (0.991 R² .. 0.999 R²)
mean                 27.12 ms   (26.62 ms .. 27.58 ms)
std dev              1.045 ms   (758.8 μs .. 1.413 ms)
variance introduced by outliers: 10% (moderately inflated)

benchmarking main/small/buffer-builder (init=4000 bytes, trim=yes)
time                 27.72 ms   (26.43 ms .. 28.85 ms)
                     0.994 R²   (0.990 R² .. 0.998 R²)
mean                 26.85 ms   (26.33 ms .. 27.48 ms)
std dev              1.254 ms   (953.7 μs .. 1.868 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking main/small/buffer-builder (init=8000 bytes, trim=yes)
time                 26.93 ms   (26.03 ms .. 27.75 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 26.03 ms   (24.99 ms .. 26.46 ms)
std dev              1.420 ms   (578.2 μs .. 2.534 ms)
variance introduced by outliers: 21% (moderately inflated)

benchmarking main/small/buffer-builder (init=16000 bytes, trim=yes)
time                 27.18 ms   (26.63 ms .. 27.72 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 26.58 ms   (26.05 ms .. 26.91 ms)
std dev              907.8 μs   (570.8 μs .. 1.417 ms)
variance introduced by outliers: 10% (moderately inflated)

benchmarking main/small/buffer-builder (init=20000000 bytes, trim=yes)
time                 39.64 ms   (37.58 ms .. 41.77 ms)
                     0.993 R²   (0.988 R² .. 0.997 R²)
mean                 36.84 ms   (33.85 ms .. 38.83 ms)
std dev              4.888 ms   (1.424 ms .. 6.918 ms)
variance introduced by outliers: 52% (severely inflated)

benchmarking main/small/bytestring builder
time                 25.99 ms   (25.26 ms .. 26.86 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 26.82 ms   (26.10 ms .. 28.67 ms)
std dev              2.472 ms   (847.5 μs .. 4.346 ms)
variance introduced by outliers: 40% (moderately inflated)

benchmarking main/small/bytestring fromChunks
time                 24.71 ms   (23.97 ms .. 25.61 ms)
                     0.996 R²   (0.994 R² .. 0.999 R²)
mean                 25.10 ms   (24.78 ms .. 25.46 ms)
std dev              762.7 μs   (615.2 μs .. 972.1 μs)

benchmarking main/small/bytestring concat
time                 24.33 ms   (23.59 ms .. 25.07 ms)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 24.82 ms   (24.37 ms .. 26.35 ms)
std dev              1.561 ms   (507.3 μs .. 3.049 ms)
variance introduced by outliers: 25% (moderately inflated)

benchmarking main/med/superbuffer (init=128 bytes)
time                 20.24 ms   (19.67 ms .. 20.82 ms)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 21.88 ms   (21.34 ms .. 22.69 ms)
std dev              1.544 ms   (1.054 ms .. 2.186 ms)
variance introduced by outliers: 32% (moderately inflated)

benchmarking main/med/superbuffer (init=40000 bytes)
time                 20.93 ms   (20.47 ms .. 21.46 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 21.22 ms   (20.87 ms .. 21.75 ms)
std dev              985.4 μs   (696.1 μs .. 1.269 ms)
variance introduced by outliers: 18% (moderately inflated)

benchmarking main/med/superbuffer (init=80000 bytes)
time                 21.63 ms   (21.14 ms .. 22.09 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 21.44 ms   (21.26 ms .. 21.61 ms)
std dev              401.8 μs   (319.6 μs .. 550.9 μs)

benchmarking main/med/superbuffer (init=160000 bytes)
time                 21.92 ms   (21.58 ms .. 22.35 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 22.14 ms   (21.80 ms .. 23.03 ms)
std dev              1.207 ms   (476.2 μs .. 2.235 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking main/med/superbuffer (init=20000000 bytes)
time                 36.50 ms   (35.29 ms .. 37.98 ms)
                     0.995 R²   (0.989 R² .. 0.998 R²)
mean                 35.84 ms   (34.96 ms .. 36.70 ms)
std dev              1.785 ms   (1.412 ms .. 2.334 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking main/med/superbuffer (init=128 bytes, threadsafe, 2 concurrent writes)
time                 21.15 ms   (18.68 ms .. 23.97 ms)
                     0.950 R²   (0.908 R² .. 0.980 R²)
mean                 24.94 ms   (23.67 ms .. 26.24 ms)
std dev              2.822 ms   (2.391 ms .. 3.339 ms)
variance introduced by outliers: 51% (severely inflated)

benchmarking main/med/superbuffer (init=40000 bytes, threadsafe, 2 concurrent writes)
time                 21.79 ms   (21.24 ms .. 22.38 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 21.96 ms   (21.66 ms .. 22.56 ms)
std dev              946.4 μs   (527.5 μs .. 1.690 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/med/superbuffer (init=80000 bytes, threadsafe, 2 concurrent writes)
time                 20.33 ms   (19.97 ms .. 20.79 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 21.51 ms   (21.06 ms .. 22.95 ms)
std dev              1.825 ms   (481.4 μs .. 3.324 ms)
variance introduced by outliers: 37% (moderately inflated)

benchmarking main/med/superbuffer (init=160000 bytes, threadsafe, 2 concurrent writes)
time                 20.96 ms   (20.72 ms .. 21.25 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 20.67 ms   (20.48 ms .. 20.86 ms)
std dev              427.3 μs   (339.9 μs .. 590.7 μs)

benchmarking main/med/superbuffer (init=20000000 bytes, threadsafe, 2 concurrent writes)
time                 15.53 ms   (14.71 ms .. 16.32 ms)
                     0.990 R²   (0.984 R² .. 0.996 R²)
mean                 17.04 ms   (16.20 ms .. 19.15 ms)
std dev              3.153 ms   (803.4 μs .. 5.600 ms)
variance introduced by outliers: 78% (severely inflated)

benchmarking main/med/buffer-builder (init=128 bytes, trim=yes)
time                 27.03 ms   (26.42 ms .. 27.71 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 27.12 ms   (26.81 ms .. 27.50 ms)
std dev              764.3 μs   (640.0 μs .. 905.9 μs)

benchmarking main/med/buffer-builder (init=40000 bytes, trim=yes)
time                 24.45 ms   (23.87 ms .. 25.04 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 25.75 ms   (25.17 ms .. 27.17 ms)
std dev              1.860 ms   (737.2 μs .. 3.305 ms)
variance introduced by outliers: 30% (moderately inflated)

benchmarking main/med/buffer-builder (init=80000 bytes, trim=yes)
time                 25.41 ms   (24.69 ms .. 25.96 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 25.03 ms   (24.65 ms .. 25.52 ms)
std dev              953.1 μs   (712.1 μs .. 1.322 ms)

benchmarking main/med/buffer-builder (init=160000 bytes, trim=yes)
time                 20.74 ms   (19.45 ms .. 22.54 ms)
                     0.987 R²   (0.976 R² .. 0.998 R²)
mean                 21.97 ms   (21.18 ms .. 23.30 ms)
std dev              2.232 ms   (1.143 ms .. 3.201 ms)
variance introduced by outliers: 43% (moderately inflated)

benchmarking main/med/buffer-builder (init=20000000 bytes, trim=yes)
time                 42.12 ms   (40.02 ms .. 45.54 ms)
                     0.986 R²   (0.972 R² .. 0.996 R²)
mean                 34.61 ms   (30.88 ms .. 37.15 ms)
std dev              6.081 ms   (4.260 ms .. 7.559 ms)
variance introduced by outliers: 67% (severely inflated)

benchmarking main/med/bytestring builder
time                 21.46 ms   (21.04 ms .. 21.91 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 22.20 ms   (21.92 ms .. 22.56 ms)
std dev              769.1 μs   (561.8 μs .. 1.135 ms)

benchmarking main/med/bytestring fromChunks
time                 21.76 ms   (21.32 ms .. 22.18 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 21.70 ms   (21.44 ms .. 22.49 ms)
std dev              915.7 μs   (377.2 μs .. 1.650 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/med/bytestring concat
time                 21.71 ms   (20.96 ms .. 22.47 ms)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 21.85 ms   (21.52 ms .. 22.36 ms)
std dev              922.0 μs   (638.6 μs .. 1.303 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking main/large/superbuffer (init=128 bytes)
time                 20.19 ms   (19.47 ms .. 20.74 ms)
                     0.996 R²   (0.992 R² .. 0.998 R²)
mean                 22.49 ms   (21.65 ms .. 23.82 ms)
std dev              2.533 ms   (1.626 ms .. 3.812 ms)
variance introduced by outliers: 51% (severely inflated)

benchmarking main/large/superbuffer (init=400000 bytes)
time                 21.03 ms   (20.42 ms .. 21.69 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 21.17 ms   (20.81 ms .. 21.85 ms)
std dev              1.095 ms   (623.6 μs .. 1.826 ms)
variance introduced by outliers: 18% (moderately inflated)

benchmarking main/large/superbuffer (init=800000 bytes)
time                 28.01 ms   (27.01 ms .. 29.08 ms)
                     0.992 R²   (0.981 R² .. 0.998 R²)
mean                 27.51 ms   (26.32 ms .. 28.46 ms)
std dev              2.271 ms   (1.609 ms .. 3.314 ms)
variance introduced by outliers: 32% (moderately inflated)

benchmarking main/large/superbuffer (init=1600000 bytes)
time                 22.41 ms   (21.10 ms .. 23.55 ms)
                     0.991 R²   (0.985 R² .. 0.997 R²)
mean                 24.47 ms   (23.70 ms .. 25.88 ms)
std dev              2.373 ms   (1.559 ms .. 3.479 ms)
variance introduced by outliers: 43% (moderately inflated)

benchmarking main/large/superbuffer (init=20000000 bytes)
time                 16.26 ms   (15.84 ms .. 16.73 ms)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 16.36 ms   (16.15 ms .. 16.71 ms)
std dev              645.5 μs   (444.5 μs .. 1.039 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking main/large/superbuffer (init=128 bytes, threadsafe, 2 concurrent writes)
time                 21.83 ms   (21.42 ms .. 22.27 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 22.34 ms   (22.00 ms .. 23.16 ms)
std dev              1.137 ms   (507.0 μs .. 2.059 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking main/large/superbuffer (init=400000 bytes, threadsafe, 2 concurrent writes)
time                 20.75 ms   (20.19 ms .. 21.11 ms)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 22.37 ms   (21.79 ms .. 23.07 ms)
std dev              1.423 ms   (971.3 μs .. 1.764 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking main/large/superbuffer (init=800000 bytes, threadsafe, 2 concurrent writes)
time                 26.87 ms   (26.39 ms .. 27.40 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 27.69 ms   (27.16 ms .. 28.82 ms)
std dev              1.668 ms   (796.8 μs .. 3.007 ms)
variance introduced by outliers: 21% (moderately inflated)

benchmarking main/large/superbuffer (init=1600000 bytes, threadsafe, 2 concurrent writes)
time                 21.94 ms   (21.20 ms .. 22.83 ms)
                     0.993 R²   (0.986 R² .. 0.997 R²)
mean                 22.67 ms   (22.10 ms .. 23.47 ms)
std dev              1.557 ms   (927.2 μs .. 2.176 ms)
variance introduced by outliers: 29% (moderately inflated)

benchmarking main/large/superbuffer (init=20000000 bytes, threadsafe, 2 concurrent writes)
time                 15.28 ms   (14.51 ms .. 16.07 ms)
                     0.985 R²   (0.969 R² .. 0.994 R²)
mean                 18.44 ms   (17.30 ms .. 20.47 ms)
std dev              3.914 ms   (2.383 ms .. 5.301 ms)
variance introduced by outliers: 82% (severely inflated)

benchmarking main/large/buffer-builder (init=128 bytes, trim=yes)
time                 27.25 ms   (26.22 ms .. 28.09 ms)
                     0.995 R²   (0.991 R² .. 0.998 R²)
mean                 29.72 ms   (28.85 ms .. 30.97 ms)
std dev              2.197 ms   (1.692 ms .. 2.863 ms)
variance introduced by outliers: 27% (moderately inflated)

benchmarking main/large/buffer-builder (init=400000 bytes, trim=yes)
time                 24.62 ms   (23.86 ms .. 25.37 ms)
                     0.995 R²   (0.990 R² .. 0.998 R²)
mean                 24.56 ms   (24.11 ms .. 25.03 ms)
std dev              1.013 ms   (806.5 μs .. 1.291 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking main/large/buffer-builder (init=800000 bytes, trim=yes)
time                 24.85 ms   (24.37 ms .. 25.24 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 24.37 ms   (24.10 ms .. 24.59 ms)
std dev              550.3 μs   (407.5 μs .. 728.3 μs)

benchmarking main/large/buffer-builder (init=1600000 bytes, trim=yes)
time                 24.18 ms   (21.67 ms .. 25.98 ms)
                     0.978 R²   (0.963 R² .. 0.993 R²)
mean                 22.34 ms   (21.77 ms .. 23.09 ms)
std dev              1.560 ms   (1.157 ms .. 2.056 ms)
variance introduced by outliers: 29% (moderately inflated)

benchmarking main/large/buffer-builder (init=20000000 bytes, trim=yes)
time                 40.55 ms   (38.93 ms .. 42.77 ms)
                     0.985 R²   (0.965 R² .. 0.995 R²)
mean                 33.52 ms   (30.49 ms .. 36.43 ms)
std dev              5.750 ms   (4.777 ms .. 6.724 ms)
variance introduced by outliers: 67% (severely inflated)

benchmarking main/large/bytestring builder
time                 20.71 ms   (19.41 ms .. 21.67 ms)
                     0.987 R²   (0.975 R² .. 0.994 R²)
mean                 23.80 ms   (22.91 ms .. 24.66 ms)
std dev              2.027 ms   (1.762 ms .. 2.468 ms)
variance introduced by outliers: 38% (moderately inflated)

benchmarking main/large/bytestring fromChunks
time                 21.18 ms   (20.72 ms .. 21.62 ms)
                     0.998 R²   (0.995 R² .. 0.999 R²)
mean                 21.64 ms   (21.28 ms .. 22.18 ms)
std dev              1.000 ms   (589.7 μs .. 1.621 ms)
variance introduced by outliers: 18% (moderately inflated)

benchmarking main/large/bytestring concat
time                 21.59 ms   (20.97 ms .. 22.39 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 21.80 ms   (21.57 ms .. 22.11 ms)
std dev              600.9 μs   (451.9 μs .. 767.7 μs)
```
