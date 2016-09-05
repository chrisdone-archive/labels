#!/bin/bash

# Install all dependencies (for library, tests and benchmarks) with
# profiling enabled.
cabal install \
    --only-dependencies \
    --enable-benchmarks \
    --enable-tests \
    --enable-library-profiling

# Configure and build.
cabal configure \
    --enable-benchmarks \
    --enable-tests \
    --enable-library-coverage \
    --enable-library-profiling
cabal build

cabal test
#cabal bench --benchmark-option="-obench.html"
#cabal haddock
