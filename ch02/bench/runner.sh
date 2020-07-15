#!/usr/bin/env sh

CH=02

export BENCHMARKS_RUNNER=TRUE
export BENCH_LIB=./ch${CH}/lib

exec dune exec -- ./ch${CH}/bench/bench.exe -fork -run-without-cross-library-inlining "$@" -quota
