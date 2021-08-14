# primes
A submission for the [Software Drag Race](https://github.com/PlummersSoftwareLLC/Primes)

primes_faithful.ml is around 10x faster than [the current submission](https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimeOCaml/solution_1).  
primes_faithful_mt.ml is around 30-50% faster than its single-threaded counterpart with nproc=4  
primes_faithful_mc.ml uses the multicore-ocaml compiler, the perf differences between it and conventional threaded OCaml code are marginal. TODO: speedup should scale almost linearly with n cores..
