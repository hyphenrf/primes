# primes
A submission for the [Software Drag Race](https://github.com/PlummersSoftwareLLC/Primes)

- `primes_faithful_st_.ml` is around **8-10x** faster than [the current submission](https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimeOCaml/solution_1).  
- `primes_faithful_mt.ml` is around **30-50%** faster than its single-threaded counterpart with `nproc=4`  
- `primes_faithful_mc.ml` uses the Multicore OCaml compiler, the perf differences between it and conventional threaded OCaml code are marginal*  
- `primes_faithful_fk.ml` uses `fork()` and `pipe()`, hitting upwards of **2x** speedup for `nproc=4` compared to single-threaded. Amazing what the classics can achieve.


\* NOTE that, when I compare the single-threaded Trunk OCaml compiled binaries with the Multicore OCaml ones. The performance then appears marginal. However, when comparing single-threaded Multicore-compiled binaries with parallel Multicore-compiled ones, I do get that relative **2x** speedup as well. Unfortunately this exercise aims to hit the highest raw performance out of all OCaml compilers, not compare running times within the context of the Multicore compiler only.
