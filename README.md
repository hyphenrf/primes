# primes
A submission for the [Software Drag Race](https://github.com/PlummersSoftwareLLC/Primes)

- `primes_faithful_st.ml` is around **8-10x** faster than [the current submission](https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race/PrimeOCaml/solution_1).  
  - `primes_faithful_im.ml` is the same implementation but with imperative constructs, showing that OCaml can optimize tailrec functions transparently.  
- `primes_faithful_mt.ml` is around **30-50%** faster than its single-threaded counterpart with `nproc=4`  
- `primes_faithful_mc.ml` uses the Multicore OCaml compiler, the perf differences between it and conventional threaded OCaml code are marginal*  
- `primes_faithful_fk.ml` uses `fork()` and `pipe()`, hitting upwards of **2x** speedup for `nproc=4` compared to single-threaded. Amazing what the classics can achieve.  
- `primes_faithful_1b.ml` uses a compact 1-bit repr. Right now it's quite slow but the plan is to batch-modify 8-bits at a time and see if this gives any nice speedup.  
- `primes_faithful_hb.ml` is built on the premise that keeping all data limited to minor-heap capacity could produce faster code. The speedup is balanced by the cost of extra indirection in practice.  
- `primes_faithful_b{b,s,m}.ml` is a direct translation of Rust submission's bitstripes implementation to see if it brings any good speedup. It does. Around **40%**.  



\* NOTE that, when I compare the single-threaded Trunk OCaml compiled binaries with the Multicore OCaml ones. The performance then appears marginal. However, when comparing single-threaded Multicore-compiled binaries with parallel Multicore-compiled ones, I do get that relative **2x** speedup as well. Unfortunately this exercise aims to hit the highest raw performance out of all OCaml compilers, not compare running times within the context of the Multicore compiler only.
