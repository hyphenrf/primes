(* Solution by @hyphenrf *)

type meta =
  { name : string
  ; bits : int
  ; size : int
  ; threads : int
  ; faithful : string
  ; algorithm : string
  }


module Sieve :
sig
  type t
  val create : int -> t
  val run    : t -> unit
end
=
struct
  type t = { size : int; store : bytes }

  let alloc n v =
    let buf = Bytes.create n in
    Bytes.unsafe_fill buf 0 n v; buf

  let create size =
    { size; store = alloc (size lsr 1) '\x01' }
    (* We only need to know about odds, not evens. So if we say our unit size is
       4-bits, we can get a byte that's both an even number and an odd number,
       and operate exclusively on the odd part. *)


(*---------------------All performance here-----------------------*)

  let get buf i =
    Bytes.unsafe_get buf (i lsr 1) = '\x01'

  let clr buf i =
    Bytes.unsafe_set buf (i lsr 1) '\x00'
    (* The reason having the shift here is faster than inside the hot loop where
       "it may be called less if it's factored out" is that a shift already
       takes place. Remember that OCaml integers are tagged, and that bytes are
       represented as ocaml integers. The compiler recognizes the shift and just
       shifts by 2 instead of by 1, which makes ocaml right-shifts --At least in
       this position-- SUPER cheap and equivalent to just using the number as an
       argument. This is why the shift is present in get and set like that. *)


  (* Hottest function *)
  let clr_all s i skip =
    let stop = s.size - (skip lsl 1 + skip) in
    let rec clr_all s i skip stop =
       if i < stop then (* we do a little unrolling *)
       begin clr s.store i
           ; clr s.store (i + skip)
           ; clr s.store (i + skip * 2) (* lea *)
           ; clr s.store (i + skip * 2 + skip) (* lea + add *)
           ; clr_all s   (i + skip * 4 ) skip stop
        end else
           clr_rest s i skip

    and clr_rest s i skip =
     if i < s.size then
     begin clr s.store i
         ; clr_rest s (i + skip) skip
     end

    in
    clr_all s i skip stop


  let isqrt i =
    float i |> sqrt |> int_of_float

  let run sieve =
    let q = isqrt sieve.size in
    let rec run factor sieve q =
      let factor = next factor sieve.store in
       if factor <= q then
          begin clr_all sieve (factor * factor) (factor * 2)
              ; run (factor + 2) sieve q
          end
    and next factor store =
     if get store factor then factor else
        next (factor + 2) store
    in
    run 3 sieve q


(*---verification (assertions are removed on optimized builds)----*)

  let test =
    assert begin
      let count {store; size} =
        let count = ref 1
        and i     = ref 3
        in
        while !i < size do
          if get store !i then incr count
             ; i := !i + 2
        done
          ; !count
      in
      let historical =
        [ 10, 4
        ; 100, 25
        ; 1000, 168
        ; 10_000, 1229
        ; 100_000, 9592
        ; 1000_000, 78498
        ; 10_000_000, 664579
        ; 100_000_000, 5761455
        ]
      in
      let size = 10_000 in
      let expected = List.assoc size historical in
      let sieve = create size in run sieve;
      let count = count sieve in
        count = expected || begin
          Printf.printf "size: %#d, count: %#d, expect: %#d\n"
            size count expected;
          false
        end
    end
end



(*---------------------Runner & Profiling-------------------------*)

let meta =
  { name = "nibbles"
  ; bits = 4
  ; size = 1_000_000
  ; threads = 1
  ; faithful = "yes"
  ; algorithm = "base"
  }

let stamp = Sys.time

let main =

  let rec loop finish passes = if stamp() < finish then
   begin
     Sieve.create meta.size |> Sieve.run
       ; loop finish (passes + 1)
   end else
      passes
  in

  let start = stamp() in
  let passes = loop (start+.5.) 0 in
  let duration = stamp() -. start in

  (* print the results *)
  Printf.printf "hyphenrf-%s;%d;%.5f;%d;algorithm=%s,faithful=%s,bits=%d\n"
    meta.name passes duration meta.threads
    meta.algorithm meta.faithful meta.bits


