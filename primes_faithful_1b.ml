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

  let create size =
    { size; store = Bytes.make (size lsr 4) '\000' }


(*---------------------All performance here-----------------------*)

  (* all of these get optimized away *)
  let ($&) a b = Char.(code a land code b |> unsafe_chr)
  let ($^) a b = Char.(code a lxor code b |> unsafe_chr)
  let ($|) a b = Char.(code a lor code b |> unsafe_chr)
  let (~$) a   = Char.(code a |> lnot |> unsafe_chr)

  let table =
    "\000\001\000\002\000\004\000\008\
     \000\016\000\032\000\064\000\128"

  let get buf i =
    let chunk = Bytes.unsafe_get buf (i lsr 4) in
    table.[i land 15] $& chunk = '\000'

  let clr buf i =
    let chunk = Bytes.unsafe_get buf (i lsr 4) in
    Bytes.unsafe_set buf (i lsr 4) (chunk $| table.[i land 15])


  (* Hottest function *)
  let rec clr_all s i skip =
    let stop = s.size - (skip * 2 + skip) in
     if i < stop then
     begin clr s.store i
         ; clr s.store (i + skip)
         ; clr s.store (i + skip * 2) (* lea *)
         ; clr s.store (i + skip * 2 + skip) (* lea + lea *)
         ; clr_all s   (i + skip * 4) skip
      end else
         clr_rest s i skip

  and clr_rest s i skip =
   if i < s.size then
   begin clr s.store i
       ; clr_rest s (i + skip) skip
   end


  let isqrt i =
    float i |> sqrt |> int_of_float

  let run sieve =
    let q = isqrt sieve.size in

    let rec get_next factor =
     if get sieve.store factor then factor else
        get_next (factor + 2)
    in
    let rec run factor = if factor <= q then
      begin
        let factor = get_next factor in

        clr_all sieve (factor * factor) (factor * 2)
          ; run (factor + 2)
      end
    in
    run 3


(*---verification (assertions are removed on optimized builds)----*)

  let test =
    assert begin
      let count {store; size} =
        let count = ref 1 and i = ref 3 in
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
  { name = "bits"
  ; bits = 1
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


