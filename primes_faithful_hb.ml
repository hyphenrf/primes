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
  type blocks = { a : bytes array }
    [@@unboxed] (* just makes typing easier *)

  type t = { size : int; store : blocks }

  (* Minor heap allocates fast via bump-allocation. To use it, our blocks
     mustn't exceed 256 words in size (Max_young_wosize).
   * For an array, that's 256 elts, for bytes, that's 2Ki elts. combine that
     with halfbytes implementation and you comfortably fit a million elts. *)

  (* size / 2 (evens) / 256 (max minor heap array size) + 1 (access) *)
  let chunk size  = size lsr 9 + 1
  let alloc bytes = Bytes.create bytes
  let empty = Bytes.create 0
  (* important: max = 2040 for minor heap perf, 1 word is reserved for len *)

  let create size =
    { size; store = {a = Array.make 256 empty} }


(*---------------------All performance here-----------------------*)

  let get {a} i =
    let buf = a.(i lsr 1 land 0xff) in
    buf == empty ||
    Bytes.unsafe_get buf (i lsr 9) <> '\x0f'

  let clr { size; store={a} } i =
    let maj = i lsr 1 land 0xff in
    let buf = a.(maj) in
    let buf = if buf != empty then buf else
      let buf = alloc (chunk size) in
      a.(maj) <- buf; buf
    in
    Bytes.unsafe_set buf (i lsr 9) '\x0f'
  [@@inline]

  (* Hottest function *)
  let rec clr_all s i skip =
    let stop = s.size - (skip lsl 2) in
     if i < stop then
     begin clr s (i)
         ; clr s (i + skip)
         ; clr s (i + skip * 2) (* lea *)
         ; clr s (i + skip * 2 + skip) (* lea + lea *)
         ; clr s (i + skip * 4)
         ; clr_all s   (i + skip * 4 + skip) skip
      end else
         clr_rest s i skip

  and clr_rest s i skip =
   if i < s.size then
   begin clr s i
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
      let size = 100 in
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
  { name = "nibbles-minor"
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


