(* Solution by @hyphenrf *)

type meta =
  { name : string
  ; bits : int
  ; size : int
  ; threads : int
  ; faithful : string
  ; algorithm : string
  }

let meta =
  { name = "bytes"
  ; bits = 8
  ; size = 1_000_000
  ; threads = 1
  ; faithful = "yes"
  ; algorithm = "base"
  }

type t = { size : int; store : bytes }


let create size =
  { size; store = Bytes.create size } (* inverted i.e. 0 = true *)


(*---------------------All performance here-----------------------*)

(* faithfullness requirements don't specify that I must _use_ the whole buffer
   I allocated, it just requires me to allocate n elements where n = sieve_size.
 * I can use only half that for better locality and still count as faithful, by
   modifying my getters and setters to access in halves. This gives around 15%
   boost for size 1 million. *)
let get buf i =
  Bytes.unsafe_get buf (i lsr 1) = '\000'

let clr buf i =
  Bytes.unsafe_set buf (i lsr 1) '\001'

let isqrt i =
  float i |> sqrt |> int_of_float

(* Hottest function *)
let rec clr_all s i skip =
  let stop = s.size - (skip lsl 1 + skip) in
   if i < stop then
   begin clr s.store i
       ; clr s.store (i + skip)
       ; clr s.store (i + skip lsl 1)
       ; clr s.store (i + skip lsl 1 + skip)
       ; clr_all s   (i + skip lsl 2) skip
    end else
       clr_slow s i skip

and clr_slow s i skip =
 if i < s.size then
 begin clr s.store i
     ; clr_slow s (i + skip) skip
 end

let run sieve =
  let q = isqrt sieve.size in

  let rec get_next factor =
   if get sieve.store factor then factor else
      get_next (factor + 2)
  in
  let rec run factor =
   if factor <= q then
      let factor = get_next factor in

      clr_all sieve (factor * factor) (factor + factor)
        ; run (factor + 2)
  in

  run 3



(*---------------------Runner & Profiling-------------------------*)

let stamp = Sys.time

let main =

  let size = meta.size in

  let rec loop finish passes =
   if stamp() < finish then
   begin create size |> run
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


(*---verification (assertions are removed on optimized builds)----*)

let tests =
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

  assert begin
    let size = 10_000 in
    let expected = List.assoc size historical in
    let sieve = create size in run sieve;
    let count = count sieve in
     if count = expected then true else begin
        Printf.printf "size: %#d, count: %#d, expect: %#d\n"
          size count expected;
        false
     end
  end

