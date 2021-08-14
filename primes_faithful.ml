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
  ; threads = 1 (* TODO *)
  ; faithful = "yes"
  ; algorithm = "base"
  }

type t = { size : int; store : bytes }


let create size =
  { size; store = Bytes.create size } (* inverted i.e. 0 = true *)

let count {store; size} =
  Bytes.to_seq store
    |> Seq.map (fun c -> int_of_char c lxor 1)
    |> Seq.fold_left (+) 0
    |> fun n -> n - size / 2 (* all even bits are untouched *)


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

let rec clr_all sieve i skip =
 if i < sieve.size then
 begin
    clr sieve.store i
      ; clr_all sieve (i + skip) skip
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

  let rec loop start duration passes =
   if duration < 5. then
   begin
      create size |> run
        ; loop start (stamp() -. start) (passes + 1)
   end else
      duration, passes
  in

  let duration, passes = loop (stamp()) 0. 0 in

  (* print the results *)
  Printf.printf "hyphenrf-%s;%d;%.5f;%d;algorithm=%s,faithful=%s,bits=%d\n"
    meta.name passes duration meta.threads
    meta.algorithm meta.faithful meta.bits


(*---verification (assertions are removed on optimized builds)----*)

let tests =
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

