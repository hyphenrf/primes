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


let run { store; size } =

  let factor = ref 3
  and idx    = ref 1
  and q      = isqrt size
  in
  while !factor <= q do

    while not (get store !factor) do
      factor := !factor + 2
    done;

    idx := !factor * !factor;

    while !idx < size do
      clr store !idx;
      idx := !idx + !factor lsl 1
    done;

    factor := !factor + 2
  done


(*---------------------Runner & Profiling-------------------------*)

let stamp = Unix.gettimeofday

let main =

  let size = meta.size in

  let passes = ref 0 in
  let duration = ref 0. in

  (* timing function call overhead is counted off *)
  let start_tm = stamp() in
  let overhead = stamp() in
  let start = 2. *. overhead -. start_tm in

  while !duration < 5. do
    create size |> run
      ; duration := stamp() -. start
      ; passes   := !passes + 1
  done;

  (* print the results *)
  Printf.printf "hyphenrf-%s;%d;%.5f;%d;algorithm=%s,faithful=%s,bits=%d\n"
    meta.name !passes !duration meta.threads
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

