(* Solution by @hyphenrf *)

type meta =
  { name : string
  ; bits : int
  ; size : int
  ; faithful : string
  ; algorithm : string
  ; mutable threads : int
  }

let meta =
  { name = "bytes-fork"
  ; bits = 8
  ; size = 1_000_000
  ; faithful = "yes"
  ; algorithm = "base"
  ; threads = 2
  }


(* Arg parsing *)
let set_threads i = meta.threads <- i

let spec = let open Arg in
  [ "-t", Int set_threads,
      "The number of threads to run the sieve with (prefer nproc)"
  ] |> align

let bad s = raise (Arg.Bad ("Not a recignized argument: "^s))
let usage = Sys.argv.(0)^" -t <num_threads>"

let _ = Arg.parse spec bad usage


(* implementation *)
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

  (* inits *)
  let size = meta.size
  and forks =
    let rec getmsb pos num =
     if num = 0 then pos else getmsb (pos+1) (num lsr 1)
    in getmsb (-1) meta.threads
  in

  let rec loop finish passes =
   if stamp() < finish then
   begin
      create size |> run
        ; loop finish (passes + 1)
   end else
      passes
  in

  (* thread work *)
  let open Unix in

  let intw fd = output_binary_int (out_channel_of_descr fd)
  and intr fd = input_binary_int (in_channel_of_descr fd) in

  let pid = Stack.create ()
  and fds = Stack.create () in

  let start = stamp() in

  for i = 1 to forks do
    Stack.push (pipe()) fds;
    Stack.push (fork()) pid;
  done;

  let passes = loop (start+.5.) 0 in
  let passes = ref passes in

  for i = 1 to forks do
    let fdr, fdw = Stack.pop fds in
    match Stack.pop pid with
    | 0 -> intw fdw !passes; exit 0
    | _ -> passes := !passes + intr fdr
  done;

  (* results *)
  let passes = !passes in
  let duration = stamp() -. start in

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
        count = expected || begin
        Printf.printf "size: %#d, count: %#d, expect: %#d\n"
          size count expected;
        false
    end
  end

