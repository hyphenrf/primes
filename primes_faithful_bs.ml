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
  let _BLK_SHIFT = 12
  let _BIT_SHIFT = _BLK_SHIFT + 3
  let _BLK_SIZE  = 1 lsl _BLK_SHIFT
  let _BIT_SIZE  = 1 lsl _BIT_SHIFT
  let _BLK_MASK  = _BLK_SIZE - 1
  let _BIT_MASK  = _BIT_SIZE - 1

  type t =
    { size : int; store_size : int
    ; store : bytes array
    }

  let min a b: int = if a <= b then a else b (* monomorphize *)

  let empty = Bytes.create 0

  let create size =
    let store_size = size lsr 1 + 1 in
    { size
    ; store_size
    ; store =
          let size = store_size in
          let blocks = size lsr _BIT_SHIFT + min 1 (size land _BIT_MASK) in
          let store = Array.make blocks empty in
          for i = 0 to blocks - 1 do
            let buf = Bytes.create _BLK_SIZE in
            Bytes.unsafe_fill buf 0 _BLK_SIZE '\x00'
              ; store.(i) <- buf
          done
            ; store
    }


(*---------------------All performance here-----------------------*)

  (* all of these get optimized away *)
  let ($&) a b = Char.(code a land b |> unsafe_chr)
  let ($|) a b = Char.(code a lor  b |> unsafe_chr)

  let (.![]<-) buf b mask =
    Bytes.unsafe_set buf b (Bytes.unsafe_get buf b $| mask)

  let rec fill_unrolled mask block stop skip start =
   if start < stop then
   begin block.![start] <- mask
       ; block.![start + skip] <- mask
       ; block.![start + skip * 2] <- mask
       ; block.![start + skip * 2 + skip] <- mask
       ; fill_unrolled mask block stop skip (start + skip * 4)
   end
   else start

  let fill_u mask block skip start =
      fill_unrolled mask block (_BLK_SIZE - (skip * 2 + skip)) skip start

  let rec fill_r mask block skip start =
   if start < _BLK_SIZE then
   begin block.![start] <- mask
       ; fill_r mask block skip (start + skip)
   end
   else start

  let rec fill_chunk ({store; store_size} as sieve) skip block chunk bit =
   if bit < 8 then
      let current_block = store.(block) in
      let mask = 1 lsl bit in

      let chunk = fill_u mask current_block skip chunk in
      let chunk = fill_r mask current_block skip chunk in

      fill_chunk sieve skip block (chunk - _BLK_SIZE) (bit + 1)
   else chunk

  let rec fill_block ({store} as sieve) skip block chunk bit =
   if block < Array.length store then
      let chunk = fill_chunk sieve skip block chunk bit in
      fill_block sieve skip (block + 1) chunk 0

  let clr_all blocks index skip =
    let block  = index  lsr  _BIT_SHIFT in
    let offset = index  land _BIT_MASK  in
    let bit    = offset lsr  _BLK_SHIFT in
    let chunk  = offset land _BLK_MASK  in
        fill_block blocks skip block chunk bit

  let (.?()) {store} index =
    let block  = index  lsr  _BIT_SHIFT in
    let offset = index  land _BIT_MASK  in
    let bit    = offset lsr  _BLK_SHIFT in
    let chunk  = offset land _BLK_MASK  in
    let chunk  = Bytes.unsafe_get store.(block) chunk in
        chunk $& (1 lsl bit) = '\000'

  let isqrt i =
    float i |> sqrt |> int_of_float

  let rec next factor store =
   if store.?(factor) then factor * 2 + 1 else
      next (factor + 1) store

  let run sieve =
    let rec run factor store q =
      let factor = next (factor lsr 1) store
      in
      if factor <= q then
         begin clr_all store ((factor * factor) lsr 1) factor
             ; run (factor + 2) store q
         end
    in
    run 3 sieve (isqrt sieve.size)


(*---verification (assertions are removed on optimized builds)----*)

  let test =
    assert begin
      let range ?(start=0) ?(step=1) stop =
        let rec range i j k acc =
         if i >= k then acc else
            range (i+j) j k (i::acc)
         in range start step stop []
      in
      let prime store number =
        number mod 2 = 1 && begin
           let index = number / 2 in
           store.?(index)
        end
      in
      let count sieve =
        range ~start:1 sieve.size (* TODO: range 3 2 size *)
        |> List.filter (prime sieve)
        |> List.length
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
      let size = 1_000_000 in
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
  { name = "bitstripes-small"
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


