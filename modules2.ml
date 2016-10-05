(* Here I show a way to emulate OCaml functors with first-class modules*)

(* We make a Nat signature with two witness : *)
module type Nat = sig
  type a
  val z : a
  val s : a -> a
  val toInt : a -> int
end

module IntNat = struct
  type a = int
  let z = 0
  let s x = 1 + x
  let toInt x = x
end

module BinaryNat = struct
  type a = bool list
  let z = []
  let rec s x = match x with
    | false :: xs -> true :: xs
    | true  :: xs -> false :: s xs
    | []          -> [true]
  let rec toInt x = match x with
    | false :: xs -> 0 + 2 * toInt xs
    | true :: xs -> 1 + 2 * toInt xs
    | []         -> 0
end


(* Lets create Counters out of our Nats: *)
module type Counter = sig
  type a
  val v : a ref
  val incr : 'a -> unit
  val asInt : 'a -> int
end

module CounterFromNat (N : Nat) : Counter = struct
  type a = N.a
  let v = ref N.z
  let incr _ = v := N.s !v
  let asInt _ = N.toInt !v
end

module IntCounter = CounterFromNat(IntNat)
module BinaryCounter = CounterFromNat(BinaryNat)




(* First-class module method: *)
let counterFromNatFC (n : (module Nat)) =
  let module N = (val n) in
  let open N in
  let module M = struct
    type a = N.a
    let v = ref z
    let incr _ = v := s !v
    let asInt _ = toInt !v
  end in
  (module M : Counter)


let intCounterFC = counterFromNatFC (module IntNat : Nat)
module IntCounterFC = (val intCounterFC : Counter)


let to5 (c : (module Counter)) =
  let module C = (val c) in
  C.incr();
  C.incr();
  C.incr();
  C.incr();
  C.incr();
  C.asInt
  (* (module C : Counter) *)
