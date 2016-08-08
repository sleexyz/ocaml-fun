module type Nat =
sig
  type a
  val z : a
  val s : a -> a
  val toInt : a -> int
end

module IntNat =
struct
  type a = int
  let z = 0
  let s x = 1 + x
  let toInt x = x
end


module UNat =
struct
  type a = unit list
  let z = []
  let s x = () :: x
  let rec toInt x =
    match x with
    | x :: xs -> 1 + toInt xs
    | []      -> 0
end

module UNat2 =
struct
  type a = unit list
  let z = []
  let s x = () :: x
  let toInt l = List.fold_left(fun x y -> x + 1) 0 l
end


let nats : (module Nat) list
  = [(module IntNat : Nat); (module UNat : Nat);(module UNat2 : Nat)]
