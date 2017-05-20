(* Typed tagless final interpreters: TTFI*)
(* http://okmij.org/ftp/tagless-final/course/optimizations.html *)

module type Symantics = sig
  type 'a repr
  val wrapInt: int -> int repr
  val add: int repr -> int repr -> int repr
end

module Program1 (I: Symantics) = struct
  open I
  let res = add (add (wrapInt 0) (wrapInt 2)) (wrapInt 3)
end

module Run = struct
  type 'a repr = 'a
  let wrapInt x = x
  let add = (+)
end

module Show = struct
  type 'a repr = string
  let wrapInt = string_of_int
  let add x y = "(" ^ x ^ " + " ^ y ^ ")"
end

module IdentityT(F: Symantics): Symantics with type 'a repr = 'a F.repr = F

module InvertT(F: Symantics): Symantics with type 'a repr = 'a F.repr = struct
  type 'a repr = 'a F.repr
  let wrapInt x = F.wrapInt (-x)
  let add x y = F.add x y
end

let () =
  print_endline "Show:";
  let module Compiled = Program1(Show) in
  print_endline Compiled.res;

  print_endline "Run:";
  let module Compiled = Program1(Run) in
  print_endline (string_of_int Compiled.res);

  print_endline "InvertT(Run):";
  let module Compiled = Program1(InvertT(Run)) in
  print_endline (string_of_int Compiled.res);

  print_endline "InvertT(Show):";
  let module Compiled = Program1(InvertT(Show)) in
  print_endline Compiled.res;

  print_endline "InvertT(IdentityT(Show)):";
  let module Compiled = Program1(InvertT(IdentityT(Show))) in
  print_endline Compiled.res;

(* Naive optimization *)
type 'a partial = { dynamic: 'a; known_zero: bool }
module SuppressZeroPlusTNaive(F: Symantics): Symantics with type 'a repr = ('a F.repr) partial = struct
  type 'a repr = ('a F.repr) partial
  let wrapInt x = { dynamic = F.wrapInt x; known_zero = (x = 0) }
  let add x y = match (x, y) with
    | ({ known_zero = true }, x) -> x
    | (x, { known_zero = true }) -> x
    | ({ dynamic = x }, { dynamic = y }) -> { dynamic = F.add x y; known_zero = false }
end

let () =
  print_endline "SuppressZeroPlusTNaive(Show):";
  let module Compiled = Program1(SuppressZeroPlusTNaive(Show)) in
  print_endline Compiled.res.dynamic;


(* We need some sort of weak inverse *)
module type DualTrans = sig
  type 'a from
  type 'a term
  val fwd: 'a from -> 'a term (* Embed in term space *)
  val bwd: 'a term -> 'a from (* Project back down *)
end

(* We need to extend Symantics to our optimization engine *)
module type Symantics' = sig
  include Symantics
  type 'a obs
  val observe: 'a repr -> 'a obs
end

module LiftSymantics'(F: Symantics): Symantics' with type 'a obs = 'a F.repr = struct
  include F
  type 'a obs = 'a F.repr
  let observe x = x
end

module BaseOptimizer(X: DualTrans) (F: Symantics' with type 'a repr = 'a X.from): (
  Symantics' with
  type 'a repr = 'a X.term and
  type 'a obs = 'a F.obs
) = struct
  open X
  type 'a repr = 'a term
  type 'a obs = 'a F.obs
  let wrapInt x = fwd (F.wrapInt x)
  let add x y = fwd (F.add (bwd x) (bwd y))
  let observe x = F.observe (bwd x)
end

module NoopPass(F: Symantics') = struct
  module X = struct
    type 'a from = 'a F.repr
    type 'a term = 'a F.repr
    let fwd x = x
    let bwd x = x
  end
  include BaseOptimizer(X)(F)
end

module UnitalPass(F: Symantics') = struct
  module X = struct
    type 'a from = 'a F.repr
    type 'a term =
      | Unknown: 'a from -> 'a term
      | Zero: int term
    let fwd x = Unknown x
    let bwd: type a. a term -> a from = function
      | Unknown x -> x
      | Zero -> F.wrapInt 0
  end
  include BaseOptimizer(X)(F)
  open X
  let wrapInt x = if x = 0 then Zero else Unknown(F.wrapInt x)
  let add x y = match (x, y) with
    | (Zero, x) -> x
    | (x, Zero) -> x
    | (Unknown x, Unknown y) -> Unknown (F.add x y)
end

module ConstantFoldPass(F: Symantics') = struct
  module X = struct
    type 'a from = 'a F.repr
    type 'a term =
      | Unknown: 'a from -> 'a term
      | Num: int -> int term
    let fwd x = Unknown x
    let bwd: type a. a term -> a from = function
      | Unknown x -> x
      | Num x -> F.wrapInt x
  end
  include BaseOptimizer(X)(F)
  open X
  let wrapInt x = Num x
  let add x y = match (x, y) with
    | (Num a, Num b) -> Num (a + b)
    | (x, y) -> Unknown(F.add (bwd x) (bwd y))
end

let () =
  print_endline "NoopPass(LiftSymantics'(Show)):";
  let module Compiler = NoopPass(LiftSymantics'(Show)) in
  let module Compiled = Program1(Compiler) in
  print_endline(Compiler.observe(Compiled.res));

  print_endline "UnitalPass(NoopPass(LiftSymantics'(Show))):";
  let module Compiler = UnitalPass(NoopPass(LiftSymantics'(Show))) in
  let module Compiled = Program1(Compiler) in
  print_endline(Compiler.observe(Compiled.res));

  print_endline "ConstantFoldPass(UnitalPass(NoopPass(LiftSymantics'(Show)))):";
  let module Compiler = ConstantFoldPass(UnitalPass(NoopPass(LiftSymantics'(Show)))) in
  let module Compiled = Program1(Compiler) in
  print_endline(Compiler.observe(Compiled.res));
