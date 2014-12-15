(* intにエンコード *)
let rec i_of_c c = c ((+) 1) 0

let zero = fun f x -> x
let one = fun f x -> f x
let two = fun f x -> f (f x)

(* successor: cにfとxを適用したものの外に
fをくっつける *)
let succ c = fun f x -> f (c f x)
let three = succ two

(* add: c1のx部分にc2を流し込む *)
let add c1 c2 = fun f x -> c1 f (c2 f x)
(*
# i_of_c (add two three);;
- : int = 5
 *)

(* mul: c1のfにc2を流し込む *)
let mul c1 c2 = fun f x -> c1 (c2 f) x
let _ = i_of_c (mul two three)

(* まずboolをつくる *)
let _true = fun x y -> x
let _false = fun x y -> y
let _if cond t f = cond t f

(* iszero: cに1つでもfがあると_falseに変える *)
let iszero c = c (fun z -> _false) _true
(*
# let _ = _if (iszero zero) true false;;
- : bool = true
# let _ = _if (iszero one) true false;;
- : bool = false
*)
