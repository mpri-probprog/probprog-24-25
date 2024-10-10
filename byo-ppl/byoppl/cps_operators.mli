(** Syntactic sugar to write CPS models (see {{: https://discuss.ocaml.org/t/what-is-the-use-of-continuation-passing-style-cps/4491} this post}) *)

(** {1 Monadic syntax} *)

val return : 'a -> ('a -> 'b) -> 'b
val ( let* ) : (('a -> 'b) -> 'c) -> ('a -> 'd -> 'b) -> 'd -> 'c

(** Monadic operator for CPS models. For instance, the following codes are equivalent.

{[
let funny_bernoulli_cps () k =
    sample (bernoulli ~p:0.5) (fun a ->
        sample (bernoulli ~p:0.5) (fun b ->
            sample (bernoulli ~p:0.5) (fun c ->
                assume (a = 1 || b = 1)
                  (fun () -> k (a + b + c)))))
]}

{[
let funny_bernoulli () =
  let* a = sample (bernoulli ~p:0.5) in
  let* b = sample (bernoulli ~p:0.5) in
  let* c = sample (bernoulli ~p:0.5) in
  let* () = assume (a = 1 || b = 1) in
  return (a + b + c)
]}

Note that this simplified syntax may prevent tail recursion... *)

(** {1 List iterators} *)

module Cps_list : sig
  (** CPS iterators on lists. *)

  val iter : ('a -> (unit -> 'b) -> 'b) -> 'a list -> (unit -> 'b) -> 'b
  val iteri : (int -> 'a -> (unit -> 'b) -> 'b) -> 'a list -> (unit -> 'b) -> 'b
  val map : ('a -> ('b -> 'c) -> 'c) -> 'a list -> ('b list -> 'c) -> 'c

  val fold :
    ('a -> 'b -> ('b -> 'c) -> 'c) -> 'a list -> init:'b -> ('b -> 'c) -> 'c

  (** For instance, [iter] can be used to apply multiple observations.
  
  {[
  let beta_bernoulli_cps data =
    let* z = sample (beta ~a:1. ~b:1.) in
    let* () = iter (observe (bernoulli ~p:z)) data in
    return z
  ]} *)
end

(** {1 Array iterators} *)

module Cps_array : sig
  (** CPS iterators on arrays. *)

  val iter : ('a -> (unit -> 'b) -> 'b) -> 'a array -> (unit -> 'b) -> 'b

  val iteri :
    (int -> 'a -> (unit -> 'b) -> 'b) -> 'a array -> (unit -> 'b) -> 'b

  val map : ('a -> ('b -> 'c) -> 'c) -> 'a array -> ('b array -> 'c) -> 'c

  val fold :
    ('a -> 'b -> ('b -> 'c) -> 'c) -> 'a array -> init:'b -> ('b -> 'c) -> 'c
end
