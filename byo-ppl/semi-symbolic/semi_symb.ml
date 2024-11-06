(* simple distribution type *)
type dist =
  | Beta of node * node
  | Bernoulli of node
  | Gaussian of node * node
  | V of float

(* nodes are ref that can be updated at runtime *)
and node = dist ref

(* build a constant node *)
let const (v : float) : node = ref (V v)

(* Symbolic computations only *)
module Symbolic = struct
  exception Not_tractable

  (* return the value of a constant node
     raise [Not_tractable] if the node is not constant. *)
  let value node = assert false

  (* return a new node with distribution [dist] *)
  let sample (d : dist) : node = assert false

  (* update distribution [dist] given an observation
     raise [Not_tractable] if this is not possible. *)
  let observe (d : dist) (x : float) : unit =
    match d with
    | Bernoulli p -> (
        match !p with
        | Beta (a, b) ->
            (* Symbolic computation is possible *)
            (* TODO: update the nodes *)
            assert false
        | _ -> raise Not_tractable)
    | Gaussian (m, s) -> assert false (* TODO *)
    | _ -> raise Not_tractable

  (* return the posterior distribution of a model
     we assume that a model always returns a value of type [node]. *)
  let infer (model : 'a -> node) (obs : 'a) : dist = assert false
end

module Semi_symbolic = struct
  (* import classic distributions with draw and logpdf *)
  open Byoppl

  let bernoulli_float ~p =
    Distribution.categorical ~support:[ (1., log p); (0., log (1. -. p)) ]

  let gaussian = Distribution.gaussian
  let beta = Distribution.beta
  let dirac = Distribution.dirac

  (* classic prob type for importance sampling *)
  type prob = Todo

  (* importance sampling, direct style, with explicit prob argument *)
  let infer ?(n = 10) model obs = assert false

  (* draw a concrete value from the [dist] of a [node] and fix the node value *)
  let value node : float =
    let v = assert false in
    node := V v;
    (* turn the node to a constant *)
    v (* return the value *)

  (* return a new node with distribution [dist] *)
  let sample (_prob : 'p) (d : dist) : node = assert false

  (* update distribution [dist] given an observation
     update the score of the current particle. *)
  let observe prob (d : dist) (x : float) : unit =
    let marginal =
      (* convert [dist] to a classic distribution and update the nodes *)
      match d with
      | Bernoulli p -> assert false (* TODO: maybe check p *)
      | Gaussian (m, s) -> assert false (* TODO: maybe check m and s *)
      | Beta (a, b) -> beta ~a:(value a) ~b:(value b)
      | V v -> dirac ~v
    in
    (* update the score of particle prob.id *)
    assert false
end
