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

(* pretty print a [dist] *)
let rec pp_dist fmt dist =
  match dist with
  | Beta (alpha, beta) ->
      Format.fprintf fmt "Beta(%a, %a)" pp_dist !alpha pp_dist !beta
  | Bernoulli p -> Format.fprintf fmt "Bernoulli(%a)" pp_dist !p
  | Gaussian (mu, sigma) ->
      Format.fprintf fmt "Gaussian(%a, %a)" pp_dist !mu pp_dist !sigma
  | V v -> Format.fprintf fmt "%f" v

(* Symbolic computations only *)
module Symbolic = struct
  exception Not_tractable

  (* return the value of a constant node
     raise [Not_tractable] if the node is not constant. *)
  let value node = match !node with V v -> v | _ -> raise Not_tractable

  (* return a new node with distribution [dist] *)
  let sample (d : dist) : node = ref d

  (* update distribution [dist] given an observation
     raise [Not_tractable] if this is not possible. *)
  let observe (d : dist) (x : float) : unit =
    match d with
    | Bernoulli p -> (
        match !p with
        | Beta (a, b) ->
            (* case Beta/Bernoulli *)
            a := V (value a +. (1. -. x));
            b := V (value b +. x)
        | _ -> raise Not_tractable)
    | Gaussian (m, s) -> (
        match !m with
        | Gaussian (m0, s0) ->
            (* case Gaussian/Gaussian *)
            let s2 =
              1. /. ((1. /. (value s0 ** 2.)) +. (1. /. (value s ** 2.)))
            in
            m0 :=
              V
                (s2
                *. ((value m0 /. (value s0 ** 2.)) +. (x /. (value s ** 2.))));
            s0 := V (sqrt s2)
        | _ -> raise Not_tractable)
    | _ -> raise Not_tractable

  (* return the posterior distribution of a model
     we assume that a model always returns a value of type [node]. *)
  let infer (model : 'a -> node) (obs : 'a) : dist = !(model obs)
end

(* here symbolic computation is not possible, raise [Not_tractable] *)
(* let coin_weird () =
     let z = sample (Gaussian (const 0.5, const 0.1)) in
     List.iter (observe (Bernoulli z)) [ 0.; 1.; 0.; 0.; 1.; 0.; 0.; 0. ];
     z
   in
   let dist = infer coin_weird () in
   Format.printf "Dist coin weird : %a @." pp_dist dist *)

module Semi_symbolic = struct
  (* import classic distributions with draw and logpdf *)
  open Byoppl

  let bernoulli_float ~p =
    Distribution.categorical ~support:[ (1., log p); (0., log (1. -. p)) ]

  let gaussian = Distribution.gaussian
  let beta = Distribution.beta
  let dirac = Distribution.dirac

  (* classic prob type for importance sampling *)
  type prob = { mutable score : float }

  (* importance sampling, direct style, with explicit prob argument *)
  let infer ?(n = 10) model obs =
    let gen _ =
      let prob = { score = 0.0 } in
      let v = !(model prob obs) in
      (* !! Only difference, de-reference the result *)
      let w = prob.score in
      (v, w)
    in
    let support = List.init n gen in
    Distribution.categorical ~support

  (* draw a concrete value from the [dist] of a [node] and fix the node value *)
  let rec value node : float =
    let v =
      (* draw a concrete value *)
      match !node with
      | Beta (a, b) -> Distribution.draw (beta ~a:(value a) ~b:(value b))
      | Bernoulli p -> Distribution.draw (bernoulli_float ~p:(value p))
      | Gaussian (mu, sigma) ->
          Distribution.draw (gaussian ~mu:(value mu) ~sigma:(value sigma))
      | V v -> v
    in
    node := V v;
    (* turn the node to a constant *)
    v (* return the value *)

  (* return a new node with distribution [dist] *)
  let sample (_prob : 'p) (d : dist) : node = ref d

  (* update distribution [dist] given an observation
     update the score of the current particle. *)
  let observe prob (d : dist) (x : float) : unit =
    let marginal =
      (* convert [dist] to a classic distribution *)
      match d with
      | Bernoulli p -> (
          match !p with
          | Beta (a, b) ->
              (* case Beta/Bernoulli *)
              let marg = bernoulli_float ~p:(value a /. (value a +. value b)) in
              a := V (value a +. (1. -. x));
              b := V (value b +. x);
              marg
          | _ -> bernoulli_float ~p:(value p))
      | Gaussian (m, s) -> (
          match !m with
          | Gaussian (m0, s0) ->
              (* case Gaussian/Gaussian *)
              let m_0 = value m0 in
              (* save initial values that will be udpated *)
              let s_0 = value s0 in
              let s2 =
                1. /. ((1. /. (value s0 ** 2.)) +. (1. /. (value s ** 2.)))
              in
              let marg =
                Distribution.gaussian ~mu:m_0
                  ~sigma:(sqrt ((value s ** 2.) +. (s_0 ** 2.)))
              in
              m0 :=
                V
                  (s2
                  *. ((value m0 /. (value s0 ** 2.)) +. (x /. (value s ** 2.)))
                  );
              s0 := V (sqrt s2);
              marg
          | _ -> Distribution.gaussian ~mu:(value m) ~sigma:(value s))
      | Beta (a, b) -> Distribution.beta ~a:(value a) ~b:(value b)
      | V v -> dirac ~v
    in
    (* update the score of particle prob.id *)
    prob.score <- prob.score +. Distribution.logpdf marginal x
end
