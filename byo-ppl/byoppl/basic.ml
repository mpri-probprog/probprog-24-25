module Rejection_sampling_hard = struct
  exception Reject

  type prob = Prob

  let sample _prob d = Distribution.draw d
  let assume _prob p = if not p then raise Reject
  let observe prob d x = assume prob (Distribution.draw d = x)

  let infer ?(n = 1000) model obs =
    let rec gen i = try model Prob obs with Reject -> gen i in
    let samples = List.init n gen in
    Distribution.empirical ~samples
end

module Importance_sampling = struct
  type prob = { mutable score : float }

  let sample _prob d = Distribution.draw d
  let factor prob w = prob.score <- prob.score +. w
  let assume prob p = if not p then factor prob (-.infinity)
  let observe prob d x = factor prob (Distribution.logpdf d x)

  let infer ?(n = 1000) model obs =
    let gen _ =
      let prob = { score = 0.0 } in
      let v = model prob obs in
      let w = prob.score in
      (v, w)
    in
    let support = List.init n gen in
    Distribution.categorical ~support
end

module Rejection_sampling = struct
  (* TODO:
     - What is prob here?
     - Implement sample / factor / observe assume : as usual?
     - Implement the generation function `gen` to generate one sample
  *)

  include Importance_sampling

  let infer ?(n = 1000) ?(max_score = 0.) model data =
    let rec gen i =
      let prob = { score = 0.0 } in
      let v = model prob data in
      let u = Random.float 1.0 in
      let alpha = exp (min 0. (prob.score -. max_score)) in
      if u <= alpha then v else gen i
    in

    let samples = List.init n gen in
    Distribution.empirical ~samples
end

module Simple_metropolis = struct
  (* TODO:
     - What is prob here?
     - Implement sample / factor / observe assume : as usual?
     - Complete the implementation of single site Metropolis Hastings.
     - Warning: next value depends on the previous one
     - Careful with the initialization step
  *)

  include Importance_sampling

  let infer ?(n = 1000) model data =
    let rec gen n samples old_score old_value =
      if n = 0 then samples
      else
        let prob = { score = 0.0 } in
        let v = model prob data in
        let u = Random.float 1.0 in
        let alpha = exp (min 0. (prob.score -. old_score)) in
        if u <= alpha then gen (n - 1) (v :: samples) prob.score v
        else gen (n - 1) (old_value :: samples) old_score old_value
    in

    let prob = { score = 0.0 } in
    let first_value = model prob data in
    let samples = gen n [] prob.score first_value in
    Distribution.empirical ~samples
end

module Metropolis_hastings = struct
  module NameSet = Set.Make (String)

  type 'a sample_site = { x_value : 'a; x_score : float }

  type 'a prob = {
    mutable score : float;
    x_store : (string, 'a sample_site) Hashtbl.t;
    cache : (string, 'a sample_site) Hashtbl.t;
  }

  let sample prob d name =
    let x_value =
      match Hashtbl.find_opt prob.cache name with
      | Some { x_value; _ } -> x_value
      | None -> Distribution.draw d
    in
    let x_score = Distribution.logpdf d x_value in
    Hashtbl.add prob.x_store name { x_value; x_score };
    x_value

  let factor prob s = prob.score <- prob.score +. s
  let observe prob d x = factor prob (Distribution.logpdf d x)
  let assume prob p = factor prob (if p then 0. else -.infinity)

  let mh cache old_score old_x_store score x_store =
    let l_alpha =
      log (Float.of_int (Hashtbl.length old_x_store))
      -. log (Float.of_int (Hashtbl.length x_store))
    in
    let l_alpha = l_alpha +. score -. old_score in
    let dom =
      NameSet.inter
        (NameSet.of_seq (Hashtbl.to_seq_keys cache))
        (NameSet.of_seq (Hashtbl.to_seq_keys x_store))
    in
    let l_alpha =
      NameSet.fold
        (fun x l_alpha ->
          let { x_score; _ } = Hashtbl.find x_store x in
          let { x_score = old_x_score; _ } = Hashtbl.find old_x_store x in
          l_alpha +. x_score -. old_x_score)
        dom l_alpha
    in
    exp (min 0. l_alpha)

  let infer ?(n = 1000) ?(warmup = 0) model data =
    let rec gen n samples old_score old_value old_x_store =
      if n = 0 then samples
      else
        let dom = old_x_store |> Hashtbl.to_seq_keys |> List.of_seq in
        let regen = List.nth dom (Random.int (List.length dom)) in
        let cache = Hashtbl.copy old_x_store in
        Hashtbl.remove cache regen;
        let prob = { score = 0.; x_store = Hashtbl.create 11; cache } in
        let new_value = model prob data in
        let alpha = mh cache old_score old_x_store prob.score prob.x_store in
        let u = Random.float 1.0 in
        if not (u < alpha) then
          gen (n - 1) (old_value :: samples) old_score old_value old_x_store
        else
          gen (n - 1) (new_value :: samples) prob.score new_value prob.x_store
    in

    let prob =
      { score = 0.; x_store = Hashtbl.create 11; cache = Hashtbl.create 1 }
    in
    let first_value = model prob data in
    let samples =
      gen (warmup + n) [] prob.score first_value prob.x_store
      |> Distribution.drop warmup
    in
    Distribution.empirical ~samples
end
