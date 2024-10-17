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
  (* TODO *)
end
