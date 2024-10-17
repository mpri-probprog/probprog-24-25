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

  let infer ?(n = 1000) ?(_max_score = 0.) _model _data =
    let gen _i = assert false (* TODO *) in
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

  let infer ?(n = 1000) _model _data =
    let gen _n _samples _old_score _old_value = (* TODO *) assert false in

    (* TODO: initialization *)
    let samples = (* TODO *) gen n [] 0. None in
    Distribution.empirical ~samples
end

module Metropolis_hastings = struct
  (* TODO *)
end
