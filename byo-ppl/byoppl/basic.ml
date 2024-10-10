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
  (* TODO *)
end

module Simple_metropolis = struct
  (* TODO *)
end

module Metropolis_hastings = struct
  (* TODO *)
end
