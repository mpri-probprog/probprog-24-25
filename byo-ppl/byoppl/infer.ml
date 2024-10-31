module Gen = struct
  type 'a prob = 'a option
  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let sample d k prob =
    let v = Distribution.draw d in
    k v prob

  let factor _s k prob = k () prob
  let observe d x = factor (Distribution.logpdf d x)
  let assume p = factor (if p then 0. else -.infinity)
  let exit v _prob = Some v

  let draw model data =
    let v = (model data) exit None in
    Option.get v
end

module Importance_sampling = struct
  (* TODO:
     - Implement sample: draw and continue
     - Implement factor: continue with updated score
     - Implement infer: run n particles to completion, build distribution
  *)

  type 'a prob = { score : float; k : 'a next; value : 'a option }
  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let sample _d _k _prob = assert false
  let factor _s _k _prob = assert false
  let infer ?(_n = 1000) _m _data = assert false
end

module Particle_filter = struct
  (* TODO
     - Start from importance sampling
     - Re-implement factor: stop and store continuation (checkpoint).
     - Update observe/assume
     - Implement infer: run particles until checkpoint, resample, again until completion
  *)

  include Importance_sampling

  let resample _particles = assert false
  let factor _s _k _prob = assert false
  let infer ?(_n = 1000) _m _data = assert false
end

module Enumeration = struct
  (* TODO *)

  type 'a prob = TODO
  and 'a particle = { k : 'a next; score : float }
  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let sample _d _k _prob = assert false
  let factor _s _k _prob = assert false
  let infer ?(_n = 1000) _m _data = assert false
end

module Metropolis_hastings = struct
  (* TODO *)
end
