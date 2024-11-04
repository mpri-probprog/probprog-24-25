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

  let sample d k prob =
    let v = Distribution.draw d in
    k v prob

  let factor s k prob = k () { prob with score = prob.score +. s }
  let observe d x = factor (Distribution.logpdf d x)
  let assume p = factor (if p then 0. else -.infinity)
  let exit v prob = { prob with value = Some v }

  let infer ?(n = 1000) model data =
    let rec gen n support =
      if n = 0 then support
      else
        let k = (model data) exit in
        let prob = k { score = 0.; k; value = None } in
        let v = Option.get prob.value in
        let s = prob.score in
        gen (n - 1) ((v, s) :: support)
    in

    let support = gen n [] in
    Distribution.categorical ~support
end

module Particle_filter = struct
  (* TODO
     - Start from importance sampling
     - Re-implement factor: stop and store continuation (checkpoint).
     - Update observe/assume
     - Implement infer: run particles until checkpoint, resample, again until completion
  *)

  include Importance_sampling

  let resample particles =
    let support =
      List.map (fun p -> ({ p with score = 0. }, p.score)) particles
    in
    let dist = Distribution.categorical ~support in
    List.map (fun _ -> Distribution.draw dist) particles

  let factor s k prob = { prob with score = prob.score +. s; k = k () }
  let observe d x = factor (Distribution.logpdf d x)
  let assume p = factor (if p then 0. else -.infinity)

  let infer ?(n = 1000) model data =
    let rec gen particles =
      if List.exists (fun p -> Option.is_none p.value) particles then
        let particles = List.map (fun p -> p.k p) particles in
        let particles = resample particles in
        gen particles
      else particles
    in

    let particles =
      List.init n (fun _ -> { value = None; score = 0.; k = (model data) exit })
    in
    let support =
      let particles = gen particles in
      List.map (fun p -> (Option.get p.value, p.score)) particles
    in
    Distribution.categorical ~support
end

module Enumeration = struct
  (* TODO *)

  type 'a prob = {
    current_score : float;
    traces : 'a particle list;
    support : ('a, float) Hashtbl.t;
  }

  and 'a particle = { k : 'a next; score : float }
  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let run_next prob =
    match prob.traces with
    | [] -> prob
    | { k; score } :: traces -> k { prob with current_score = score; traces }

  let exit v prob =
    let p = exp prob.current_score in
    (match Hashtbl.find_opt prob.support v with
    | None -> Hashtbl.add prob.support v p
    | Some p' -> Hashtbl.replace prob.support v (p +. p'));
    run_next prob

  let sample d k prob =
    let sup = Distribution.get_support d in
    let traces =
      List.fold_left
        (fun acc (v, l) ->
          { k = (fun prob -> k v prob); score = prob.current_score +. l } :: acc)
        [] sup
    in
    run_next { prob with traces = prob.traces @ traces }

  let factor s k prob =
    k () { prob with current_score = prob.current_score +. s }

  let assume p k prob = factor (if p then 0. else -.infinity) k prob
  let observe d x k prob = factor (Distribution.logpdf d x) k prob

  let infer m data =
    let prob =
      (m data) exit
        { current_score = 0.; traces = []; support = Hashtbl.create 11 }
    in
    let support =
      prob.support |> Hashtbl.to_seq |> List.of_seq
      |> List.map (fun (v, l) -> (v, log l))
    in
    Distribution.categorical ~support
end

module Metropolis_hastings = struct
  (* TODO *)
end
