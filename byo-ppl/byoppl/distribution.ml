type _ t =
  | Dirac : 'a -> 'a t
  | Uniform : (float * float) -> float t
  | Bernoulli : float -> bool t
  | Binomial : (float * int) -> int t
  | Beta : (float * float) -> float t
  | Gaussian : (float * float) -> float t
  | Categorical : ('a * float) list -> 'a t
  | Empirical : 'a list -> 'a t
  | Poisson : float -> int t

let dirac ~v = Dirac v
let uniform ~a ~b = Uniform (a, b)
let bernoulli ~p = Bernoulli p
let binomial ~p ~n = Binomial (p, n)
let beta ~a ~b = Beta (a, b)
let gaussian ~mu ~sigma = Gaussian (mu, sigma)
let categorical ~support = Categorical support
let empirical ~samples = Empirical samples
let poisson ~lambda = Poisson lambda
let pi = 4. *. atan 1.
let two_pi = 2.0 *. pi
let sqrt_two_pi = sqrt two_pi

let log_sum_exp logits =
  let max_score =
    List.fold_right (fun l acc -> max l acc) logits neg_infinity
  in
  let sum_exp_scores =
    List.fold_right (fun l acc -> exp (l -. max_score) +. acc) logits 0.0
  in
  max_score +. log sum_exp_scores

let normalize support =
  let lse = support |> List.map snd |> log_sum_exp in
  List.map (fun (v, l) -> (v, exp (l -. lse))) support

let rec draw : type a. a t -> a = function
  | Dirac v -> v
  | Uniform (a, b) -> a +. Random.float (b -. a)
  | Bernoulli p -> if draw (Uniform (0., 1.)) < p then true else false
  | Binomial (p, n) ->
      let rec run_trials n n_success =
        if n = 0 then n_success
        else if draw (Uniform (0., 1.)) < p then
          run_trials (n - 1) (n_success + 1)
        else run_trials (n - 1) n_success
      in
      run_trials n 0
  | Beta (a, b) ->
      (* Draw gamma samples with Marsaglia and Tsang's algorithm [2000] *)
      let gamma_draw a =
        let d = a -. (1. /. 3.) in
        let c = 1. /. sqrt (9. *. d) in
        let rec loop () =
          let x = draw (Gaussian (0., 1.)) in
          let u = draw (Uniform (0., 1.)) in
          let v = (1. +. (c *. x)) ** 3. in
          if
            v > 0. && log u < (0.5 *. (x ** 2.)) +. d -. (d *. v) +. (d *. log v)
          then d *. v
          else loop ()
        in
        loop ()
      in
      (* Return x / (x + y) where x and y are gamma samples *)
      let x = gamma_draw a in
      let y = gamma_draw b in
      x /. (x +. y)
  | Gaussian (mu, sigma) ->
      (* Box and Muller's algorithm [1958] *)
      let u1 = draw (Uniform (0., 1.)) in
      let u2 = draw (Uniform (0., 1.)) in
      let z = sqrt (-2. *. log u1) *. cos (two_pi *. u2) in
      (z *. sigma) +. mu
  | Categorical support ->
      let u = draw (Uniform (0., 1.)) in
      let rec search cs d =
        match d with
        | [] -> raise (Invalid_argument "Empty distribution")
        | [ (v, _) ] -> v
        | (v, p) :: d -> if u < cs +. p then v else search (cs +. p) d
      in
      search 0. (normalize support)
  | Empirical samples ->
      let i = Random.int (List.length samples) in
      List.nth samples i
  | Poisson _ -> assert false

let log_combination n k =
  let rec comb acc n k =
    if k = 0 then acc
    else
      comb (acc +. log (float_of_int n) -. log (float_of_int k)) (n - 1) (k - 1)
  in
  comb 0. n k

(* Lanczos approximation of the Gamma function
   from https://rosettacode.org/wiki/Gamma_function#OCaml *)
let gamma_f =
  let g = 7. in
  let c =
    [|
      0.99999999999980993;
      676.5203681218851;
      -1259.1392167224028;
      771.32342877765313;
      -176.61502916214059;
      12.507343278686905;
      -0.13857109526572012;
      9.9843695780195716e-6;
      1.5056327351493116e-7;
    |]
  in
  let rec ag z d =
    if d = 0 then c.(0) +. ag z 1
    else if d < 8 then (c.(d) /. (z +. float d)) +. ag z (succ d)
    else c.(d) /. (z +. float d)
  in
  fun z ->
    let z = z -. 1. in
    let p = z +. g +. 0.5 in
    sqrt_two_pi *. (p ** (z +. 0.5)) *. exp (-.p) *. ag z 0

let log_gamma x = log (gamma_f x)

let logpdf : type a. a t -> a -> float =
 fun d x ->
  match d with
  | Dirac v -> if x = v then 0. else neg_infinity
  | Uniform (a, b) -> if a <= x && x <= b then -.log (b -. a) else neg_infinity
  | Bernoulli p -> if x then log p else log (1. -. p)
  | Binomial (p, n) ->
      log_combination n x
      +. (Float.of_int x *. log p)
      +. (Float.of_int (n - x) *. log (1. -. p))
  | Beta (a, b) ->
      let log_beta = log_gamma a +. log_gamma b -. log_gamma (a +. b) in
      if x > 0. && x < 1. then
        ((a -. 1.) *. log x) +. ((b -. 1.) *. log (1. -. x)) -. log_beta
      else neg_infinity
  | Gaussian (mu, sigma) ->
      (-0.5 *. log (two_pi *. sigma *. sigma))
      -. (((x -. mu) ** 2.) /. (2. *. sigma *. sigma))
  | Categorical support -> List.assoc x support
  | Poisson _ -> assert false
  | Empirical _ ->
      raise
        (Invalid_argument "Logpdf is not defined for an Empirical distribution")

let mean : float t -> float = function
  | Dirac v -> v
  | Uniform (a, b) -> a +. (b /. 2.)
  | Beta (a, b) -> a /. (a +. b)
  | Gaussian (mu, _) -> mu
  | Categorical support ->
      let dist = normalize support in
      let weighted_sum, norm =
        List.fold_left
          (fun (ws, n) (v, p) -> (ws +. (v *. p), n +. p))
          (0., 0.) dist
      in
      weighted_sum /. norm
  | Empirical samples ->
      let norm = samples |> List.length |> Float.of_int in
      let sum = List.fold_left (fun s v -> s +. v) 0. samples in
      sum /. norm

let mean_int : int t -> float = function
  | Dirac v -> Float.of_int v
  | Binomial (p, n) -> Float.of_int n *. p
  | Categorical support ->
      let dist = normalize support in
      let weighted_sum, norm =
        List.fold_left
          (fun (ws, n) (v, p) -> (ws +. (Float.of_int v *. p), n +. p))
          (0., 0.) dist
      in
      weighted_sum /. norm
  | Empirical samples ->
      let norm = samples |> List.length |> Float.of_int in
      let sum = List.fold_left (fun s v -> s +. Float.of_int v) 0. samples in
      sum /. norm
  | Poisson _ -> assert false

let var : float t -> float = function
  | Dirac _ -> 0.
  | Uniform (a, b) -> 1. /. 12. *. ((b -. a) ** 2.)
  | Beta (a, b) -> a *. b /. ((a +. b) *. (a +. b) *. (a +. b +. 1.))
  | Gaussian (_, sigma) -> sigma ** 2.
  | Categorical support ->
      let dist = normalize support in
      let weighted_sum, weighted_sq_sum =
        List.fold_left
          (fun (ws, wss) (v, p) -> (ws +. (v *. p), wss +. ((v ** 2.) *. p)))
          (0., 0.) dist
      in
      weighted_sq_sum -. (weighted_sum ** 2.)
  | Empirical samples ->
      let norm = samples |> List.length |> Float.of_int in
      let sq_sum, sum =
        List.fold_left
          (fun (ss, s) v -> (ss +. (v ** 2.), s +. v))
          (0., 0.) samples
      in
      (sq_sum /. norm) -. ((sum /. norm) ** 2.)

let var_int : int t -> float = function
  | Dirac _ -> 0.
  | Binomial (p, n) -> Float.of_int n *. p *. (1. -. p)
  | Categorical support ->
      let dist = normalize support in
      let weighted_sum, weighted_sq_sum =
        List.fold_left
          (fun (ws, wss) (v, p) ->
            (ws +. (Float.of_int v *. p), wss +. ((Float.of_int v ** 2.) *. p)))
          (0., 0.) dist
      in
      weighted_sq_sum -. (weighted_sum ** 2.)
  | Empirical samples ->
      let norm = samples |> List.length |> Float.of_int in
      let sq_sum, sum =
        List.fold_left
          (fun (ss, s) v -> (ss +. (Float.of_int v ** 2.), s +. Float.of_int v))
          (0., 0.) samples
      in
      (sq_sum /. norm) -. ((sum /. norm) ** 2.)
  | Poisson _ -> assert false

let std : float t -> float = fun dist -> sqrt (var dist)
let std_int : int t -> float = fun dist -> sqrt (var_int dist)
let stats : float t -> float * float = fun dist -> (mean dist, sqrt (var dist))

let stats_int : int t -> float * float =
 fun dist -> (mean_int dist, sqrt (var_int dist))

let get_support : type a. a t -> (a * float) list =
 fun dist ->
  match dist with
  | Dirac v -> [ (v, 0.) ]
  | Bernoulli p -> [ (true, p); (false, 1. -. p) ]
  | Empirical samples -> List.map (fun v -> (v, 0.)) samples
  | Categorical support -> support
  | Uniform _ | Binomial _ | Beta _ | Gaussian _ | Poisson _ ->
      raise (Invalid_argument "Support is not finite")

let categorical_to_list : type a. a t -> (a * float) list =
 fun dist ->
  let support = get_support dist in
  let dist = normalize support in
  let tbl = Hashtbl.create 11 in
  List.iter
    (fun (v, p) ->
      match Hashtbl.find_opt tbl v with
      | None -> Hashtbl.add tbl v p
      | Some p' -> Hashtbl.replace tbl v (p +. p'))
    dist;
  tbl |> Hashtbl.to_seq |> List.of_seq

let split dist =
  let support = get_support dist in
  let v1, v2 = support |> List.map fst |> List.split in
  let probs = support |> List.map snd in
  ( categorical ~support:(List.combine v1 probs),
    categorical ~support:(List.combine v2 probs) )

let split_list dist =
  let support = get_support dist in
  let values = support |> List.map fst in
  let logits = support |> List.map snd in
  let rec split res sup =
    if List.for_all (fun v -> List.is_empty v) sup then res
    else
      let res = split res (List.map (fun x -> List.tl x) sup) in
      let values = List.map (fun x -> List.hd x) sup in
      let support = List.combine values logits in
      categorical ~support :: res
  in
  split [] values

let split_array dist =
  let support = get_support dist in
  let support = List.map (fun (a, l) -> (Array.to_list a, l)) support in
  let d = split_list (categorical ~support) in
  Array.of_list d

let take n l = l |> List.to_seq |> Seq.take n |> List.of_seq
let drop n l = l |> List.to_seq |> Seq.drop n |> List.of_seq
