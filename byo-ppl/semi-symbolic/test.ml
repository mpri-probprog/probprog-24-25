open Byoppl
open Semi_symb

(* pretty print a [dist] *)
let rec pp_dist fmt dist =
  match dist with
  | Beta (alpha, beta) ->
      Format.fprintf fmt "Beta(%a, %a)" pp_dist !alpha pp_dist !beta
  | Bernoulli p -> Format.fprintf fmt "Bernoulli(%a)" pp_dist !p
  | Gaussian (mu, sigma) ->
      Format.fprintf fmt "Gaussian(%a, %a)" pp_dist !mu pp_dist !sigma
  | V v -> Format.fprintf fmt "%f" v

(* pretty print mixture distributions *)
let pp_dist_support fmt (dist : dist Distribution.t) =
  let support = Distribution.categorical_to_list dist in
  Format.fprintf fmt "Support [ @[<hov>";
  List.iter (fun (d, p) -> Format.fprintf fmt "%f, %a; @," p pp_dist d) support;
  Format.fprintf fmt "]@]"

open Symbolic

let _ =
  Format.printf "@.XXX Symbolic XXX@.";

  let coin () =
    let z = sample (Beta (const 1., const 1.)) in
    List.iter (observe (Bernoulli z)) [ 0.; 1.; 0.; 0.; 1.; 0.; 0.; 0. ];
    z
  in
  let dist = infer coin () in
  Format.printf "Dist coin : %a @." pp_dist dist;

  let gaussian_gaussian () =
    let mu = sample (Gaussian (const 0., const 1.)) in
    List.iter (observe (Gaussian (mu, const 1.0))) (List.init 100 (fun _ -> 4.));
    mu
  in
  let dist = infer gaussian_gaussian () in
  Format.printf "Dist gaussian : %a @." pp_dist dist

(* here symbolic computation is not possible, raise [Not_tractable] *)
(* let _ =
   let coin_weird () =
     let z = sample (Gaussian (const 0.5, const 0.1)) in
     List.iter (observe (Bernoulli z)) [ 0.; 1.; 0.; 0.; 1.; 0.; 0.; 0. ];
     z
   in
   let dist = infer coin_weird () in
   Format.printf "Dist coin weird : %a @." pp_dist dist *)

open Semi_symbolic

let _ =
  Format.printf "@.XXX Semi-Symbolic XXX@.";
  let coin prob () =
    let z = sample prob (Beta (const 1., const 1.)) in
    List.iter (observe prob (Bernoulli z)) [ 0.; 1.; 0.; 0.; 1.; 0.; 0.; 0. ];
    z
  in
  let dist = infer coin () in
  Format.printf "Beta-Bernoulli: %a@." pp_dist_support dist;

  let gaussian_gaussian prob () =
    let mu = sample prob (Gaussian (const 0., const 1.)) in
    List.iter
      (observe prob (Gaussian (mu, const 1.0)))
      (List.init 100 (fun _ -> 4.));
    mu
  in
  let dist = infer gaussian_gaussian () in
  Format.printf "Gauss-Gauss: %a@." pp_dist_support dist;

  (* Test with no conjugacy relation *)
  let coin_weird prob () =
    let z = sample prob (Gaussian (const 0.5, const 0.1)) in
    List.iter (observe prob (Bernoulli z)) [ 0.; 1.; 0.; 0.; 1.; 0.; 0.; 0. ];
    z
  in
  let dist = infer coin_weird () in
  Format.printf "Coin-Weird: %a@." pp_dist_support dist;

  (* [mu] should be symbolic, [z] should be sampled *)
  let weird prob () =
    let z = sample prob (Gaussian (const 0., const 10.)) in
    let mu = sample prob (Gaussian (z, const 0.1)) in
    observe prob (Gaussian (mu, const 0.5)) 1.;
    mu
  in
  let dist = infer weird () in
  Format.printf "Weird: %a@." pp_dist_support dist
