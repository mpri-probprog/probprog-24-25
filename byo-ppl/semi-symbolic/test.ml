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
  Format.printf "@.XXX Symbolic XXX@. TODO";

  let beta_bernoulli v = assert false in
  let dist = infer beta_bernoulli 1. in
  Format.printf "Dist beta_bernoulli : %a @." pp_dist dist

open Semi_symbolic

let _ =
  Format.printf "@.XXX Semi-Symbolic XXX@.";

  let beta_bernoulli v = assert false in
  let dist = infer beta_bernoulli 1. in
  Format.printf "Dist beta_bernoulli : %a @." pp_dist_support dist
