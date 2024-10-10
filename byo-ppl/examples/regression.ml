open Byoppl
open Distribution
open Basic.Importance_sampling

let regression prob data =
  let a = sample prob (gaussian ~mu:0. ~sigma:2.) in
  let b = sample prob (gaussian ~mu:0. ~sigma:2.) in
  List.iter
    (fun (x, y) -> observe prob (gaussian ~mu:((a *. x) +. b) ~sigma:0.01) y)
    data;
  (a, b)

let data =
  List.init 100 (fun i ->
      let a, b = (2.0, 1.0) in
      let w = draw (gaussian ~mu:0. ~sigma:0.01) in
      (Float.of_int i, (Float.of_int i *. a) +. b +. w))

let _ =
  Format.printf "@.-- Regressin, Basic Importance Sampling --@.";
  let a_dist, b_dist = split (infer ~n:100_000 regression data) in
  let a_m = mean a_dist in
  let b_m = mean b_dist in
  Format.printf "Regression: a=%f, b=%f@." a_m b_m
