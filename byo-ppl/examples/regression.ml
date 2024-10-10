open Byoppl
open Distribution
open Basic.Importance_sampling

let regression _prob _data = assert false

let data =
  List.init 100 (fun i -> (Float.of_int i, (Float.of_int i *. 2.) +. 1.))

let _ =
  Format.printf "@.-- Regressin, Basic Importance Sampling --@.";
  let a_dist, b_dist = split (infer regression data) in
  let a_m, a_s = stats a_dist in
  let b_m, b_s = stats b_dist in
  Format.printf "Regression: a=%f (%f), b=%f (%f)@." a_m a_s b_m b_s
