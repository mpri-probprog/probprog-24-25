open Byoppl
open Distribution
open Basic.Rejection_sampling_hard

let funny_bernoulli prob () =
  let a = sample prob (bernoulli ~p:0.5) in
  let b = sample prob (bernoulli ~p:0.5) in
  let c = sample prob (bernoulli ~p:0.5) in
  let () = assume prob (a || b) in
  Bool.to_int a + Bool.to_int b + Bool.to_int c

let _ =
  Format.printf "@.-- Funny Bernoulli, Basic Rejection Sampling --@.";
  let dist = infer funny_bernoulli () in
  let support = categorical_to_list dist in
  List.iter (fun (v, w) -> Format.printf "%d %f@." v w) support

open Basic.Importance_sampling

let funny_bernoulli prob () =
  let a = sample prob (bernoulli ~p:0.5) in
  let b = sample prob (bernoulli ~p:0.5) in
  let c = sample prob (bernoulli ~p:0.5) in
  let () = assume prob (a || b) in
  Bool.to_int a + Bool.to_int b + Bool.to_int c

let _ =
  Format.printf "@.-- Funny Bernoulli, Basic Importance Sampling --@.";
  let dist = infer funny_bernoulli () in
  let support = categorical_to_list dist in
  List.iter (fun (v, w) -> Format.printf "%d %f@." v w) support
