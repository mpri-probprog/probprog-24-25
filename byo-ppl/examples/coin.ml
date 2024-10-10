open Byoppl
open Distribution
open Basic.Rejection_sampling_hard

let coin prob data =
  let z = sample prob (uniform ~a:0. ~b:1.) in
  List.iter (observe prob (bernoulli ~p:z)) data;
  z

let data =
  [ false; true; true; false; false; false; false; false; false; false ]

let _ =
  Format.printf "@.-- Coin, Basic Rejection Sampling --@.";
  let dist = infer coin data in
  let m, s = stats dist in
  Format.printf "Coin bias, mean: %f std:%f@." m s

open Basic.Importance_sampling

let coin prob data =
  let z = sample prob (uniform ~a:0. ~b:1.) in
  List.iter (observe prob (bernoulli ~p:z)) data;
  z

let data =
  [ false; true; true; false; false; false; false; false; false; false ]

let _ =
  Format.printf "@.-- Coin, Basic Importance Sampling --@.";
  let dist = infer coin data in
  let m, s = stats dist in
  Format.printf "Coin bias, mean: %f std:%f@." m s
