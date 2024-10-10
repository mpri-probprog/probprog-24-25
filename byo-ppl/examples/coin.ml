open Byoppl
open Distribution
open Basic.Rejection_sampling_hard

let coin _prob _data = assert false

let data =
  [ false; true; true; false; false; false; false; false; false; false ]

let _ =
  Format.printf "@.-- Coin, Basic Rejection Sampling --@.";
  let dist = infer coin data in
  let m, s = stats dist in
  Format.printf "Coin bias, mean: %f std:%f@." m s
