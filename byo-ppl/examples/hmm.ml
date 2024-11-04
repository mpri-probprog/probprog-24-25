open Byoppl
open Distribution
open Basic.Importance_sampling

let hmm prob data =
  let rec gen states data =
    match (states, data) with
    | [], y :: data -> gen [ y ] data
    | states, [] -> states
    | pre_x :: _, y :: data ->
        let x = sample prob (gaussian ~mu:pre_x ~sigma:1.0) in
        let () = observe prob (gaussian ~mu:x ~sigma:1.0) y in
        gen (x :: states) data
  in
  gen [] data

let data = List.init 50 (fun i -> Float.of_int i)

let _ =
  Format.printf "@.-- HMM, Basic Importance Sampling --@.";
  let dist = Distribution.split_list (infer ~n:5000 hmm data) in
  let m_x = List.rev (List.map Distribution.mean dist) in
  List.iter2 (Format.printf "%f >> %f@.") data m_x

open Basic.Metropolis_hastings

let hmm prob data =
  let rec gen i states data =
    match (states, data) with
    | [], y :: data -> gen 1 [ y ] data
    | states, [] -> states
    | pre_x :: _, y :: data ->
        let x =
          sample prob (gaussian ~mu:pre_x ~sigma:1.0) ("x_" ^ Int.to_string i)
        in
        let () = observe prob (gaussian ~mu:x ~sigma:1.0) y in
        gen (i + 1) (x :: states) data
  in
  gen 0 [] data

let _ =
  Format.printf "@.-- HMM, Basic Metropolis Hastings --@.";
  let dist = Distribution.split_list (infer ~warmup:1000 ~n:5000 hmm data) in
  let m_x = List.rev (List.map Distribution.mean dist) in
  List.iter2 (Format.printf "%f >> %f@.") data m_x

open Cps_operators
open Infer.Particle_filter

let hmm data =
  let rec gen states data =
    match (states, data) with
    | [], y :: data -> gen [ y ] data
    | states, [] -> return states
    | pre_x :: _, y :: data ->
        let* x = sample (gaussian ~mu:pre_x ~sigma:1.0) in
        let* () = observe (gaussian ~mu:x ~sigma:1.0) y in
        gen (x :: states) data
  in
  gen [] data

let _ =
  Format.printf "@.-- HMM, CPS Particle Filter --@.";
  let dist = Distribution.split_list (infer hmm ~n:10 data) in
  let m_x = List.map Distribution.mean (List.rev dist) in
  List.iter2 (Format.printf "%f %f@.") data m_x
