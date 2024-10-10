let return x k = k x
let ( let* ) x f k = x (fun y -> f y k)

module Cps_list = struct
  let rec iter f l k =
    match l with [] -> k () | h :: t -> f h (fun () -> iter f t k)

  let iteri f l k =
    let rec traversal i l k =
      match l with
      | [] -> k ()
      | h :: t -> f i h (fun () -> traversal (i + 1) t k)
    in
    traversal 0 l k

  let fold f l ~init k =
    let rec traversal l acc k =
      match l with
      | [] -> k acc
      | h :: t -> f h acc (fun acc -> traversal t acc k)
    in
    traversal l init k

  let map f l k =
    let rec traversal l acc k =
      match l with
      | [] -> k []
      | h :: t -> f h (fun y -> traversal t acc (fun acc -> k (y :: acc)))
    in
    traversal l [] k
end

module Cps_array = struct
  let iter f a k =
    let rec traversal i k =
      if i < Array.length a then f a.(i) (fun () -> traversal (i + 1) k)
      else k ()
    in
    traversal 0 k

  let iteri f a k =
    let rec traversal i k =
      if i < Array.length a then f i a.(i) (fun () -> traversal (i + 1) k)
      else k ()
    in
    traversal 0 k

  let fold f a ~init k =
    let rec traversal i acc k =
      if i < Array.length a then
        f a.(i) acc (fun acc -> traversal (i + 1) acc k)
      else k acc
    in
    traversal 0 init k

  let map f a k =
    f a.(0) (fun a0 ->
        let b = Array.make (Array.length a) a0 in
        let rec traversal i k =
          if i < Array.length a then
            f a.(i) (fun bi ->
                b.(i) <- bi;
                traversal (i + 1) k)
          else k b
        in
        traversal 0 k)
end
