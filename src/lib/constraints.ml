open Syntax

type constr =
  | CEqual of ty * ty
  | CConsistent of ty * ty

module IConstraints = Set.Make (
  struct
    type t = constr
    let compare (x : constr) y = compare x y
  end
)

module Constraints = struct
  include IConstraints

  let map f c = IConstraints.fold (fun x l -> (f x) :: l) c []
end
