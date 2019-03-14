(* categories of values in golite *)
type vcat =
    | Type
    | Constant
    | Variable

(* all valid types in golite *)
type gltype =
    | IntT
    | FloatT
    | BoolT
    | RuneT
    | StringT
    | NamedT of string
    | StructT of (string list * gltype) list
    | ArrayT of int * gltype
    | SliceT of gltype
    | FunctionT of string * gltype list * gltype
    | Void

