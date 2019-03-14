type gltype =
    | IntT
    | FloatT
    | BoolT
    | RuneT
    | StringT
    | StructT of (string list * gltype) list
    | ArrayT of int * gltype
    | SliceT of gltype
    | AliasT of string * gltype
    | FunctionT string * gltype list * gltype option

(* Do not allow use to alias functions *)