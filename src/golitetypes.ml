type gltype =
    | IntT
    | FloatT
    | BoolT
    | RuneT
    | StringT
    | StructT of (string list * gltype)
    | ArrayT of int * gltype
    | SliceT of gltype
