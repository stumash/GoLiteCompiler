let ifsome o f =
    match o with
    | Some a -> f a
    | _      -> ()

