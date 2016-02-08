
type ('a, +'b) server_function_service =
  (unit, 'a, [`Post], Eliom_service.na_s,
   [ `Co ], [ `Non_ext ], [ `WithoutSuffix ],
   unit, [ `One of 'a Eliom_parameter.ocaml ] Eliom_parameter.param_name,
   [ `Registrable ],
   'b Eliom_service.ocaml)
  Eliom_service.service
