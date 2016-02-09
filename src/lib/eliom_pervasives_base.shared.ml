
type ('a, 'b) server_function_service =
  (unit, 'a, Eliom_service.post, Eliom_service.na_s,
   Eliom_service.co, Eliom_service.non_ext, [ `WithoutSuffix ],
   unit, [ `One of 'a Eliom_parameter.ocaml ] Eliom_parameter.param_name,
   Eliom_service.reg, 'b Eliom_service.ocaml)
  Eliom_service.service
