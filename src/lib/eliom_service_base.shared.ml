(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2007-2010 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Manipulation of services - this code can be use on server or client side. *)

open Eliom_lib
open Eliom_parameter

(** Typed services *)
type suff =
  [ `WithSuffix
  | `WithoutSuffix ]

type co_flag = [ `Co | `Non_co ]

type ext_flag = [ `Ext | `Non_ext ]

type get
type post
type put
type delete

type _ service_method =
  | Get    : get service_method
  | Post   : post service_method
  | Put    : put service_method
  | Delete : delete service_method

(* post means that there is at least one post param (possibly only the
   state post param). get is for all the other cases.  *)

type registrable =
  [ `Registrable
  | `Unregistrable ]

type a_s = {
  prefix: string; (* name of the server and protocol, for external links. Ex: http://ocsigen.org *)
  subpath: Url.path; (* name of the service without parameters *)
  fullpath: Url.path; (* full path of the service = site_dir@subpath *)
  get_name: Eliom_common.att_key_serv;
  post_name: Eliom_common.att_key_serv;
  redirect_suffix: bool;
  priority: int;
}

type na_s = {
  na_name: Eliom_common.na_key_serv;
  keep_get_na_params: bool
       (*
          where bool is used only for `Post and means "keep_get_na_params":
          do we keep GET non-attached parameters in links (if any)
          (31/12/2007 - experimental -
          WAS: 'a, but may be removed (was not used))
       *)
  }

type 'a attached_info =
  | Attached : a_s -> a_s attached_info
  | Nonattached : na_s -> na_s attached_info

type send_appl_content =
  | XNever
  | XAlways
  | XSame_appl of string * string option
(* the string is the name of the application to which the service
   belongs and the option is the name of template *)
(** Whether the service is capable to send application content or not.
    (application content has type Eliom_service.eliom_appl_answer:
    content of the application container, or xhr redirection ...).
    A link towards a service with send_appl_content = XNever will
    always answer a regular http frame (this will stop the application if
    used in a regular link or form, but not with XHR).
    XAlways means "for all applications" (like redirections/actions).
    XSame_appl means "only for this application".
    If there is a client side application, and the service has
    XAlways or XSame_appl when it is the same application,
    then the link (or form or change_page) will expect application content.
*)

type service_kind =
  [ `Service
  | `AttachedCoservice
  | `NonattachedCoservice
  | `External ]

type http
type appl

type 'a ocaml
type 'a non_ocaml

type ext
type non_ext

type ('r, 'e) rt =
  | Ocaml  : ('r ocaml, ext) rt
  | Http   : (http non_ocaml, ext) rt
  | Appl   : (appl non_ocaml, non_ext) rt
  (* FIXME! temporary to get current registration modules
     working. REMOVE! *)
  | Unsafe : ('a, ext) rt

(* 'return is the value returned by the service *)
type ('get, 'post, 'meth, 'attached, +'co, +'ext,
      +'tipo, 'getnames, 'postnames, +'registr, +'rt) service = {
  pre_applied_parameters:
    (string * Eliommod_parameters.param) list String.Table.t
    (* non localized parameters *) *
    (string * Eliommod_parameters.param) list (* regular parameters *);
  get_params_type: ('get, 'tipo, 'getnames) Eliom_parameter.params_type;
  post_params_type: ('post, [`WithoutSuffix], 'postnames) Eliom_parameter.params_type;
  max_use: int option; (* Max number of use of this service *)
  timeout: float option; (* Timeout for this service (the service will
                            disappear if it has not been used during this amount of seconds) *)
  meth: 'meth service_method;
  kind: service_kind;
  info: 'attached attached_info;

  https: bool; (* force https *)
  keep_nl_params: [ `All | `Persistent | `None ];
  mutable send_appl_content : send_appl_content;
  (* XNever when we create the service, then changed at registration :/ *)

  (* If the service has a client-side implementation,
     we put the generating function here: *)
  mutable client_fun: ('get -> 'post ->
                       [ `Html] Eliom_content_core.Html5.elt Lwt.t)
      Eliom_lib.client_value option;

  service_mark :
    (unit, unit, 'meth,
     'attached, co_flag, ext_flag,
     suff, unit, unit, registrable, unit)
      service Eliom_common.wrapper;
}
  constraint 'co = [< co_flag ]
  constraint 'ext = [< ext_flag ]
  constraint 'tipo = [< suff ]
  constraint 'registr = [< registrable ]

let pre_wrap s =
  {s with
    get_params_type = Eliom_parameter.wrap_param_type s.get_params_type;
    post_params_type = Eliom_parameter.wrap_param_type s.post_params_type;
    service_mark = Eliom_common.empty_wrapper ();
  }

let service_mark () = Eliom_common.make_wrapper pre_wrap

let get_kind_ s = s.kind

let get_info {info} = info

let get_info_ (type y) (s : (_, _, _, y, _, _, _, _, _, _, _) service) =
  match s.info with
  | Attached k ->
    `Attached k
  | Nonattached k ->
    `Nonattached k
let get_pre_applied_parameters_ s = s.pre_applied_parameters
let get_get_params_type_ s = s.get_params_type
let get_post_params_type_ s = s.post_params_type
let get_prefix_ s = s.prefix
let get_sub_path_ s = s.subpath
let get_redirect_suffix_ s = s.redirect_suffix
let get_full_path_ s = s.fullpath
let get_get_name_ s = s.get_name
let get_post_name_ s = s.post_name
let get_na_name_ s = s.na_name
let get_na_keep_get_na_params_ s = s.keep_get_na_params
let get_max_use_ s = s.max_use
let get_timeout_ s = s.timeout
let get_https s = s.https
let get_priority_ s = s.priority
let get_client_fun_ s = s.client_fun
let set_client_fun_ s f = s.client_fun <- Some f

let is_external = function
  | {kind=`External} -> true
  | _ -> false

let default_priority = 0

let get_get_or_post s = s.meth

let change_get_num service attser n =
  {service with
   info = Attached {attser with
                    get_name = n}}


(** Satic directories **)
let static_dir_ ?(https = false) () =
  {
    pre_applied_parameters = String.Table.empty, [];
    get_params_type = Eliom_parameter.suffix
      (Eliom_parameter.all_suffix Eliom_common.eliom_suffix_name);
    post_params_type = Eliom_parameter.unit;
    max_use= None;
    timeout= None;
    kind = `Service;
    meth = Get;
    info = Attached {
        prefix = "";
        subpath = [""];
        fullpath = (Eliom_request_info.get_site_dir ()) @
                   [Eliom_common.eliom_suffix_internal_name];
        get_name = Eliom_common.SAtt_no;
        post_name = Eliom_common.SAtt_no;
        redirect_suffix = true;
        priority = default_priority;
      };
    https = https;
    keep_nl_params = `None;
    service_mark = service_mark ();
    send_appl_content = XNever;
    client_fun = None;
  }

let static_dir () = static_dir_ ()

let https_static_dir () = static_dir_ ~https:true ()

let get_static_dir_ ?(https = false)
    ?(keep_nl_params = `None) ~get_params () =
    {
     pre_applied_parameters = String.Table.empty, [];
     get_params_type =
        Eliom_parameter.suffix_prod
          (Eliom_parameter.all_suffix Eliom_common.eliom_suffix_name)
          get_params;
     post_params_type = Eliom_parameter.unit;
     max_use= None;
     timeout= None;
     kind = `Service;
     meth = Get;
     info = Attached {
         prefix = "";
         subpath = [""];
         fullpath = (Eliom_request_info.get_site_dir ()) @
                    [Eliom_common.eliom_suffix_internal_name];
         get_name = Eliom_common.SAtt_no;
         post_name = Eliom_common.SAtt_no;
         redirect_suffix = true;
         priority = default_priority;
       };
     https = https;
     keep_nl_params = keep_nl_params;
     service_mark = service_mark ();
     send_appl_content = XNever;
     client_fun = None;
   }

let static_dir_with_params ?keep_nl_params ~get_params () =
  get_static_dir_ ?keep_nl_params ~get_params ()

let https_static_dir_with_params ?keep_nl_params ~get_params () =
  get_static_dir_ ~https:true ?keep_nl_params ~get_params ()


(****************************************************************************)
let get_send_appl_content s = s.send_appl_content
let set_send_appl_content s n = s.send_appl_content <- n

(****************************************************************************)

type clvpreapp =
  { mutable clvpreapp_f : 'a 'b.
             (('a -> 'b -> [ `Html ] Eliom_content_core.Html5.elt Lwt.t)
                Eliom_lib.client_value ->
              'a ->
              (unit -> 'b -> [ `Html ] Eliom_content_core.Html5.elt Lwt.t)
                Eliom_lib.client_value)}

let preapply_client_fun =
  {clvpreapp_f = fun f getparams -> failwith "preapply_client_fun"}
(* will be initialized later (in eliom_content for now),
   when client syntax is available, with:
   fun f getparams -> {{ fun _ pp -> %f %getparams pp }} *)

let rec append_suffix l m = match l with
  | [] -> m
  | [eliom_suffix_internal_name] -> m
  | a::ll -> a::(append_suffix ll m)

let preapply ~service getparams =
  let nlp, preapp = service.pre_applied_parameters in
  let suff, nlp, params =
    Eliom_parameter.construct_params_list_raw
      nlp service.get_params_type getparams
  in
  {service with
   pre_applied_parameters = nlp, params@preapp;
   get_params_type = Eliom_parameter.unit;
   info =
     (match service.info with
      | Attached k ->
         Attached
          {k with
            subpath =
              (match suff with
               | Some suff -> append_suffix k.subpath suff
               | _ -> k.subpath);
            fullpath =
              (match suff with
               | Some suff -> append_suffix k.fullpath suff
               | _ -> k.fullpath);
          };
      | k -> k);
   client_fun =
     (match service.client_fun with
      | None -> None
      | Some f_cl_val ->
         Some (preapply_client_fun.clvpreapp_f f_cl_val getparams))
  }



let void_coservice' =
  {
    max_use= None;
    timeout= None;
    pre_applied_parameters = String.Table.empty, [];
    get_params_type = Eliom_parameter.unit;
    post_params_type = Eliom_parameter.unit;
    kind = `NonattachedCoservice;
    meth = Get;
    info = Nonattached
        {na_name = Eliom_common.SNa_void_dontkeep;
         keep_get_na_params= true;
        };
    https = false;
    keep_nl_params = `All;
    service_mark = service_mark ();
    client_fun = None;
    send_appl_content = XAlways;
  }

let https_void_coservice' =
  {
    max_use= None;
    timeout= None;
    pre_applied_parameters = String.Table.empty, [];
    get_params_type = Eliom_parameter.unit;
    post_params_type = Eliom_parameter.unit;
    kind = `NonattachedCoservice;
    meth = Get;
    info = Nonattached
        {na_name = Eliom_common.SNa_void_dontkeep;
         keep_get_na_params=true
        };
    https = true;
    keep_nl_params = `All;
    service_mark = service_mark ();
    client_fun = None;
    send_appl_content = XAlways;
  }

let void_hidden_coservice' =
  { void_coservice' with
    kind = `NonattachedCoservice;
    meth = Get;
    info = Nonattached
        { na_name = Eliom_common.SNa_void_keep;
         keep_get_na_params=true;
        };
  }

let https_void_hidden_coservice' =
  { void_coservice' with
    kind = `NonattachedCoservice;
    meth = Get;
    info = Nonattached
        {na_name = Eliom_common.SNa_void_keep;
         keep_get_na_params=true;
        };
  }

type clvnlget =
  { mutable clvnlget_f : 'a 'b 'c.
             (('a -> 'b -> [ `Html ] Eliom_content_core.Html5.elt Lwt.t)
                Eliom_lib.client_value ->
              (('a * 'c) -> 'b -> [ `Html ] Eliom_content_core.Html5.elt Lwt.t)
                Eliom_lib.client_value)}

type clvnlpost =
  { mutable clvnlpost_f : 'a 'b 'c.
             (('a -> 'b -> [ `Html ] Eliom_content_core.Html5.elt Lwt.t)
                Eliom_lib.client_value ->
              ('a -> ('b * 'c) -> [ `Html ] Eliom_content_core.Html5.elt Lwt.t)
                Eliom_lib.client_value)}

let add_nl_get_client =
  {clvnlget_f = fun f -> failwith "add_nl_get_client"}
(* will be initialized later  (in eliom_content for now),
   when client syntax is available, with:
   fun f -> {{ fun (g, _) p -> %f g p }} *)

let add_nl_post_client =
  {clvnlpost_f = fun f -> failwith "add_nl_post_client"}
(* will be initialized later (in eliom_content for now),
   when client syntax is available, with:
   fun f -> {{ fun g (p, _) -> %f g p }} *)

(*VVV Non localized parameters not implemented for client side services *)
let add_non_localized_get_parameters ~params ~service =
  {service with
   get_params_type =
     Eliom_parameter.nl_prod service.get_params_type params;
   client_fun = match service.client_fun with
     | None -> None
     | Some f -> Some (add_nl_get_client.clvnlget_f f)
  }

let add_non_localized_post_parameters ~params ~service =
  {service with
   post_params_type =
     Eliom_parameter.nl_prod service.post_params_type params;
   client_fun = match service.client_fun with
     | None -> None
     | Some f -> Some (add_nl_post_client.clvnlpost_f f)
  }

let keep_nl_params s = s.keep_nl_params



let register_delayed_get_or_na_coservice ~sp s =
  failwith "CSRF coservice not implemented client side for now"

let register_delayed_post_coservice  ~sp s getname =
  failwith "CSRF coservice not implemented client side for now"




(* external services *)
(** Create a main service (not a coservice) internal or external, get only *)
let service_aux_aux
    ~https
    ~prefix
    ~(path : Url.path)
    ~site_dir
    ~kind
    ~getorpost
    ?(redirect_suffix = true)
    ?(keep_nl_params = `None)
    ?(priority = default_priority)
    ~get_params
    ~post_params
    () =
(* ici faire une v�rification "duplicate parameter" ? *)
  {
   pre_applied_parameters = String.Table.empty, [];
   get_params_type = get_params;
   post_params_type = post_params;
   max_use= None;
   timeout= None;
   meth = getorpost;
   kind = kind;
   info = Attached
       {prefix = prefix;
        subpath = path;
        fullpath = site_dir @ path;
        get_name = Eliom_common.SAtt_no;
        post_name = Eliom_common.SAtt_no;
        redirect_suffix;
        priority;
       };
   https = https;
   keep_nl_params = keep_nl_params;
   service_mark = service_mark ();
   send_appl_content = XNever;
   client_fun = None;
  }


let external_service_
    ~prefix
    ~path
    ?keep_nl_params
    ~getorpost
    ~get_params
    ~post_params
    () =
  let suffix = Eliom_parameter.contains_suffix get_params in
  service_aux_aux
    ~https:false (* not used for external links *)
    ~prefix
    ~path:(Url.remove_internal_slash
            (match suffix with
               | None -> path
               | _ -> path@[Eliom_common.eliom_suffix_internal_name]))
    ~site_dir:[]
    ~kind:`External
    ~getorpost
    ?keep_nl_params
    ~redirect_suffix:false
    ~get_params
    ~post_params
    ()

let external_post_service
    ~prefix
    ~path
    ?keep_nl_params
    ~rt
    ~get_params
    ~post_params
    () =
  external_service_
    ~prefix
    ~path
    ?keep_nl_params
    ~getorpost:Post
    ~get_params
    ~post_params
    ()

let external_put_service
    ~prefix
    ~path
    ?keep_nl_params
    ~rt
    ~get_params
    () =
  external_service_
    ~prefix
    ~path
    ?keep_nl_params
    ~getorpost:Put
    ~get_params
    ~post_params:Eliom_parameter.raw_post_data
    ()

let external_delete_service
    ~prefix
    ~path
    ?keep_nl_params
    ~rt
    ~get_params
    () =
  external_service_
    ~prefix
    ~path
    ?keep_nl_params
    ~getorpost:Delete
    ~get_params
    ~post_params:Eliom_parameter.raw_post_data
    ()

let external_service
    ~prefix
    ~path
    ?keep_nl_params
    ~rt
    ~get_params
    () =
  external_service_
    ~prefix
    ~path
    ?keep_nl_params
    ~getorpost:Get
    ~get_params
    ~post_params:Eliom_parameter.unit
    ()



let untype_service_ s =
  (s : ('get, 'post, 'meth, 'attached, 'co, 'ext, 'tipo, 'getnames, 'postnames,'register,'return) service
   :> ('get, 'post, 'meth, 'attached, 'co, 'ext, 'tipo, 'getnames, 'postnames,'register,'http) service)

let eliom_appl_answer_content_type = "application/x-eliom"






(*****************************************************************************)

let uniqueid =
  let r = ref (-1) in
  fun () -> r := !r + 1; !r

let new_state () =
  (* FIX: we should directly produce a string of the right length *)
  (* 72bit of entropy is large enough:
     CSRF-safe services are short-lived;
     with 65536 services, the probability of a collision is about 2^-41.
  *)
  make_cryptographic_safe_string ~len:12 ()

(** Definition of services *)
let service_aux
    ~https
    ~path
    ?redirect_suffix
    ?keep_nl_params
    ?priority
    ~get_params =
  let sp = Eliom_common.get_sp_option () in
  match sp with
  | None ->
      (match Eliom_common.global_register_allowed () with
      | Some get_current_sitedata ->
          let sitedata = get_current_sitedata () in
          let path =
            Url.remove_internal_slash
              (Url.change_empty_list
                 (Url.remove_slash_at_beginning path))
          in
          let u = service_aux_aux
            ~https
            ~prefix:""
            ~path
            ~site_dir: (Eliom_common.get_site_dir sitedata)
            ~kind:`Service
            ~getorpost:Get
            ?redirect_suffix
            ?keep_nl_params
            ?priority
            ~get_params
            ~post_params:unit
            ()
          in
          Eliom_common.add_unregistered sitedata path;
          u
      | None ->
          raise (Eliom_common.Eliom_site_information_not_available
                   "service"))
  | Some sp ->
      let path =
        Url.remove_internal_slash
          (Url.change_empty_list
             (Url.remove_slash_at_beginning path))
      in
      service_aux_aux
        ~https
        ~prefix:""
        ~path:path
        ~site_dir:(Eliom_request_info.get_site_dir_sp sp)
        ~kind:(`Service)
        ~getorpost:Get
        ?redirect_suffix
        ?keep_nl_params
        ?priority
        ~get_params
        ~post_params:unit
        ()

let service
    ?(https = false)
    ~path
    ?keep_nl_params
    ?priority
    ~rt
    ~get_params
    () =
  ignore rt;
  let suffix = contains_suffix get_params in
  service_aux
    ~https
    ~path:(match suffix with
      | None -> path
      | _ -> path@[Eliom_common.eliom_suffix_internal_name])
    ?keep_nl_params
    ?redirect_suffix:suffix
    ?priority
    ~get_params

let default_csrf_scope = function
    (* We do not use the classical syntax for default
       value. Otherwise, the type for csrf_scope was:
       [< Eliom_common.user_scope > `Session] *)
  | None -> `Session Eliom_common_base.Default_ref_hier
  | Some c -> (c :> [Eliom_common.user_scope])

exception Unreachable_exn

let get_attached_info = function
  | {info = Attached k} ->
    k
  | _ ->
    failwith "get_attached_info"

let get_non_attached_info = function
  | {info = Nonattached k} ->
    k
  | _ ->
    failwith "get_non_attached_info"

let coservice
    ?name
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?(https = false)
    ~rt
    ~fallback
    ?keep_nl_params
    ~get_params
    () =
  ignore rt;
  let csrf_scope = default_csrf_scope csrf_scope in
  let k = get_attached_info fallback in
  (* (match Eliom_common.global_register_allowed () with
     | Some _ -> Eliom_common.add_unregistered k.path;
     | _ -> ()); *)
  {fallback with
   max_use= max_use;
   timeout= timeout;
   get_params_type = add_pref_params Eliom_common.co_param_prefix get_params;
   kind = `AttachedCoservice;
   meth = Get;
   info = Attached
       {k with
        get_name =
          (if csrf_safe
           then Eliom_common.SAtt_csrf_safe (uniqueid (),
                                             (csrf_scope:>Eliom_common.user_scope),
                                             csrf_secure)
           else
             (match name with
              | None -> Eliom_common.SAtt_anon (new_state ())
              | Some name -> Eliom_common.SAtt_named name));
       };
   https = https || fallback.https;
   keep_nl_params = (match keep_nl_params with
     | None -> fallback.keep_nl_params | Some k -> k);
   client_fun = None;
  }
(* Warning: here no GET parameters for the fallback.
   Preapply services if you want fallbacks with GET parameters *)


let coservice'
    ?name
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?(https = false)
    ?(keep_nl_params = `Persistent)
    ~rt
    ~get_params
    () =
  ignore rt;
  let csrf_scope = default_csrf_scope csrf_scope in
  (* (match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered_na n;
  | _ -> () (* Do we accept unregistered non-attached coservices? *)); *)
  (* (* Do we accept unregistered non-attached named coservices? *)
     match sp with
     | None ->
     ...
  *)
        {
(*VVV allow timeout and max_use for named coservices? *)
          max_use= max_use;
          timeout= timeout;
          pre_applied_parameters = String.Table.empty, [];
          get_params_type =
            add_pref_params Eliom_common.na_co_param_prefix get_params;
          post_params_type = unit;
          kind = `NonattachedCoservice;
          meth = Get;
          info = Nonattached
              {na_name =
                 (if csrf_safe
                  then Eliom_common.SNa_get_csrf_safe (uniqueid (),
                                                       (csrf_scope:>Eliom_common.user_scope),
                                                       csrf_secure)
                  else
                    match name with
                    | None -> Eliom_common.SNa_get' (new_state ())
                    | Some name -> Eliom_common.SNa_get_ name);
               keep_get_na_params= true
              };
          https = https;
          keep_nl_params = keep_nl_params;
          send_appl_content = XNever;
          service_mark = service_mark ();
          client_fun = None;
        }


let attach_coservice' :
  fallback:
  (unit, unit, get, a_s, _, [< `Non_ext],
   [< suff ], unit, unit, 'rg1, 'return1) service ->
  service:
  ('get, 'post, 'gp, na_s, [< `Co ], [< `Non_ext],
   [< `WithoutSuffix] as 'sf, 'gn, 'pn, 'rg2, 'return) service ->
  ('get, 'post, 'gp, a_s, [> `Co ], [> `Non_ext],
   'sf, 'gn, 'pn, [< registrable > `Unregistrable ], 'return) service =
  fun ~fallback ~service ->
  let {na_name} = get_non_attached_info service in
  let fallbackkind = get_attached_info fallback in
  let open Eliom_common in
  {
    pre_applied_parameters = service.pre_applied_parameters;
    get_params_type = service.get_params_type;
    post_params_type = service.post_params_type;
    https = service.https;
    keep_nl_params = service.keep_nl_params;
    service_mark = service_mark ();
    send_appl_content = service.send_appl_content;
    max_use = service.max_use;
    timeout = service.timeout;
    client_fun = service.client_fun;
    kind = `AttachedCoservice;
    meth = service.meth;
    info = Attached {
      prefix = fallbackkind.prefix;
      subpath = fallbackkind.subpath;
      fullpath = fallbackkind.fullpath;
      priority = fallbackkind.priority;
      redirect_suffix = fallbackkind.redirect_suffix;
      get_name = (match na_name with
          | SNa_get_ s -> SAtt_na_named s
          | SNa_get' s -> SAtt_na_anon s
          | SNa_get_csrf_safe a -> SAtt_na_csrf_safe a
          | SNa_post_ s -> fallbackkind.get_name (*VVV check *)
          | SNa_post' s -> fallbackkind.get_name (*VVV check *)
          | SNa_post_csrf_safe a -> fallbackkind.get_name (*VVV check *)
          | _ -> failwith "attach_coservice' non implemented for this kind of non-attached coservice. Please send us an email if you need this.");
      (*VVV Do we want to make possible to attach POST na coservices
        on GET attached coservices? *)
      post_name = (match na_name with
          | SNa_get_ s -> SAtt_no
          | SNa_get' s -> SAtt_no
          | SNa_get_csrf_safe a -> SAtt_no
          | SNa_post_ s -> SAtt_na_named s
          | SNa_post' s -> SAtt_na_anon s
          | SNa_post_csrf_safe a -> SAtt_na_csrf_safe a
          | _ -> failwith "attach_coservice' non implemented for this kind of non-attached coservice. Please send us an email if you need this.");
    };
  }

(****************************************************************************)
(* Create a service with post parameters in the server *)


let post_service_aux ~https ~fallback
    ?(keep_nl_params = `None) ?(priority = default_priority) ~post_params =
(* Create a main service (not a coservice) internal, post only *)
(* ici faire une v�rification "duplicate parameter" ? *)
  let k1 = get_attached_info fallback in
  {
   pre_applied_parameters = fallback.pre_applied_parameters;
   get_params_type = fallback.get_params_type;
   post_params_type = post_params;
   max_use= None;
   timeout= None;
   kind = fallback.kind;
   meth = Post;
   info = Attached {
       prefix = k1.prefix;
       subpath = k1.subpath;
       fullpath = k1.fullpath;
       get_name = k1.get_name;
       post_name = Eliom_common.SAtt_no;
       redirect_suffix = false;
       priority;
     };
   https = https;
   keep_nl_params = keep_nl_params;
   send_appl_content = XNever;
   service_mark = service_mark ();
   client_fun = None;
  }

let post_service ?(https = false) ~rt ~fallback
    ?keep_nl_params ?priority ~post_params () =
  (* POST service without POST parameters means that the service will
     answer to a POST request only.  *)
  ignore rt;
  let k1 = get_attached_info fallback in
  let path = k1.subpath in
  let sp = Eliom_common.get_sp_option () in
  let u = post_service_aux
      ~https ~fallback ?keep_nl_params ?priority ~post_params in
  match sp with
  | None ->
    (match Eliom_common.global_register_allowed () with
     | Some get_current_sitedata ->
       Eliom_common.add_unregistered (get_current_sitedata ()) path;
       u
     | None ->
       if fallback.kind = `Service
       then
         raise (Eliom_common.Eliom_site_information_not_available
                  "post_service")
       else u)
  | _ -> u
(* if the fallback is a coservice, do we get a coservice or a service? *)


let post_coservice
    ?name
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?(https = false)
    ~rt
    ~(fallback : (_, _, get, _, _, _, _, _, _, _, _) service)
    ?keep_nl_params
    ~post_params
    () =
  ignore rt;
  let csrf_scope = default_csrf_scope csrf_scope in
  let k1 = get_attached_info fallback in
  (* (match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered k1.path;
  | _ -> ()); *)
  {
    post_params_type = post_params;
    pre_applied_parameters = fallback.pre_applied_parameters;
    get_params_type = fallback.get_params_type;
    send_appl_content = fallback.send_appl_content;
    service_mark = service_mark ();
    max_use= fallback.max_use;
    timeout= fallback.timeout;
    meth = Post;
    kind = `AttachedCoservice;
    info = Attached
        {k1 with
         post_name =
           (if csrf_safe
            then Eliom_common.SAtt_csrf_safe (uniqueid (),
                                              (csrf_scope:>Eliom_common.user_scope),
                                              csrf_secure)
            else
              (match name with
               | None -> Eliom_common.SAtt_anon (new_state ())
               | Some name -> Eliom_common.SAtt_named name));
        };
    https = fallback.https;
    keep_nl_params =
      (match keep_nl_params with
       | None -> fallback.keep_nl_params
       | Some k -> k);
    client_fun = None;
  }
(* It is not possible to make a post_coservice function
   with an optional ?fallback parameter
   because the type 'get of the result depends on the 'get of the
   fallback. Or we must impose 'get = unit ...
 *)


let post_coservice'
    ?name
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use ?timeout
    ?(https = false)
    ?(keep_nl_params = `All)
    ?(keep_get_na_params = true)
    ~rt ~post_params () =
  ignore rt;
  let csrf_scope = default_csrf_scope csrf_scope in
  (* match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered None
  | _ -> () *)
  {
(*VVV allow timeout and max_use for named coservices? *)
    max_use= max_use;
    timeout= timeout;
    pre_applied_parameters = String.Table.empty, [];
    get_params_type = unit;
    post_params_type = post_params;
    meth = Post;
    kind = `NonattachedCoservice;
    info = Nonattached
        {na_name =
           (if csrf_safe
            then Eliom_common.SNa_post_csrf_safe (uniqueid (),
                                                  (csrf_scope:>Eliom_common.user_scope),
                                                  csrf_secure)
            else
              (match name with
               | None ->
                 Eliom_common.SNa_post' (new_state ())
               | Some name -> Eliom_common.SNa_post_ name));
         keep_get_na_params=keep_get_na_params
        };
    https = https;
    keep_nl_params = keep_nl_params;
    send_appl_content = XNever;
    service_mark = service_mark ();
    client_fun = None;
  }


(****************************************************************************)
(* Aux function to create services with [raw_post_data] as [post_params] *)
(* Used to create PUT and DELETE services. *)


let raw_post_data_service_aux ~getorpost
    ~https
    ~path
    ?redirect_suffix
    ?keep_nl_params
    ?priority
    ~get_params =
  let sp = Eliom_common.get_sp_option () in
  match sp with
  | None ->
      (match Eliom_common.global_register_allowed () with
      | Some get_current_sitedata ->
          let sitedata = get_current_sitedata () in
          let path =
            Url.remove_internal_slash
              (Url.change_empty_list
                 (Url.remove_slash_at_beginning path))
          in
          let u = service_aux_aux
            ~https
            ~prefix:""
            ~path
            ~site_dir: (Eliom_common.get_site_dir sitedata)
            ~kind:(`Service)
            ~getorpost
            ?redirect_suffix
            ?keep_nl_params
            ?priority
            ~get_params
            ~post_params:Eliom_parameter.raw_post_data
            ()
          in
          Eliom_common.add_unregistered sitedata path;
          u
      | None ->
          raise (Eliom_common.Eliom_site_information_not_available
                   "service"))
  | Some sp ->
      let path =
        Url.remove_internal_slash
          (Url.change_empty_list
             (Url.remove_slash_at_beginning path))
      in
      service_aux_aux
        ~https
        ~prefix:""
        ~path
        ~site_dir:(Eliom_request_info.get_site_dir_sp sp)
        ~kind:(`Service)
        ~getorpost
        ?redirect_suffix
        ?keep_nl_params
        ?priority
        ~get_params
        ~post_params:Eliom_parameter.raw_post_data
        ()

(****************************************************************************)
(* Create a PUT service with [raw_post_data] as content, in the server *)

let put_service
    ?(https = false)
    ~path
    ?keep_nl_params
    ?priority
    ~rt
    ~get_params
    () =
  ignore rt;
  let suffix = contains_suffix get_params in
  raw_post_data_service_aux ~getorpost:Put
    ~https
    ~path:(match suffix with
             | None -> path
             | _ -> path@[Eliom_common.eliom_suffix_internal_name])
    ?keep_nl_params
    ?redirect_suffix:suffix
    ?priority
    ~get_params

let put_coservice
    ?name
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?(https = false)
    ~rt
    ~fallback
    ?keep_nl_params
    ~get_params
    () =
  ignore rt;
  let csrf_scope = default_csrf_scope csrf_scope in
  let k = get_attached_info fallback in
  (* (match Eliom_common.global_register_allowed () with
     | Some _ -> Eliom_common.add_unregistered k.path;
     | _ -> ()); *)
  {
    pre_applied_parameters = fallback.pre_applied_parameters;
    post_params_type = fallback.post_params_type;
    send_appl_content = fallback.send_appl_content;
    service_mark = service_mark ();
    max_use= max_use;
    timeout= timeout;
    get_params_type = add_pref_params Eliom_common.co_param_prefix get_params;
    meth = Put;
    kind = `AttachedCoservice;
    info = Attached
        {k with
         get_name =
           (if csrf_safe
            then Eliom_common.SAtt_csrf_safe (uniqueid (),
                                              (csrf_scope:>Eliom_common.user_scope),
                                              csrf_secure)
            else
              (match name with
               | None -> Eliom_common.SAtt_anon (new_state ())
               | Some name -> Eliom_common.SAtt_named name));
        };
    https = https || fallback.https;
    keep_nl_params = (match keep_nl_params with
      | None -> fallback.keep_nl_params | Some k -> k);
    client_fun = None;
  }
(* Warning: here no GET parameters for the fallback.
   Preapply services if you want fallbacks with GET parameters *)

let put_coservice'
    ?name
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?(https = false)
    ?(keep_nl_params = `Persistent)
    ~rt ~get_params () =
  ignore rt;
  let csrf_scope = default_csrf_scope csrf_scope in
  (* (match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered_na n;
  | _ -> () (* Do we accept unregistered non-attached coservices? *)); *)
  (* (* Do we accept unregistered non-attached named coservices? *)
     match sp with
     | None ->
     ...
  *)
        {
(*VVV allow timeout and max_use for named coservices? *)
          max_use= max_use;
          timeout= timeout;
          pre_applied_parameters = String.Table.empty, [];
          get_params_type =
            add_pref_params Eliom_common.na_co_param_prefix get_params;
          post_params_type = Eliom_parameter.raw_post_data;
          meth = Put;
          kind = `NonattachedCoservice;
          info = Nonattached
            {na_name =
                (if csrf_safe
                 then Eliom_common.SNa_get_csrf_safe (uniqueid (),
                                                      (csrf_scope:>Eliom_common.user_scope),
                                                      csrf_secure)
                 else
                   match name with
                     | None -> Eliom_common.SNa_get' (new_state ())
                     | Some name -> Eliom_common.SNa_get_ name);
             keep_get_na_params = true;
            };
          https = https;
          keep_nl_params = keep_nl_params;
          send_appl_content = XNever;
          service_mark = service_mark ();
          client_fun = None;
        }



(****************************************************************************)
(* Create a DELETE service with [raw_post_data] as content, in the server *)

let delete_service
    ?(https = false)
    ~path
    ?keep_nl_params
    ?priority
    ~rt
    ~get_params
    () =
  ignore rt;
  let suffix = contains_suffix get_params in
  raw_post_data_service_aux ~getorpost:Delete
    ~https
    ~path:(match suffix with
             | None -> path
             | _ -> path@[Eliom_common.eliom_suffix_internal_name])
    ?keep_nl_params
    ?redirect_suffix:suffix
    ?priority
    ~get_params

let delete_coservice
    ?name
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?(https = false)
    ~rt
    ~fallback
    ?keep_nl_params
    ~get_params
    () =
  ignore rt;
  let csrf_scope = default_csrf_scope csrf_scope in
  let k = get_attached_info fallback in
  (* (match Eliom_common.global_register_allowed () with
     | Some _ -> Eliom_common.add_unregistered k.path;
     | _ -> ()); *)
  {fallback with
   max_use= max_use;
   timeout= timeout;
   get_params_type = add_pref_params Eliom_common.co_param_prefix get_params;
   meth = Delete;
   kind = `AttachedCoservice;
   info = Attached
     {k with
      get_name =
         (if csrf_safe
          then Eliom_common.SAtt_csrf_safe (uniqueid (),
                                            (csrf_scope:>Eliom_common.user_scope),
                                            csrf_secure)
          else
            (match name with
               | None -> Eliom_common.SAtt_anon (new_state ())
               | Some name -> Eliom_common.SAtt_named name));
     };
   https = https || fallback.https;
   keep_nl_params = (match keep_nl_params with
     | None -> fallback.keep_nl_params | Some k -> k);
   client_fun = None;
 }
(* Warning: here no GET parameters for the fallback.
   Preapply services if you want fallbacks with GET parameters *)

let delete_coservice'
    ?name
    ?(csrf_safe = false)
    ?csrf_scope
    ?csrf_secure
    ?max_use
    ?timeout
    ?(https = false)
    ?(keep_nl_params = `Persistent)
    ~rt ~get_params () =
  ignore rt;
  let csrf_scope = default_csrf_scope csrf_scope in
  (* (match Eliom_common.global_register_allowed () with
  | Some _ -> Eliom_common.add_unregistered_na n;
  | _ -> () (* Do we accept unregistered non-attached coservices? *)); *)
  (* (* Do we accept unregistered non-attached named coservices? *)
     match sp with
     | None ->
     ...
  *)
        {
(*VVV allow timeout and max_use for named coservices? *)
          max_use= max_use;
          timeout= timeout;
          pre_applied_parameters = String.Table.empty, [];
          get_params_type =
            add_pref_params Eliom_common.na_co_param_prefix get_params;
          post_params_type = Eliom_parameter.raw_post_data;
          meth = Delete;
          kind = `NonattachedCoservice;
          info = Nonattached
            {na_name =
                (if csrf_safe
                 then Eliom_common.SNa_get_csrf_safe (uniqueid (),
                                                      (csrf_scope:>Eliom_common.user_scope),
                                                      csrf_secure)
                 else
                   match name with
                     | None -> Eliom_common.SNa_get' (new_state ())
                     | Some name -> Eliom_common.SNa_get_ name);
             keep_get_na_params = true
            };
          https = https;
          keep_nl_params = keep_nl_params;
          send_appl_content = XNever;
          service_mark = service_mark ();
          client_fun = None;
        }
