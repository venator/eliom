(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2007 Vincent Balat
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

(** Functions to add non localised parameters to services and
    client side declaration of void coservices. Void coservices are the only ones
    defined on client side. *)

open Eliom_parameter
open Eliom_lib

(** {2 Type definitions for services} *)

(** {3 Kind of parameters} *)

include Eliom_service_sigs.S_with_external

(** {2 Predefined services} *)

(** {3 Static files} *)

(** The predefined service [static_dir] allows one to create links to
    static files. This service takes the name of a static file as a
    parameter (a string list, slash separated). The actual directory
    in filesystem where static pages will be found must be set up in
    the configuration file with the staticmod extension. *)
val static_dir :
  unit ->
  (string list, unit, get, a_s, non_co, non_ext,
   [ `WithSuffix ],
   [ `One of string list ] param_name, unit, non_reg, 'return)
    service

(** Same as {!static_dir} but forcing https link. *)
val https_static_dir :
  unit ->
  (string list, unit, get, a_s, non_co, non_ext,
   [ `WithSuffix ],
   [ `One of string list ] param_name, unit, non_reg, 'return)
    service

(** Like [static_dir], but allows one to put GET parameters *)
val static_dir_with_params :
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [`WithoutSuffix], 'an) params_type ->
  unit ->
  ((string list * 'a), unit, get, a_s, non_co, non_ext,
   [ `WithSuffix ],
   [ `One of string list ] param_name *'an, unit, non_reg, 'return)
    service

(** Same as {!static_dir_with_params} but forcing https link. *)
val https_static_dir_with_params :
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [`WithoutSuffix], 'an) params_type ->
  unit ->
  ((string list * 'a), unit, get, a_s, [> `Non_co ], [> `Non_ext ],
   [ `WithSuffix ],
   [ `One of string list ] param_name *'an, unit, non_reg, 'return)
    service


(** {3 Void non-attached coservices} *)

val void_coservice' :
  (unit, unit, get, na_s, co, non_ext,
   [ `WithoutSuffix ],
   unit, unit, non_reg, _ non_ocaml)
  service
(** A predefined non-attached action with special behaviour:
    it has no parameter at all, even non-attached parameters.
    Use it if you want to make a link to the current page without non-attached
    parameters.
    It is almost equivalent to a POST non-attached service without POST
    parameters, on which you register an action that does nothing,
    but you can use it with <a> links, not only forms.
    It does not keep non attached GET parameters.
 *)

val https_void_coservice' :
  (unit, unit, get, na_s, co, non_ext,
   [ `WithoutSuffix ],
   unit, unit, non_reg, _ non_ocaml)
  service
(** The same, but forcing https. *)

val void_hidden_coservice' :
  (unit, unit, get, na_s, co, non_ext,
   [ `WithoutSuffix ],
   unit, unit, non_reg, _ non_ocaml)
  service
(** Same as [void_coservice'] but keeps non attached GET parameters.
 *)

val https_void_hidden_coservice' :
  (unit, unit, get, na_s, co, non_ext,
   [ `WithoutSuffix ],
   unit, unit, non_reg, 'return)
  service
(** The same, but forcing https. *)


(** {2 Miscellaneous} *)

(** The function [preapply ~service paramaters] creates a new service
    by preapplying [service] to the GET [parameters]. It is not
    possible to register a handler on an preapplied service ;
    preapplied services may be used in links or as fallbacks for
    coservices *)
val preapply :
  service:('a, 'b, 'meth,a_s as 'att,'co, 'ext, [< suff ], 'e, 'f, 'g, 'return) service ->
  'a ->
  (unit, 'b, 'meth,'att, 'co, 'ext, [ `WithoutSuffix ], unit, 'f, non_reg, 'return) service

(** [attach_coservice' ~fallback ~service] attaches the non-attached
    coservice [service] on the URL of [fallback]. This allows to
    create a link to a non-attached coservice but with another URL
    than the current one. It is not possible to register something
    on the service returned by this function. *)
val attach_coservice' :
  fallback:
    (unit, unit, get, a_s, _, non_ext,
     [< suff ], unit, unit, 'rg1, 'return1) service ->
  service:
    ('get, 'post, 'meth, na_s, co, non_ext,
     [< `WithoutSuffix] as 'sf, 'gn, 'pn, 'rg2, 'return) service ->
  ('get, 'post, 'meth, a_s, co, non_ext,
   'sf, 'gn, 'pn, non_reg, 'return) service


(** {3 Localized parameters} *)

val add_non_localized_get_parameters :
  params:('p, [ `WithoutSuffix ], 'pn) non_localized_params ->
  service:('a, 'b, 'meth,'attach, 'co, 'ext, 'd, 'e, 'f, 'g, 'return) service ->
  ('a * 'p, 'b, 'meth, 'attach, 'co, 'ext, 'd, 'e * 'pn, 'f, 'g, 'return) service
(** Adds non localized GET parameters to a service *)

val add_non_localized_post_parameters :
  params:('p, [ `WithoutSuffix ], 'pn) non_localized_params ->
  service:('a, 'b, 'meth,'attach,'co, 'ext, 'd, 'e, 'f, 'g, 'return) service ->
  ('a, 'b * 'p, 'meth,'attach, 'co, 'ext, 'd, 'e, 'f * 'pn, 'g, 'return) service
(** Adds non localized POST parameters to a service *)

(** {3 Static files} *)

(** The predefined service [static_dir] allows one to create links to
    static files. This service takes the name of a static file as a
    parameter (a string list, slash separated). The actual directory
    in filesystem where static pages will be found must be set up in
    the configuration file with the staticmod extension. *)
val static_dir :
  unit ->
  (string list, unit, get, a_s, [> `Non_co ], [> `Non_ext ],
   [ `WithSuffix ],
   [ `One of string list ] param_name, unit, non_reg,
   http non_ocaml)
    service

(** Same as {!static_dir} but forcing https link. *)
val https_static_dir :
  unit ->
  (string list, unit, get, a_s, [> `Non_co ], [> `Non_ext ],
   [ `WithSuffix ],
   [ `One of string list ] param_name, unit, non_reg,
   http non_ocaml)
    service

(** Like [static_dir], but allows one to put GET parameters *)
val static_dir_with_params :
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [`WithoutSuffix], 'an) params_type ->
  unit ->
  ((string list * 'a), unit, get, a_s, [> `Non_co ], [> `Non_ext ],
   [ `WithSuffix ],
   [ `One of string list ] param_name *'an, unit, non_reg,
   http non_ocaml)
    service

(** Same as {!static_dir_with_params} but forcing https link. *)
val https_static_dir_with_params :
  ?keep_nl_params:[ `All | `Persistent | `None ] ->
  get_params:('a, [`WithoutSuffix], 'an) params_type ->
  unit ->
  ((string list * 'a), unit,
   get,
   a_s,
   [> `Non_co ], [> `Non_ext ],
   [ `WithSuffix ],
   [ `One of string list ] param_name *'an, unit, non_reg,
   http non_ocaml)
    service




(**/**)

(* used by Eliom_uri *)
val get_get_or_post :
  (_, _, 'm, _, _, _, _, _, _, _, _) service -> 'm service_method

val get_info :
  (_, _, _, 'att, _, _, _, _, _, _, _) service ->
  'att attached_info

val get_info_ :
  (_,_,_,_,_,_,_,_,_,_,_) service ->
  [ `Attached of a_s | `Nonattached of na_s ]

val is_external : (_, _, _, _, _, _, _, _, _, _, _) service -> bool
val get_pre_applied_parameters_ : (_, _, _, _, _, _, _, _, _, _, _) service ->
  (string * Eliommod_parameters.param) list String.Table.t *
  (string * Eliommod_parameters.param) list
val get_get_params_type_ : ('a, 'b, 'meth,'attch, _, _, 'd, 'e, 'f, 'g, 'return) service ->
  ('a, 'd, 'e) Eliom_parameter.params_type
val get_post_params_type_ : ('a, 'b, 'meth,'attch, _, _, 'd, 'e, 'f, 'g, 'return) service ->
  ('b, [ `WithoutSuffix ], 'f) Eliom_parameter.params_type
val get_sub_path_ : a_s -> Url.path
val get_full_path_ : a_s -> Url.path
val get_prefix_ :   a_s -> string
val get_get_name_ : a_s -> Eliom_common.att_key_serv
val get_post_name_ : a_s -> Eliom_common.att_key_serv
val get_redirect_suffix_ : a_s -> bool
val get_na_name_ : na_s -> Eliom_common.na_key_serv
val get_na_keep_get_na_params_: na_s -> bool
val get_max_use_ : (_, _, _, _, _, _, _, _, _, _, _) service -> int option
val get_timeout_ : (_, _, _, _, _, _, _, _, _, _, _)service -> float option
val get_https : (_, _, _, _, _, _, _, _, _, _, _) service -> bool
val get_priority_ : a_s -> int
val get_client_fun_ :
  ('a, 'b, _, _, _, _, _, _, _, _, _) service ->
  ('a -> 'b -> [ `Html ] Eliom_content_core.Html5.elt Lwt.t) client_value option

val keep_nl_params : (_, _, _, _, _, _, _, _, _, _, _) service ->
  [ `All | `Persistent | `None ]

val change_get_num :
  ('a, 'b, 'meth,'attch, 'co, 'ext, 'd, 'e, 'f, 'g, 'return) service ->
  a_s ->
  Eliom_common.att_key_serv ->
  ('a, 'b, 'meth, a_s, 'co, 'ext, 'd, 'e, 'f, 'i, 'return) service

(* Not implemented on client side: TODO should not be called in Eliom_uri *)
val register_delayed_get_or_na_coservice :
  sp:Eliom_common.server_params ->
  (int * Eliom_common.user_scope * bool option) ->
  string

val register_delayed_post_coservice :
  sp:Eliom_common.server_params ->
  (int * Eliom_common.user_scope * bool option) ->
  Eliom_common.att_key_serv -> string

(* used by eliommod_mkform *)
type send_appl_content =
  | XNever
  | XAlways
  | XSame_appl of string * string option
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

(** Returns the name of the application to which belongs the service, if any. *)
val get_send_appl_content :
  (_, _, _, _, _, _, _, _, _, _, _) service -> send_appl_content

val xhr_with_cookies :
  (_, _, _, _, _, _, _, _, _, _, _) service -> string option option
