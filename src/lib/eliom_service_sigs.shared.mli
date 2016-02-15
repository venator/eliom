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

module type S_types = sig

  type get
  type put
  type post
  type delete

  type _ service_method =
    | Get    : get service_method
    | Post   : post service_method
    | Put    : put service_method
    | Delete : delete service_method

  type a_s
  type na_s

  type 'a attached_info =
    | Attached : a_s -> a_s attached_info
    | Nonattached : na_s -> na_s attached_info

  type co
  type non_co

  type ext
  type non_ext

  type http
  type appl

  type 'a ocaml
  type 'a non_ocaml

  type reg
  type non_reg

  type ('r, 'e) rt =
    | Ocaml  : ('r ocaml, ext) rt
    | Http   : (http non_ocaml, ext) rt
    | Appl   : (appl non_ocaml, non_ext) rt
    (* FIXME! temporary to get current registration modules
       working. REMOVE! *)
    | Unsafe : ('a, ext) rt

  (** Type of services.
      - ['get] is the type of GET parameters expected by the service.
      - ['post] is the type of POST parameters expected by the service.
      - ['meth] the HTTP method
      - ['attached] attached or non-attached
      - ['co] co-service or regular service
      - ['ext] external or internal
      - ['tipo] the type paremeter of subtype {!suff} states the kind
        of parameters it uses: suffix or not.
      - ['gn] is the type of GET parameters names. See
        {!Eliom_parameter.param_name} and form generation functions
        (e. g. {!Eliom_content.Html5.D.get_form}).
      - ['pn] is the type of POST parameters names. See
        {!Eliom_parameter.param_name} and form generation functions
        (e. g. {!Eliom_content.Html5.D.post_form}).
      - ['reg]: possible to register a handler on this service
      - [ 'ret] is an information on what the service returns.
        See {!Eliom_registration.kind}. *)
  type ('get, 'post, 'meth, 'attached, 'co, 'ext,
        +'tipo, 'gn, 'pn, 'reg, +'ret) service
    constraint 'tipo = [< `WithSuffix | `WithoutSuffix ]

end

module type S = sig

  include S_types

  (** {2 Definitions of services}

      {e Warning: These functions must be called when the site
      information is available, that is, either during a request or
      during the initialisation phase of the site.  Otherwise, it will
      raise the exception
      {!Eliom_common.Eliom_site_information_not_available}.  If you
      are using static linking, you must delay the call to this
      function until the configuration file is read, using
      {!Eliom_service.register_eliom_module}. Otherwise you will also
      get this exception.}  *)

  (** {3 Main services} *)

  (** The function [service ~path ~get_params ()] creates a {!service}
      associated to the path [path] and taking [get_params] as GET
      parameters.

      If the optional parameter [~https:true] is given, all links
      towards that service will use https. By default, links will keep
      the current protocol.

      The optional parameter [~priority] allows one to change the
      priority order between service that shares the same path. The
      default priority is 0 ; if you want the service to be tried
      before (resp. after) other services, put a higher (resp. lower)
      priority.

      If the optional parameter [~keep_nl_params:`Persistent]
      (resp. [~keep_nl_params:`All]) is given, all links towards that
      service will keep persistent (resp. all) non localized GET
      parameters of the current service. The default is [`None]. See
      the eliom manual for more information about {% <<a_manual
      chapter="params" fragment="nonlocalizedparameters"|non localized
      parameters>>%}.

      The parameter [~rt] is used to constrain the type parameter
      ['rt] of the service. *)
  val service :
    ?https:bool ->
    path:Eliom_lib.Url.path ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?priority:int ->
    rt:('rt, _) rt ->
    get_params:
      ('get, [< `WithSuffix | `WithoutSuffix ] as 'tipo,'gn)
        Eliom_parameter.params_type ->
    unit ->
    ('get, unit, get, a_s, non_co, non_ext,
     'tipo, 'gn, unit, reg, 'rt) service

  (** The function [post_service ~fallback ~post_params ()] creates a
      service that takes [post_params] as POST parameters and share
      the same path and GET parameters than the service [fallback].

      POST parameters couldn't contain a suffix parameter.

      The service [fallback] should be an (internal) attached service
      without POST parameters ; it couldn't be a preapplied service.
      This argument enforces the creation of an equivalent service (
      i.e. a service with the same path and GET parameters ) to be
      served when POST parameters are missing. Thus, the user cannot
      put a bookmark on a page that does not exist.

      See {!service} for a description of optional [~https],
      [~keep_nl_params], [~priority] and [~rt] parameters .  *)
  val post_service :
    ?https:bool ->
    rt:('rt, _) rt ->
    fallback:
      ('get, unit, get, a_s, 'co, non_ext,
       [< `WithSuffix | `WithoutSuffix] as 'tipo, 'gn, unit,
       reg, 'rt) service ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?priority:int ->
    post_params:
      ('post, [`WithoutSuffix], 'pn) Eliom_parameter.params_type ->
    unit ->
    ('get, 'post, post, a_s, 'co, non_ext,
     'tipo, 'gn, 'pn, reg, 'rt) service

  (** The function [put_service ~path ~get_params ()] creates a
      service that answers the HTTP PUT method, and only takes
      {!Eliom_parameter.raw_post_data} as POST parameter.

      [path] and [get_params], however, can be set at will.

      See {!service} for a description of optional [~https],
      [~keep_nl_params], [~priority] and [~rt] parameters . *)
  val put_service :
    ?https:bool ->
    path:Eliom_lib.Url.path ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?priority:int ->
    rt:('rt, _) rt ->
    get_params:
      ('get, [< `WithSuffix | `WithoutSuffix ] as 'tipo,'gn)
        Eliom_parameter.params_type ->
    unit ->
    ('get, Eliom_parameter.raw_post_data,
     put, a_s, non_co, non_ext, 'tipo, 'gn,
     Eliom_parameter.no_param_name, reg, 'rt) service

  (** The function [delete_service ~path ~get_params ()] creates a
      service that answers the HTTP DELETE method, and only takes
      {!Eliom_parameter.raw_post_data} as POST parameter.

      [path] and [get_params], however, can be set at will.

      See {!service} for a description of optional [~https],
      [~keep_nl_params], [~priority] and [~rt] parameters .  *)
  val delete_service :
    ?https:bool ->
    path:Eliom_lib.Url.path ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?priority:int ->
    rt:('rt, _) rt ->
    get_params:
      ('get, [< `WithSuffix | `WithoutSuffix ] as 'tipo,'gn)
        Eliom_parameter.params_type ->
    unit ->
    ('get, Eliom_parameter.raw_post_data,
     delete, a_s,
     non_co, non_ext,
     'tipo, 'gn,
     Eliom_parameter.no_param_name, reg, 'rt) service

  (** {3 Attached coservices} *)

  (** The function [coservice ~fallback ~get_params] creates an {%
      <<a_manual chapter="services"
      fragment="attached_coservices"|attached coservice>>%} at the
      same path than the service [fallback] and taking [get_params] as
      GET parameters.

      GET parameters of [coservice] couldn't contain a suffix
      parameter.

      The service [fallback] should be an (internal) attached service
      without any GET or POST parameters ; it could be a preapplied
      service.

      The optional [~name] parameter Coservices can be named if the
      [?name] optional parameter is present or anonymous (in that
      case, a coservice number will be generated).

      The optional [~timeout] parameter specifies a timeout (in
      seconds) after which the coservice will disappear. This amount
      of time is computed from the creation or from the last call to
      the service. The default is "no timeout". The optional
      [~max_use] parameter specifies that the service can be used only
      a fixed number of times. The default is "no limitation".

      If the optional [~csrf_safe] parameter is [true], it will create
      a {% <<a_manual chapter="security" fragment="csrf"|"CSRF-safe"
      service>>%}. In that case the [~name] parameter is ignored. The
      default is [false].

      The [~csrf_scope] and [~csrf_secure], if present, should
      respectively correspond to the [~scope] and [~secure] parameters
      that will be given to the associated [register]
      function. Otherwise the registration will fail with
      {Eliom_service.Wrong_session_table_for_CSRF_safe_coservice}. See
      {!Eliom_registration.Html5.register},
      {!Eliom_registration.App.register} or any other
      {!Eliom_registration}[.*.register] functions for a description
      of those parameters.

      See {!service} for a description of the optional [~https], [~rt]
      and [~keep_nl_params] parameters .  *)
  val coservice :
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [< Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    rt:('rt, _) rt ->
    fallback:
      (unit, unit, get, a_s, non_co, non_ext,
       [ `WithoutSuffix ] as 'tipo, unit, unit, _, 'rt)
        service ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    get_params:
      ('get,[`WithoutSuffix],'gn) Eliom_parameter.params_type ->
    unit ->
    ('get, unit, get, a_s, co, non_ext,
     'tipo, 'gn, unit, reg, 'rt) service

  (** The function [post_coservice ~fallback ~post_params] creates an
      {% <<a_manual chapter="services"
      fragment="attached_coservices"|attached coservice>>%} with the
      same path and GET parameters than the service [fallback] and
      taking [post_params] as POST parameters.

      POST parameters couldn't contain a suffix parameter.

      The service [fallback] should be an (internal) attached service
      or coservice without any POST parameters ; it could be a
      preapplied service.

      See {!coservice} for a description of optional parameters. *)
  val post_coservice :
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [< Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    rt:('rt, _) rt ->
    fallback:
      ('get, unit, get, a_s, _, non_ext,
       'tipo, 'gn, unit, reg, 'rt) service ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    post_params:
      ('post, [`WithoutSuffix], 'pn) Eliom_parameter.params_type ->
    unit ->
    ('get, 'post, post, a_s, co, non_ext, 'tipo, 'gn, 'pn, reg, 'rt)
      service

  (** The function [put_coservice ~fallback ~get_params] creates an {%
      <<a_manual chapter="services"
      fragment="attached_coservices"|attached coservice>>%} with the
      same path and GET parameters than the service [fallback] and
      taking a single POST parameter of type
      {!Eliom_parameter.raw_post_data}.

      The service [fallback] should be an (internal) attached PUT
      service or coservice without any GET parameters ; it could be a
      preapplied service.

      See {!coservice} for a description of optional parameters. *)
  val put_coservice :
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [< Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    rt:('rt, _) rt ->
    fallback:
      (unit, Eliom_parameter.raw_post_data, put, a_s, non_co, non_ext,
       [ `WithoutSuffix ], unit, Eliom_parameter.no_param_name, _, 'rt)
        service ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    get_params:
      ('get,[`WithoutSuffix],'gn) Eliom_parameter.params_type ->
    unit ->
    ('get, Eliom_parameter.raw_post_data,put,
     a_s, co, non_ext,
     [ `WithoutSuffix ], 'gn, Eliom_parameter.no_param_name,
     reg, 'rt) service

  (** The function [delete_coservice ~fallback ~get_params] creates an
      {% <<a_manual chapter="services"
      fragment="attached_coservices"|attached coservice>>%} with the
      same path and GET parameters than the service [fallback] and
      taking a single POST parameter of type
      {!Eliom_parameter.raw_post_data}.

      The service [fallback] should be an (internal) attached DELETE
      service or coservice without any GET parameters ; it could be a
      preapplied service.

      See {!coservice} for a description of optional parameters. *)
  val delete_coservice :
    ?name: string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [< Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    rt:('rt, _) rt ->
    fallback:
      (unit, Eliom_parameter.raw_post_data, delete,
       a_s, non_co, non_ext, [ `WithoutSuffix ],
       unit, Eliom_parameter.no_param_name,
       _, 'rt) service ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    get_params:
      ('get,[`WithoutSuffix],'gn) Eliom_parameter.params_type ->
    unit ->
    ('get, Eliom_parameter.raw_post_data,
     delete, a_s, co, non_ext,
     [ `WithoutSuffix ], 'gn, Eliom_parameter.no_param_name,
     reg, 'rt) service

  (** {3 Non attached coservices} *)

  (** The function [coservice' ~get_param] creates a {% <<a_manual
      chapter="services" fragment="non-attached_coservices"|non-attached
      coservice>>%} taking [get_params] as extra GET parameters.

      GET parameters of [coservice'] couldn't contain a suffix
      parameter.

      See {!service} for a description of the optional [~https], [~rt]
      and [~keep_nl_params] parameters ; see {!coservice} for others
      optional parameters.  *)
  val coservice' :
    ?name:string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [< Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    rt:('rt, _) rt ->
    get_params:
      ('get, [`WithoutSuffix], 'gn) Eliom_parameter.params_type ->
    unit ->
    ('get, unit, get, na_s,
     co, non_ext, [`WithoutSuffix],
     'gn, unit, reg, 'rt) service


  (** The function [post_coservice' ~post_params] creates a {%
      <<a_manual chapter="services"
      fragment="non-attached_coservices"|non-attached coservice>>%}
      taking [post_params] as POST parameters.

      POST parameters couldn't contain a suffix parameter.

      If the optional parameter [~keep_get_na_params] is [false], GET
      non-attached parameters of the current page won't be kept in the
      URL (if any) when you create a POST form to this coservice. The
      default is true.

      See {!service} for a description of the optional [~https], [~rt]
      and [~keep_nl_params] parameters ; see {!coservice} for others
      optional parameters.  *)
  val post_coservice' :
    ?name:string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [< Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?keep_get_na_params:bool ->
    rt:('rt, _) rt ->
    post_params:
      ('post, [`WithoutSuffix], 'pn) Eliom_parameter.params_type ->
    unit ->
    (unit, 'post,post, na_s,
     co, non_ext, [ `WithoutSuffix ],
     unit, 'pn, reg, 'rt) service

  (** The function [put_coservice' ~get_params] creates a {%
      <<a_manual chapter="services"
      fragment="non-attached_coservices"|non-attached coservice>>%}
      taking a single POST parameter of type
      {!Eliom_parameter.raw_post_data}.

      See {!service} for a description of the optional [~https], [~rt]
      and [~keep_nl_params] parameters ; see {!coservice} for others
      optional parameters.  *)
  val put_coservice' :
    ?name:string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [< Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    rt:('rt, _) rt ->
    get_params:
      ('get, [`WithoutSuffix], 'gn) Eliom_parameter.params_type ->
    unit ->
    ('get, Eliom_parameter.raw_post_data, put, na_s, co, non_ext,
     [`WithoutSuffix], 'gn, Eliom_parameter.no_param_name, reg, 'rt)
      service

  (** The function [delete_coservice' ~get_params] creates a {%
      <<a_manual chapter="services"
      fragment="non-attached_coservices"|non-attached coservice>>%}
      taking a single POST parameter of type
      {!Eliom_parameter.raw_post_data}.

      See {!service} for a description of the optional [~https], [~rt]
      and [~keep_nl_params] parameters ; see {!coservice} for others
      optional parameters.  *)
  val delete_coservice' :
    ?name:string ->
    ?csrf_safe: bool ->
    ?csrf_scope: [< Eliom_common.user_scope] ->
    ?csrf_secure: bool ->
    ?max_use:int ->
    ?timeout:float ->
    ?https:bool ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    rt:('rt, _) rt ->
    get_params:
      ('get, [`WithoutSuffix], 'gn) Eliom_parameter.params_type ->
    unit ->
    ('get, Eliom_parameter.raw_post_data, delete, na_s, co, non_ext,
     [`WithoutSuffix], 'gn, Eliom_parameter.no_param_name, reg, 'rt)
      service

  (** {2 External services} *)

  (** The function [external_service ~prefix ~path ~get_params ()]
      creates a service for an external web site, that will use GET
      method and requires [get_params] as parameters. This allows one to
      creates links or forms towards other Web sites using Eliom's
      syntax.

      The parameter labelled [~path] is the URL path. Each element of
      the list will be URL-encoded.

      The parameter labelled [~prefix] contains all what you want to put
      before the path. It usually starts with "http://" plus the name of
      the server. The prefix is not URL encoded.

      The whole URL is constructed from the prefix, the path and GET
      parameters. Hence, an empty prefix can be used to make a link to
      another site of the same server.

      See {!val:service} for a description of the optional
      [~keep_nl_params] and [~rt] parameters.  *)
  val external_service :
    prefix: string ->
    path:Eliom_lib.Url.path ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    rt:('rt, ext) rt ->
    get_params:
      ('get, [< `WithSuffix | `WithoutSuffix ] as 'tipo, 'gn)
        Eliom_parameter.params_type ->
    unit ->
    ('get, unit, get, a_s, non_co, ext,
     'tipo, 'gn, unit, non_reg, 'rt) service

  (** Same as {!external_service} but with POST method. *)
  val external_post_service :
    prefix: string ->
    path:Eliom_lib.Url.path ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    rt:('rt, ext) rt ->
    get_params:
      ('get, [< `WithSuffix | `WithoutSuffix ] as 'tipo, 'gn)
        Eliom_parameter.params_type ->
    post_params:
      ('post, [ `WithoutSuffix ], 'pn) Eliom_parameter.params_type ->
    unit ->
    ('get, 'post, post, a_s, non_co, ext, 'tipo,
     'gn, 'pn, non_reg, 'rt) service

  (** Same as {!external_service} but with PUT method. *)
  val external_put_service :
    prefix: string ->
    path:Eliom_lib.Url.path ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    rt:('rt, ext) rt ->
    get_params:
      ('get, [< `WithSuffix | `WithoutSuffix ] as 'tipo, 'gn)
        Eliom_parameter.params_type ->
    unit ->
    ('get, Eliom_parameter.raw_post_data,
     put, a_s, non_co, ext, 'tipo,
     'gn, Eliom_parameter.no_param_name, non_reg, 'rt)
      service

  (** Same as {!external_service} but with DELETE method. *)
  val external_delete_service :
    prefix: string ->
    path:Eliom_lib.Url.path ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    rt:('rt, ext) rt ->
    get_params:
      ('get, [< `WithSuffix | `WithoutSuffix ] as 'tipo, 'gn)
        Eliom_parameter.params_type ->
    unit ->
    ('get, Eliom_parameter.raw_post_data,
     delete, a_s, non_co, ext, 'tipo,
     'gn, Eliom_parameter.no_param_name, non_reg, 'rt)
      service

  (** {2 Predefined services} *)

  (** {3 Void non-attached coservices} *)

  (** The service [void_coservice'] is a predefined non-attached
      action with special behaviour: it has no parameter at all, even
      non-attached parameters.  Use it if you want to make a link to
      the current page without non-attached parameters.  It is almost
      equivalent to a POST non-attached service without POST
      parameters, on which you register an action that does nothing,
      but you can use it with <a> links, not only forms.  It does not
      keep non attached GET parameters.  *)
  val void_coservice' :
    (unit, unit, get, na_s, co, non_ext, [ `WithoutSuffix ],
     unit, unit, non_reg, _ non_ocaml)
      service

  (** Same as {!void_coservice'} but forcing https. *)
  val https_void_coservice' :
    (unit, unit, get, na_s, co, non_ext, [ `WithoutSuffix ],
     unit, unit, non_reg, _ non_ocaml)
      service

  (** Same as {!void_coservice'} but keeps non attached GET
  parameters. *)
  val void_hidden_coservice' :
    (unit, unit, get, na_s, co, non_ext, [ `WithoutSuffix ],
     unit, unit, non_reg, _ non_ocaml)
      service

  (** Same as {!void_hidden_coservice'} but forcing https. *)
  val https_void_hidden_coservice' :
    (unit, unit, get, na_s, co, non_ext, [ `WithoutSuffix ],
     unit, unit, non_reg, _ non_ocaml)
      service

  (** {3 Static files} *)

  (** The predefined service [static_dir] allows one to create links
      to static files. This service takes the name of a static file as
      a parameter (a string list, slash separated). The actual
      directory in filesystem where static pages will be found must be
      set up in the configuration file with the staticmod
      extension. *)
  val static_dir :
    unit ->
    (string list, unit, get, a_s, non_co, non_ext, [ `WithSuffix ],
     [ `One of string list ] Eliom_parameter.param_name,
     unit, non_reg, http non_ocaml)
      service

  (** Same as {!static_dir} but forcing https link. *)
  val https_static_dir :
    unit ->
    (string list, unit, get, a_s, non_co, non_ext, [ `WithSuffix ],
     [ `One of string list ] Eliom_parameter.param_name,
     unit, non_reg, http non_ocaml)
      service

  (** Like [static_dir], but allows one to put GET parameters *)
  val static_dir_with_params :
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    get_params:('a, [`WithoutSuffix], 'an) Eliom_parameter.params_type ->
    unit ->
    ((string list * 'a), unit, get, a_s, non_co, non_ext, [ `WithSuffix ],
     [ `One of string list ] Eliom_parameter.param_name *'an,
     unit, non_reg, http non_ocaml)
      service

  (** Same as {!static_dir_with_params} but forcing https link. *)
  val https_static_dir_with_params :
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    get_params:('a, [`WithoutSuffix], 'an) Eliom_parameter.params_type ->
    unit ->
    ((string list * 'a), unit, get, a_s, non_co, non_ext, [ `WithSuffix ],
     [ `One of string list ] Eliom_parameter.param_name *'an,
     unit, non_reg, http non_ocaml)
      service

  (** {2 Miscellaneous} *)

  (** The function [preapply ~service paramaters] creates a new
      service by preapplying [service] to the GET [parameters]. It is
      not possible to register a handler on an preapplied service ;
      preapplied services may be used in links or as fallbacks for
      coservices *)
  val preapply :
    service:
      ('a, 'b, 'meth, a_s, 'co, 'ext, _, 'e, 'f, 'g, 'return)
      service ->
    'a ->
    (unit, 'b, 'meth, a_s, 'co, 'ext,
     [ `WithoutSuffix ], unit, 'f, non_reg, 'return) service

  (** [attach_coservice' ~fallback ~service] attaches the non-attached
      coservice [service] on the URL of [fallback]. This allows to
      create a link to a non-attached coservice but with another URL
      than the current one. It is not possible to register something
      on the service returned by this function. *)
  val attach_coservice' :
    fallback:
      (unit, unit, get, a_s, _, non_ext,
       _, unit, unit, 'rg1, 'return1) service ->
    service:
      ('get, 'post, 'meth, na_s, co, non_ext,
       [< `WithoutSuffix] as 'sf, 'gn, 'pn, 'rg2, 'return) service ->
    ('get, 'post, 'meth, a_s, co, non_ext,
     'sf, 'gn, 'pn, non_reg, 'return) service

  (** The function [add_non_localized_get_parameters ~params ~service]
      Adds non localized GET parameters [params] to [service]. See the
      Eliom manual for more information about {% <<a_manual
      chapter="server-params" fragment="nonlocalizedparameters"|non
      localized parameters>>%}. *)
  val add_non_localized_get_parameters :
    params:
      ('p, [ `WithoutSuffix ], 'pn)
      Eliom_parameter.non_localized_params ->
    service:
      ('a, 'b, 'meth, 'attach, 'co, 'ext, 'd, 'e, 'f, 'g, 'return)
      service ->
    ('a * 'p, 'b, 'meth, 'attach, 'co, 'ext,
     'd, 'e * 'pn, 'f, 'g, 'return) service

  (** Same as {!add_non_localized_get_parameters} but with POST
      parameters.*)
  val add_non_localized_post_parameters :
    params:
      ('p, [ `WithoutSuffix ], 'pn)
      Eliom_parameter.non_localized_params ->
    service:
      ('a, 'b, 'meth, 'attach, 'co, 'ext,
       'd, 'e, 'f, 'g, 'return) service ->
    ('a, 'b * 'p, 'meth, 'attach, 'co, 'ext,
     'd, 'e, 'f * 'pn, 'g, 'return) service

  (**/**)

  val get_get_or_post :
    (_, _, 'm, _, _, _, _, _, _, _, _) service -> 'm service_method

  val get_info :
    (_, _, _, 'att, _, _, _, _, _, _, _) service ->
    'att attached_info

  val get_info_ :
    (_, _, _, _, _, _, _, _, _, _, _) service ->
    [ `Attached of a_s | `Nonattached of na_s ]

  val is_external : (_, _, _, _, _, _, _, _, _, _, _) service -> bool

  val get_get_params_type_ :
    ('a, _, _, _, _, _, 'b, 'c, _, _, _) service ->
    ('a, 'b, 'c) Eliom_parameter.params_type

  val get_post_params_type_ :
    (_, 'a, _, _, _, _, _, _, 'b, _, _) service ->
    ('a, [ `WithoutSuffix ], 'b) Eliom_parameter.params_type

  val get_sub_path_ : a_s -> Eliom_lib.Url.path

  val get_full_path_ : a_s -> Eliom_lib.Url.path

  val get_prefix_ :   a_s -> string

  val get_get_name_ : a_s -> Eliom_common.att_key_serv

  val get_post_name_ : a_s -> Eliom_common.att_key_serv

  val get_redirect_suffix_ : a_s -> bool

  val get_na_name_ : na_s -> Eliom_common.na_key_serv

  val get_na_keep_get_na_params_: na_s -> bool

  val get_max_use_ :
    (_, _, _, _, _, _, _, _, _, _, _) service -> int option

  val get_timeout_ :
    (_, _, _, _, _, _, _, _, _, _, _) service -> float option

  val get_https :
    (_, _, _, _, _, _, _, _, _, _, _) service -> bool

  val get_priority_ : a_s -> int

  val get_client_fun_ :
    ('a, 'b, _, _, _, _, _, _, _, _, _) service ->
    ('a -> 'b -> [ `Html ] Eliom_content_core.Html5.elt Lwt.t)
      Eliom_lib.client_value option

  val keep_nl_params :
    (_, _, _, _, _, _, _, _, _, _, _) service ->
    [ `All | `Persistent | `None ]

  val change_get_num :
    ('a, 'b, 'meth, a_s, 'co, 'ext, 'd, 'e, 'f, 'g, 'return) service ->
    a_s ->
    Eliom_common.att_key_serv ->
    ('a, 'b, 'meth, a_s, 'co, 'ext, 'd, 'e, 'f, 'i, 'return) service

  (* Not implemented on client side: TODO should not be called in
     Eliom_uri *)
  val register_delayed_get_or_na_coservice :
    sp:Eliom_common.server_params ->
    (int * [< Eliom_common.user_scope ] * bool option) ->
    string

  val register_delayed_post_coservice :
    sp:Eliom_common.server_params ->
    (int * [< Eliom_common.user_scope ] * bool option) ->
    Eliom_common.att_key_serv -> string

  (** Whether the service is capable to send application content or
      not. (application content has type
      Eliom_service.eliom_appl_answer: content of the application
      container, or xhr redirection ...).  A link towards a service
      with send_appl_content = XNever will always answer a regular
      http frame (this will stop the application if used in a regular
      link or form, but not with XHR).  XAlways means "for all
      applications" (like redirections/actions).  XSame_appl means
      "only for this application".  If there is a client side
      application, and the service has XAlways or XSame_appl when it
      is the same application, then the link (or form or change_page)
      will expect application content.  *)
  type send_appl_content =
    | XNever
    | XAlways
    | XSame_appl of string * string option
  (* used by eliommod_mkform *)

  (** Returns the name of the application to which belongs the
      service, if any. *)
  val get_send_appl_content :
    (_, _, _, _, _, _, _, _, _, _, _) service -> send_appl_content

  val xhr_with_cookies :
    (_, _, _, _, _, _, _, _, _, _, _) service -> string option option

end
