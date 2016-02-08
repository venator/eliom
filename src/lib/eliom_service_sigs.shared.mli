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

  type ('a, 'e) rt

  type a_s
  type na_s

  type ('get,'post,+'meth,+'attached,+'co,+'ext,
        +'tipo,'gn,'pn,+'reg,+'ret) service
    constraint 'meth = [< `Get | `Post | `Put | `Delete ]
    constraint 'attached = [< `Attached of a_s | `Nonattached of na_s]
    constraint 'co = [< `Co | `Non_co ]
    constraint 'ext = [< `Ext | `Non_ext ]
    constraint 'tipo = [< `WithSuffix | `WithoutSuffix ]
    constraint 'reg = [< `Registrable | `Unregistrable ]

  type http
  type appl

  type 'a ocaml
  type 'a non_ocaml

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
    ('get,unit,[> `Get],[> `Attached of a_s ], [> `Non_co ], [> `Non_ext ],
     'tipo, 'gn, unit, [> `Registrable ], 'rt) service

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
      ('get, unit, [< `Get], [< `Attached of a_s],
       'co, [< `Non_ext ],
       [< `WithSuffix | `WithoutSuffix] as 'tipo, 'gn, unit,
       [< `Registrable ], 'rt) service ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?priority:int ->
    post_params:
      ('post, [`WithoutSuffix], 'pn) Eliom_parameter.params_type ->
    unit ->
    ('get, 'post, [> `Post], [> `Attached of a_s], 'co, [< `Non_ext],
     'tipo, 'gn, 'pn, [> `Registrable ], 'rt) service

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
     [> `Put],[> `Attached of a_s],[> `Non_co ], [> `Non_ext], 'tipo, 'gn,
     Eliom_parameter.no_param_name, [> `Registrable ], 'rt) service

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
     [> `Delete], [> `Attached of a_s],
     [> `Non_co ], [> `Non_ext],
     'tipo, 'gn,
     Eliom_parameter.no_param_name, [> `Registrable ], 'rt) service

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
      (unit, unit, [<`Get], [<`Attached of a_s],
       [< `Non_co ], [< `Non_ext ],
       [ `WithoutSuffix ] as 'tipo, unit, unit,
       [< `Registrable | `Unregistrable ], 'rt) service ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    get_params:
      ('get,[`WithoutSuffix],'gn) Eliom_parameter.params_type ->
    unit ->
    ('get, unit, [> `Get], [> `Attached of a_s], [> `Co ], [> `Non_ext ],
     'tipo, 'gn, unit, [> `Registrable ], 'rt) service

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
      ('get, unit, [<`Get], [<`Attached of a_s],
       _, [< `Non_ext ],
       [< `WithSuffix | `WithoutSuffix ] as 'tipo,
       'gn, unit, [< `Registrable ], 'rt) service ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    post_params:
      ('post, [`WithoutSuffix], 'pn) Eliom_parameter.params_type ->
    unit ->
    ('get, 'post, [>`Post], [> `Attached of a_s],
     [> `Co ], [> `Non_ext ], 'tipo, 'gn, 'pn, [> `Registrable ], 'rt)
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
      (unit,
       Eliom_parameter.raw_post_data,
       [`Put],
       [`Attached of a_s],
       [ `Non_co ], [ `Non_ext ],
       [ `WithoutSuffix ] as 'tipo,
       unit, Eliom_parameter.no_param_name,
       [< `Registrable | `Unregistrable ], 'rt) service ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    get_params:
      ('get,[`WithoutSuffix],'gn) Eliom_parameter.params_type ->
    unit ->
    ('get, Eliom_parameter.raw_post_data,[> `Put],
     [> `Attached of a_s], [> `Co ], [> `Non_ext ],
     'tipo, 'gn, Eliom_parameter.no_param_name,
     [> `Registrable ], 'rt) service

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
      (unit, Eliom_parameter.raw_post_data,[`Delete],
       [`Attached of a_s], [ `Non_co ], [ `Non_ext ],
       [ `WithoutSuffix ] as 'tipo,
       unit, Eliom_parameter.no_param_name,
       [< `Registrable | `Unregistrable ], 'rt) service ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    get_params:
      ('get,[`WithoutSuffix],'gn) Eliom_parameter.params_type ->
    unit ->
    ('get, Eliom_parameter.raw_post_data,
     [>`Delete], [> `Attached of a_s], [> `Co ], [> `Non_ext ],
     'tipo, 'gn, Eliom_parameter.no_param_name,
     [> `Registrable ], 'rt) service

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
    ('get, unit, [> `Get], [> `Nonattached of na_s],
     [> `Co], [> `Non_ext ], [`WithoutSuffix],
     'gn, unit, [> `Registrable ], 'rt) service


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
    (unit, 'post,[>`Post],[> `Nonattached of na_s],
     [> `Co ], [> `Non_ext ], [ `WithoutSuffix ],
     unit, 'pn, [> `Registrable ], 'rt) service

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
    ('get, Eliom_parameter.raw_post_data, [>`Put],
     [> `Nonattached of na_s], [> `Co ], [> `Non_ext ],
     [`WithoutSuffix], 'gn,
     Eliom_parameter.no_param_name, [> `Registrable ], 'rt) service

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
    ('get, Eliom_parameter.raw_post_data, [> `Delete],
     [> `Nonattached of na_s], [> `Co ], [> `Non_ext ],
     [`WithoutSuffix], 'gn,
     Eliom_parameter.no_param_name, [> `Registrable ], 'rt) service

end

module type S_with_external = sig

  include S

  type ext

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
    ('get, unit, [>`Get],[> `Attached of a_s],
     [> `Non_co], [> `Ext ],
     'tipo, 'gn, unit, [> `Unregistrable ], 'rt) service

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
    ('get, 'post, [>`Post],[> `Attached of a_s], [> `Non_co ], [> `Ext ], 'tipo,
     'gn, 'pn, [> `Unregistrable ], 'rt) service

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
     [> `Put], [> `Attached of a_s], [> `Non_co ], [> `Ext ], 'tipo,
     'gn, Eliom_parameter.no_param_name, [> `Unregistrable ], 'rt)
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
     [>`Delete ], [> `Attached of a_s], [> `Non_co ], [> `Ext ], 'tipo,
     'gn, Eliom_parameter.no_param_name, [> `Unregistrable ], 'rt)
      service

end
