(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
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

include Eliom_service_base

let xhr_with_cookies s =
  if is_external s then
    None
  else
    match s.send_appl_content with
    | XAlways -> Some None
    | XNever -> None
    | XSame_appl (appl, _) when appl <> Eliom_process.get_application_name () ->
      None
    | XSame_appl (_, tmpl) -> Some tmpl

let has_client_fun service = client_fun service <> None

let reload_fun :
  type gp pp .
  (gp, pp, _, _, _, _, _, _, _, _, _) t ->
  (gp -> unit -> unit Lwt.t) option =
  fun service ->
    match Eliom_parameter.is_unit (post_params_type service) with
    | Eliom_parameter.U_yes ->
      (match service with
       | { client_fun = Some f ; reload_fun = Rf_client_fun } ->
         Some f
       | _ ->
         None)
    | _ ->
      None

let register_delayed_get_or_na_coservice ~sp s =
  failwith "CSRF coservice not implemented client side for now"

let register_delayed_post_coservice  ~sp s getname =
  failwith "CSRF coservice not implemented client side for now"
