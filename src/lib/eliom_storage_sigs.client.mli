(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015 Vasilis Papavasileiou
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

module type STORAGE_RAW = sig

  type key

  val length : unit -> int
  val find : key -> string
  val set : key -> string -> unit
  val remove : key -> unit
  val clear : unit -> unit

end

module type STORAGE = sig

  type 'a key

  val length : unit -> int
  val find : 'a key -> 'a
  val set : 'a key -> 'a -> unit
  val remove : 'a key -> unit
  val clear : unit -> unit

end

module type CONV = sig

  type key
  type data

  val string_of_key : key -> string
  val string_of_data : data -> string
  val data_of_string : string -> data

end

module type TABLE = sig

  type ('a, 'b) t

  val find : ('a, 'b) t -> 'a -> 'b
  val set : ('a, 'b) t -> 'a -> 'b -> unit
  val remove : ('a, 'b) t -> 'a -> unit

end

module type ALL = sig
  module Raw : sig
    include STORAGE_RAW
    val create_key : string -> key
  end
  module Json : sig
    module Storage : sig
      include STORAGE
      val create_key : string -> 'a Deriving_Json.t -> 'a key
    end
    module Table : sig
      include TABLE
      val create :
        string -> 'a Deriving_Json.t -> 'b Deriving_Json.t -> ('a, 'b) t
    end
  end
  module Poly : sig
    module Storage : sig
      include STORAGE
      val create_key : string -> 'a key
    end
    module Table : sig
      include TABLE
      val create : string -> ('a, 'b) t
    end
  end
end
