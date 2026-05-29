(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Thomas Del Vecchio, Jane Street, New York              *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Global = struct
  type 'a t = { global : 'a } [@@unboxed]
end

module Portable = struct
  type 'a t = { portable : 'a } [@@unboxed]
end

module Contended = struct
  type 'a t = { contended : 'a } [@@unboxed]
end

module Portended = struct
  type 'a t =
    { portended : 'a }
  [@@unboxed]
end

module Aliased = struct
  type 'a t = { aliased : 'a } [@@unboxed]
end

module Shared = struct
  type 'a t = { shared : 'a } [@@unboxed]
end

module Many = struct
  type 'a t = { many : 'a } [@@unboxed]
end

module Unyielding = struct
  type 'a t = { unyielding : 'a } [@@unboxed]
end
