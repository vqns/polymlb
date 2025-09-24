structure Cache :
sig
  (* Cache ids are full paths to MLB files. *)
  type t =
    (* Returns the time passed in the last call to store with the given id. *)
    { time     : string -> Time.time option
    (* Returns true if the given id is associated with the given basis;
     * otherwise updates the mapping and returns false.
     *)
    , hasOrSet : { id : string, bas : Basis.t, deps : string vector } -> bool
    , set      : { id : string, bas : Basis.t, deps : string vector } -> unit
    , fetch    : string -> NameSpace.t option
    , store    : string * Time.time * NameSpace.t -> NameSpace.t option
    }
end =
struct
  type t =
    { time     : string -> Time.time option
    , hasOrSet : { id : string, bas : Basis.t, deps : string vector } -> bool
    , set      : { id : string, bas : Basis.t, deps : string vector } -> unit
    , fetch    : string -> NameSpace.t option
    , store    : string * Time.time * NameSpace.t -> NameSpace.t option
    }
end
