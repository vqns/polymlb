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

  (* In memory cache. Safe to use across multiple threads and concurrent
   * compilation instances.
   *)
  val memory : unit -> t
end =
struct
  structure H = HashArray
  structure M = Thread.Mutex
  structure U = Universal

  type t =
    { time     : string -> Time.time option
    , hasOrSet : { id : string, bas : Basis.t, deps : string vector } -> bool
    , set      : { id : string, bas : Basis.t, deps : string vector } -> unit
    , fetch    : string -> NameSpace.t option
    , store    : string * Time.time * NameSpace.t -> NameSpace.t option
    }

  fun memory () : t =
    let
      val sz = 10
      val bases : Basis.t H.hash     = H.hash sz
      val times : Time.time H.hash   = H.hash sz
      val nss   : NameSpace.t H.hash = H.hash sz
      val m = M.mutex ()
      fun p f = ThreadLib.protect m f

      (* Tracking deps is not needed. *)
      fun set { id, bas, deps = _ } = H.update (bases, id, bas)
    in
      { time = fn id => p H.sub (times, id)
      , hasOrSet = fn (z as { id, bas, ... }) => p
          (fn () => H.sub (bases, id) = SOME bas orelse (set z; false)) ()
      , set = p set
      , fetch = fn id => p H.sub (nss, id)
      , store = fn (id, t, ns) => SOME ns before p
          (fn () => (H.update (nss, id, ns); H.update (times, id, t))) ()
      }
    end
end
