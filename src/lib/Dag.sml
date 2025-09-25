structure Dag :
sig
  datatype node = N of int * node vector

  type dag = { root : node, leaves : node vector }

  (* The root basis has an id of 0. *)
  type t =
    (* potentially reduced *)
    { dag   : dag
    , full  : dag
    , bases : Basis.t vector
    , paths : string vector
    , dirty : BoolVector.vector
      (* raises on invalid path *)
    , getId : string -> int
    }

  datatype err = Cycle of string list

  exception Dag of err

  val errToString : (string -> string) -> err -> string

  type opts =
    { cache  : Cache.t option
    , logger : Log.logger option
    , reduce : bool
    }

  (* Traverse and reduce to a minimal equivalent the DAG formed by having the
   * given basis as root and all other bases which are transitively reachable
   * through basis file imports.
   * Declarations annotated with Discard are completely ignored,
   * as well as MLB files which match an enclosing IgnoreFiles annotation.
   * If a cycle is found, then a Dag exception is raised.
   * The given function takes in the absolute path of an mlb file and must
   * return its content.
   *)
  val process : opts -> (string -> Basis.t) -> string -> t
end =
struct
  structure A  = Array
  structure AS = ArraySlice
  structure BA = BoolArray
  structure H  = HashArray
  structure L  = List
  structure T  = Time
  structure V  = Vector

  datatype node = N of int * node vector

  type dag = { root : node, leaves : node vector }

  type t =
    { dag   : dag
    , full  : dag
    , bases : Basis.t vector
    , paths : string vector
    , dirty : BoolVector.vector
    , getId : string -> int
    }

  datatype err = Cycle of string list

  exception Dag of err

  fun errToString fmt (Cycle l) =
    concat
      ("error: mlb cycle:\n" :: List.concat (map (fn s => ["  ", s, "\n"]) l))

  type opts =
    { cache  : Cache.t option
    , logger : Log.logger option
    , reduce : bool
    }

  structure Buffer :>
  sig
    type 'a t

    val new : int * 'a -> 'a t
    val cnt : 'a t -> int

    val add : 'a t * 'a -> unit
    val sub : 'a t * int -> 'a
    val set : 'a t * int * 'a -> unit
    val clear : 'a t -> unit
    val addIfAbsent : ('a * 'a -> bool) -> 'a t * 'a -> unit

    val slice : 'a t -> 'a AS.slice
    val array : 'a t -> 'a array
    val vec   : 'a t -> 'a vector
  end =
  struct
    type 'a t = int ref * 'a * 'a array ref

    fun new (i, x) = (ref 0, x, (ref o A.array) (i, x))

    fun cnt (ref i, _, _) = i

    fun resize ((ri, x, ra as ref a), n) =
      let
        val a' = A.array (Int.max (A.length a * 2, n), x)
      in
        AS.copy { src = AS.slice (a, 0, SOME (!ri)), dst = a', di = 0 };
        ra := a';
        ()
      end

    fun add (t as (ri, _, ra), x) =
      ( if !ri = A.length (!ra) then resize (t, 1) else ()
      ; A.update (!ra, !ri, x)
      ; ri := !ri + 1
      )

    fun sub ((_, _, ref a), i) = A.sub (a, i)

    fun set (t as (ri, _, ra), i, x) =
      ( if i >= A.length (!ra) then resize (t, i + 1) else ()
      ; A.update (!ra, i, x)
      ; ri := Int.max (!ri, i) + 1
      )

    fun addIfAbsent eq (t as (ref i, _, ref a), x) =
      let
        fun f j = i <> j andalso (eq (x, A.sub (a, j)) orelse f (j + 1))
      in
        if f 0 then
          ()
        else
          add (t, x)
      end

    fun slice (ref i, _, ref a) = AS.slice (a, 0, SOME i)

    fun array (ref i, _, ref a) = A.tabulate (i, fn j => A.sub (a, j))

    fun vec z = AS.vector (slice z)

    fun clear (t as (ri, x, _)) = (AS.modify (fn _ => x) (slice t); ri := 0)
  end

  structure Set :>
  sig
    type t
    val new : int -> t
    val sub : t * int -> bool
    val set : t * int -> unit
    val del : t * int -> unit
    val clear : t -> unit
  end =
  struct
    type t = BA.array
    fun new i = BA.array (i, false)
    val sub = BA.sub
    fun set (a, i) = BA.update (a, i, true)
    fun del (a, i) = BA.update (a, i, false)
    val clear = BA.modify (fn _ => false)
  end

  structure DynSet :>
  sig
    type t
    val new : int -> t
    val sub : t * int -> bool
    val set : t * int -> unit
    val vec : t * int -> BoolVector.vector
  end =
  struct
    type t = BA.array ref

    fun new i = (ref o BA.array) (i, false)

    fun sub (ref a, i) = i < BA.length a andalso BA.sub (a, i)

    fun set (r, i) =
      ( if i >= BA.length (!r) then
          let
            val a = BA.array (Int.max (BA.length (!r) * 2, i + 1), false)
          in
            BA.copy { src = !r, dst = a, di = 0 };
            r := a
          end
        else
          ()
      ; BA.update (!r, i, true)
      )

    fun vec (ref a, i) = BoolVector.tabulate (i, fn i => BA.sub (a, i))
  end

  structure Matrix :>
  sig
    type t
    val new : int -> t
    val sub : t * int * int -> bool
    val set : t * int * int -> unit
    val del : t * int * int -> unit
    val clear : t -> unit
  end =
  struct
    (* use BoolArray over BoolArray2 because the latter does not seem to have
     * a packed representation
     *)
    type t = int * BA.array
    fun new i = (i, BA.array (i * i, false))
    fun sub ((c, a), i, j) = BA.sub (a, i * c + j)
    fun set ((c, a), i, j) = BA.update (a, i * c + j, true)
    fun del ((c, a), i, j) = BA.update (a, i * c + j, false)
    fun clear (_, a) = BA.modify (fn _ => false) a
  end

  structure B = Buffer
  structure D = DynSet
  structure S = Set
  structure M = Matrix

  fun index (l, s) =
    let
      fun idx ([], _) = ~1
        | idx (x::xs, i) = if x = s then i else idx (xs, i + 1)
    in
      idx (l, 0)
    end

  fun mtime (h, p) =
    case H.sub (h, p) of
      SOME t => t
    | NONE =>
        let
          val t = OS.FileSys.modTime p handle _ => T.now ()
        in
          H.update (h, p, t);
          t
        end

  datatype z = datatype Basis.dec
  datatype z = datatype Basis.exp

  val baseSize = 10

  (* Depth first so that any cycle found is the first one when reading
   * sequentially from the root.
   *)
  fun traverse (cache : Cache.t option, log, getBas, root) =
    let
      val bases : Basis.t B.t = B.new (baseSize * 2, [])
      val paths : string B.t = B.new (baseSize * 2, "")
      val dirty = D.new (baseSize * 2)
      val times : T.time B.t = B.new (baseSize * 2, T.now ())
      val mods  : T.time H.hash = H.hash (baseSize * 4)
      val ids   : int H.hash = H.hash (baseSize * 2)
      val deps  = B.new (baseSize, B.new (0, ~1))
      val revs  = B.new (baseSize, B.new (0, ~1))

      val op > = T.>

      fun doBas (p, ps) =
        let
          val ds = getBas p
          val id = B.cnt bases
          val sz = Int.max (id + 1, baseSize)
        in
          (* Check if basis is in cache and up to date. *)
          case cache of
            NONE => D.set (dirty, id)
          | SOME { time, ... } =>
              let
                val t = getOpt (time p, T.zeroTime)
                val t' = mtime (mods, p)
              in
                if t' > t then
                  D.set (dirty, id)
                else
                  B.set (times, id, t)
              end;
          (* Update bases, ids, etc. *)
          H.update (ids, p, id);
          B.add (paths, p);
          B.add (deps, B.new (sz, ~1));
          B.add (revs, B.new (sz, ~1));
          B.add (bases, ds);
          (* Traverse declarations. *)
          dec (ds, id, p::ps, []);
          (* Update with new basis content if dirty. *)
          case cache of
            NONE => ()
          | SOME { hasOrSet, set, ... } =>
              let
                val d = B.sub (deps, id)
                val z =
                  { id   = p
                  , bas  = ds
                  , deps = V.tabulate
                      (B.cnt d, fn i => B.sub (paths, B.sub (d, i)))
                  }
              in
                (* If already dirty, update the cache. *)
                if D.sub (dirty, id) then
                  set z
                (* Else check that the cache contains the exact same basis.
                 * If not, then update cache (during the check) and set dirty.
                 *)
                else if (not o hasOrSet) z then
                  D.set (dirty, id)
                else
                  ();
                Log.log log Log.Debug (fn fmt =>
                  fmt p ^ (if D.sub (dirty, id) then ": dirty" else ": clean"))
              end;
          (* Return the id. *)
          id
        end

      and dec ([], _, _, _) = ()
        | dec (Basis (_, e) :: ds, id, ps, is) =
            (exp (e, id, ps, is); dec (ds, id, ps, is))
        | dec (BasisFile p :: ds, id, ps, is) =
            if L.exists (fn p' => p = p') is then
              dec (ds, id, ps, is)
            else
              let
                val id' =
                  case H.sub (ids, p) of
                    SOME id =>
                      (case index (ps, p) of
                        ~1 => id
                      | i => raise (Dag o Cycle) (p :: (rev o L.take) (ps, i)))
                  | NONE => doBas (p, ps)
              in
                (* Set dirty if dep is dirty. *)
                if D.sub (dirty, id') then (D.set (dirty, id)) else ();
                B.addIfAbsent op= (B.sub (deps, id), id');
                B.addIfAbsent op= (B.sub (revs, id'), id);
                dec (ds, id, ps, is)
              end
        | dec (SourceFile p :: ds, id, ps, is) =
            ( if not (L.exists (fn p' => p = p') is)
                andalso (not o D.sub) (dirty, id)
                andalso mtime (mods, p) > B.sub (times, id)
              then
                (D.set (dirty, id))
              else
                ()
            ; dec (ds, id, ps, is)
            )
        | dec (Ann (l, ds') :: ds, id, ps, is) =
            ( if Ann.exists Ann.Discard l then
                ()
              else
                dec (ds', id, ps,
                  foldl
                    (fn (Ann.IgnoreFiles f, fs) => f @ fs | (_, fs) => fs)
                    is l)
            ; dec (ds, id, ps, is)
            )
        | dec (Local (ds1, ds2) :: ds, id, ps, is) =
            (dec (ds1, id, ps, is); dec (ds2, id, ps, is); dec (ds, id, ps, is))
        | dec (_::ds, id, ps, is) = dec (ds, id, ps, is)

      and exp (Bas ds, id, ps, is) = dec (ds, id, ps, is)
        | exp (Let (ds, e), id, ps, is) = (dec (ds, id, ps, is); exp (e, id, ps, is))
        | exp (Id _, _, _, _) = ()

    in
      doBas (root, [root]);

      { bases = B.vec bases
      , paths = B.vec paths
      , dirty = D.vec (dirty, B.cnt bases)
      , ids   = ids
      , deps  = deps
      , revs  = revs
      }
    end

  (* Hsu's algorithm for transitive reduction; "An algorithm for finding a
   * minimal equivalent graph of a digraph", ACM, 22(1):11-16.
   * See:
   *   https://projects.csail.mit.edu/jacm/References/hsu1975:11.html
   *   https://dl.acm.org/doi/10.1145/321864.321866
   *   https://stackoverflow.com/a/16357676
   *)
  local
    fun loop n f =
      let
        fun loop' i = if i = n then () else (f i; loop' (i + 1))
      in
        loop' 0
      end
  in
    fun reduce { sz, deps, revs } : unit =
      let
        val m = M.new sz
        val s = S.new sz

        fun init id =
          if (not o S.sub) (s, id) then
            ( S.set (s, id)
            ; (B.clear o B.sub) (revs, id)
            ; ( AS.app (fn id' => (M.set (m, id, id'); init id'))
              o B.slice
              o B.sub
              ) (deps, id)
            )
          else
            ()

        fun update id =
          if (not o S.sub) (s, id) then
            let
              val b = B.sub (deps, id)
              val l = AS.foldr
                (fn (id', ids) => if M.sub (m, id, id') then id'::ids else ids)
                [] (B.slice b)
            in
              S.set (s, id);
              B.clear b;
              app (fn id' =>
                ( B.add (b, id')
                ; B.add (B.sub (revs, id'), id)
                ; update id'
                )) l
            end
          else
            ()
      in
        (* construct edge matrix *)
        init 0;
        S.clear s;

        (* transform edge- into path matrix *)
        loop sz (fn i =>
          loop sz (fn j =>
            if i = j orelse (not o M.sub) (m, j, i) then
              ()
            else
              loop sz (fn k =>
                if (not o M.sub) (m, j, k) andalso M.sub (m, i, k) then
                  M.set (m, j, k)
                else
                  ())));

        (* unset unwanted edges *)
        loop sz (fn j =>
          loop sz (fn i =>
            if M.sub (m, i, j) then
              loop sz (fn k =>
                if M.sub (m, j, k) then
                  M.del (m, i, k)
                else
                  ())
            else
              ()));

        (* delete from the graph *)
        update 0
      end
  end

  local
    val dummy = N (~1, V.fromList [])
    fun isDummy (N (i, _)) = i = ~1
  in
    fun mkDag { sz, deps, revs } : dag =
      let
        val ndeps  = A.array (sz, dummy)
        val nrevs  = A.array (sz, dummy)
        val leaves = B.new (baseSize, ~1)

        fun dep id =
          if (not o isDummy o A.sub) (ndeps, id) then
            A.sub (ndeps, id)
          else
            let
              val n as N (_, v) =
                N ( id
                  , let
                      val b = B.sub (deps, id)
                    in
                      V.tabulate (B.cnt b, fn i => (dep o B.sub) (b, i))
                    end
                  )
            in
              A.update (ndeps, id, n);
              if V.length v = 0 then B.add (leaves, id) else ();
              n
            end

        fun rev id =
          if (not o isDummy o A.sub) (nrevs, id) then
            A.sub (nrevs, id)
          else
            let
              val n =
                N ( id
                  , let
                      val b = B.sub (revs, id)
                    in
                      V.tabulate (B.cnt b, fn i => (rev o B.sub) (b, i))
                    end
                  )
            in
              A.update (nrevs, id, n);
              n
            end
      in
        { root   = dep 0
        , leaves = V.tabulate (B.cnt leaves, fn i => (rev o B.sub) (leaves, i))
        }
      end
  end

  fun process { cache, logger, reduce = red } f s =
    let
      val log = Log.log logger Log.Debug
      fun parse s = (log (fn fmt => "parsing " ^ fmt s); f s)

      val { bases, paths, dirty, ids, deps, revs } =
        ( log (fn _ => "traversing MLB graph")
        ; traverse (cache, logger, parse, s)
        )
      val bs = { sz = V.length bases, deps = deps, revs = revs }
      val full = (log (fn _ => "building MLBgraph"); mkDag bs)
      val dag =
        if not red then
          full
        else
          ( log (fn _ => "reducing MLB graph")
          ; reduce bs
          ; log (fn _ => "building reduced MLB graph")
          ; mkDag bs
          )
    in
      { dag   = dag
      , full  = full
      , bases = bases
      , paths = paths
      , dirty = dirty
      , getId = fn s => (valOf o H.sub) (ids, s)
      }
    end
end
