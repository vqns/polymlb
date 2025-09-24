structure Compile :
sig
  datatype err =
    Compilation of string * PolyML.location
  | Dependency  of string (* depsFirst invariant violated; means bad input *)
  | Execution   of string * exn
  | UnboundId   of string

  exception Compile of err

  val errToString : (string -> string) -> err -> string

  type opts =
    { cache     : Cache.t option
    , depsFirst : bool
    , jobs      : int
    , logger    : Log.logger option
    }

  (* Resolve and compile a list of declarations in a fresh env, triggering
   * side effects from top level declarations.
   * Does not handle IO.Io and raises Compile on non IO error.
   * single job and depsFirst = false is guaranteed to be encounter order.
   * The logger may be called from different threads if multiple jobs.
   *)
  val compile : opts -> Dag.t -> NameSpace.t
end =
struct
  structure A    = Array
  structure BA   = BoolArray
  structure BV   = BoolVector
  structure D    = Dag
  structure FTP  = ThreadPools.FTP
  structure H    = HashArray
  structure L    = Log
  structure M    = Thread.Mutex
  structure NS   = NameSpace
  structure P    = PolyML
  structure PC   = PolyML.Compiler
  structure PTP  = ThreadPools.PTP
  structure TIO  = TextIO
  structure TSIO = TIO.StreamIO
  structure V    = Vector

  datatype err =
    Compilation of string * P.location
  | Dependency  of string
  | Execution   of string * exn
  | UnboundId   of string

  exception Compile of err

  fun errToString fmt k =
    concat
      [ case k of
          Compilation (_, at) => Log.locFmt fmt at ^ ": "
        | Execution (f, _) => fmt f ^ ": "
        | _ => ""
      , "error: "
      , case k of
          Compilation (s, _) => s
        | Dependency s => "dependency invariant violated: " ^ s
        | Execution (_, e) => "raised during execution: " ^ exnMessage e
        | UnboundId s => "unbound id: " ^ s
      ]

  type opts =
    { cache     : Cache.t option
    , depsFirst : bool
    , jobs      : int
    , logger    : Log.logger option
    }

  fun compileSML log (ns, path, opts) =
    let
      val msg = ref ([] : string list)
      val loc = ref
        { file = ""
        , startLine = 0, startPosition = 0
        , endLine = 0, endPosition = 0
        }

      fun msgCb { message, hard, location, ... } =
        if hard then
          ( P.prettyPrint (fn s => msg := s :: !msg, 80) message
          ; loc := location
          )
        else
          L.log log L.Warn
            (fn fmt =>
              let
                val m = ref ([] : string list)
              in
                P.prettyPrint (fn s => m := s :: !m, 80) message;
                concat (L.locFmt fmt location :: ": " :: rev (!m))
              end)

      val s = (ref o TIO.getInstream o TIO.openIn) path
      val l = ref 1

      fun getc () =
        case TSIO.input1 (!s) of
          NONE => NONE
        | SOME (c as #"\n", s') => (l := !l + 1; s := s'; SOME c)
        | SOME (c, s') => (s := s'; SOME c)

      val opts =
        [ PC.CPErrorMessageProc msgCb
        , PC.CPLineNo (fn () => !l)
        , PC.CPFileName path
        , PC.CPNameSpace ns
        ] @ opts
    in
      while (not o TSIO.endOfStream o !) s do
        let
          val f = P.compiler (getc, opts)
            handle _ =>
              ( TSIO.closeIn (!s)
              ; raise
                  (Compile o Compilation)
                  ((String.concat o List.rev o !) msg, !loc)
              )
        in
          f ()
            handle e =>
              ( TSIO.closeIn (!s)
              ; raise (Compile o Execution) (path, e)
              )
        end;
      TSIO.closeIn (!s)
    end

  fun anns l =
    let
      fun f (Ann.Debug b, opts) = PC.CPDebug b :: opts
        | f (_, opts) = opts
    in
      List.foldr f [] l
    end

  fun isIgnored (l, p) =
    let
      val p = OS.Path.file p
    in
      List.exists (fn p' => p' = p) l
    end

  (* compileBas elaborates a given Basis.t (= a list of declarations). It also
   * takes in a free form argument ('a), typically the basis id in a dag.
   * When it encounters a Basis import (BasisFile), it returns the path, the
   * free argument as well as a continuation function which takes in a namespace
   * and will elaborates the remaining declarations. When it reaches the end,
   * it returns the complete namespace.
   * This allows to completely delegate the scheduling and MLB caching to
   * driver functions, which can process through the dag as needed.
   *)

  datatype 'a r = Done of 'a * NS.t | Cont of 'a * string * 'a cont
  withtype 'a cont = NS.t -> 'a r

  local
    datatype z = datatype Basis.dec
    datatype z = datatype Basis.exp
  in
    fun compileBas log ret ds =
      let
        (* addendum to the above about compileBas: declaration scopes (e.g
         * annotations or local/in) are also implemented with continuations,
         * as well as passing the current namespace as argument.
         *)
        fun elab (opts, ign) (ns as NS.N (bns, pns), ds, cont) =
          let
            val elab' = elab (opts, ign)

            fun dec ds =
              case (Thread.Thread.testInterrupt (); ds) of
                [] => cont ns
              | Basis (b, e) :: ds =>
                  exp (e, ns, fn ns' => (#enterBas bns (b, ns'); dec ds))
              | BasisFile p :: ds =>
                if isIgnored (ign, p) then
                  dec ds
                else
                  Cont (ret, p, fn ns' =>
                    (NS.import { src = ns', dst = ns }; dec ds))
              | SourceFile p :: ds =>
                  if isIgnored (ign, p) then
                    dec ds
                  else
                    ( L.log log L.Trace (fn fmt => "compiling " ^ fmt p)
                    ; compileSML log (pns, p, opts)
                    ; dec ds
                    )
              | Ann (l, ds') :: ds =>
                  if Ann.exists Ann.Discard l then
                    dec ds
                  else
                    let
                      val ign' =
                        List.foldl
                          (fn (Ann.IgnoreFiles f, fs) => f @ fs | (_, fs) => fs)
                          ign l

                      val ns' =
                        if Ann.exists Ann.ImportAll l then
                          let
                            val { loc, pub } = NS.delegates ns
                          in
                            NS.import { src = NS.all, dst = loc };
                            pub
                          end
                        else
                          ns
                    in
                      elab (anns l @ opts, ign') (ns', ds', fn _ => dec ds)
                    end
              | Local (ds1, ds2) :: ds =>
                  let
                    val { loc, pub } = NS.delegates ns
                  in
                    elab' (loc, ds1, fn _ => elab' (pub, ds2, fn _ => dec ds))
                  end
              | Open b :: ds =>
                  ( case #lookupBas bns b of
                      NONE => raise Compile (UnboundId b)
                    | SOME ns' => NS.import { src = ns', dst = ns }
                  ; dec ds
                  )
              | Structure (new, old) :: ds =>
                  ( case #lookupStruct pns old of
                      NONE => raise Compile (UnboundId old)
                    | SOME s => #enterStruct pns (new, s)
                  ; dec ds
                  )
              | Signature (new, old) :: ds =>
                  ( case #lookupSig pns old of
                      NONE => raise Compile (UnboundId old)
                    | SOME s => #enterSig pns (new, s)
                  ; dec ds
                  )
              | Functor (new, old) :: ds =>
                  ( case #lookupFunct pns old of
                      NONE => raise Compile (UnboundId old)
                    | SOME f => #enterFunct pns (new, f)
                  ; dec ds
                  )

            and exp (e, ns as NS.N (bns, _), cont) =
              case e of
                Bas ds =>
                  let
                    val ns' = NS.empty ()
                    val { loc, pub } = NS.delegates ns'
                  in
                    NS.import { src = ns, dst = loc };
                    elab' (pub, ds, fn _ => cont ns')
                  end
             | Id b =>
                  (case #lookupBas bns b of
                    NONE => raise Compile (UnboundId b)
                  | SOME ns => cont ns)
              | Let (ds, e) =>
                  let
                    val ns' = NS.empty ()
                    val { loc, pub } = NS.delegates ns'
                  in
                    NS.import { src = ns, dst = loc };
                    elab' (loc, ds, fn _ => exp (e, pub, fn _ => cont ns'))
                  end
          in
            dec ds
          end
      in
        elab ([], []) (NS.empty (), ds, fn ns => Done (ret, ns))
      end
  end

  (* All driver functions are passed in a cache manager as well as a namespace
   * array, which will contain the namespaces resulting from MLB elaboration
   * and is indexed by dag ids.
   *)

  structure CacheManager :>
  sig
    type t
    val new : Cache.t option * L.logger option * D.t -> t
    val fetch : t * int -> NS.t option
    val store : t * int * NS.t -> NS.t
  end =
  struct
    type t =
      { cache : Cache.t
      , log   : L.logger option
      , paths : string vector
      , dirty : BV.vector
      } option

    fun new (NONE, _, _) = NONE
      | new (SOME c, l, { paths, dirty, ... } : D.t) =
          SOME { cache = c, log   = l, paths = paths, dirty = dirty }

    fun fetch (NONE : t, _) = NONE
      | fetch (SOME { cache = { fetch, ... }, log, dirty, paths }, id) =
          if BV.sub (dirty, id) then
            NONE
          else
            let
              val p = V.sub (paths, id)
            in
              L.log log L.Debug (fn fmt => "fetching " ^ fmt p ^ " from cache");
              (fetch o V.sub) (paths, id)
            end

    fun store (NONE : t, _, ns) = ns
      | store (SOME { cache = { store, ... }, log, dirty, paths }, id, ns) =
          (* Cache the namespace only if it's new *)
          if (not o BV.sub) (dirty, id) then
            ns
          else
            let
              val p = V.sub (paths, id)
            in
              L.log log L.Debug (fn fmt => "caching " ^ fmt p);
              case store (V.sub (paths, id), Time.now (), ns) of
                SOME ns => ns
              | _ => ns
            end
  end

  structure NameSpaceArray :>
  sig
    type t
    (* whether to make threadsafe *)
    val new  : Dag.t * bool -> t
    val sub  : t * int -> NS.t
    val sub' : t * string -> NS.t
    val get  : t * int -> NS.t option
    val set  : t * int * NS.t -> unit
  end =
  struct
    type t =
      { a : NS.t option array
      , m : M.mutex option
      , paths : string vector
      , getId : (string -> int)
      }

    fun new ({ paths, getId, ... } : Dag.t, b) =
      { m = if b then (SOME o M.mutex) () else NONE
      , a = A.array (V.length paths, NONE)
      , paths = paths
      , getId = getId
      }

    (* double checked locking? *)
    val lock = Option.app M.lock
    val unlock = Option.app M.unlock

    fun sub ({ a, m, paths, ... } : t, i) =
      case (lock m; A.sub (a, i) before unlock m) of
        NONE => raise (Compile o Dependency o V.sub) (paths, i)
      | SOME ns => ns

    fun sub' (t, s) = sub (t, #getId t s)

    fun get ({ a, m, ... } : t, i) = (lock m; A.sub (a, i) before unlock m)

    fun set ({ a, m, paths, ... } : t, i, ns) =
      if (lock m; (isSome o A.sub) (a, i)) then
        ( unlock m
        ; raise Fail
            ("Compile.NameSpaceArray.set: illegal set for " ^ V.sub (paths, i))
        )
      else
        A.update (a, i, SOME ns) before unlock m
  end

  structure CM  = CacheManager
  structure NSA = NameSpaceArray

  fun logElab log p = L.log log L.Info (fn fmt => "elaborating " ^ fmt p)

  (* Driver functions. *)

  fun serialDeps (cm, log, nsa, { full, bases, paths, ... } : D.t) =
    let
      fun cont (Done (id, ns)) = NSA.set (nsa, id, CM.store (cm, id, ns))
        | cont (Cont (_, p, f)) = (cont o f o NSA.sub') (nsa, p)

      fun comp (D.N (id, deps)) =
        if (isSome o NSA.get) (nsa, id) then
          ()
        else
          case CM.fetch (cm, id) of
            SOME ns => NSA.set (nsa, id, ns)
          | NONE =>
              ( V.app comp deps
              ; (logElab log o V.sub) (paths, id)
              ; (cont o compileBas log id o V.sub) (bases, id)
              )
     in
      comp (#root full);
      NSA.sub (nsa, 0)
    end

  fun serialEncounter (cm, log, nsa, { bases, paths, getId, ... } : D.t) =
    let
      fun cont (Done (id, ns)) =
            let
              val ns = CM.store (cm, id, ns)
            in
              ns before NSA.set (nsa, id, ns)
            end
        | cont (Cont (_, p, f)) =
            let
              val id = getId p
            in
              case NSA.get (nsa, id) of
                SOME ns => cont (f ns)
              | NONE =>
                  case CM.fetch (cm, id) of
                    SOME ns => cont (f ns)
                  | NONE =>
                      ( logElab log p
                      ; (cont o f o cont o compileBas log id o V.sub)
                          (bases, id)
                      )
            end
    in
      (logElab log o V.sub) (paths, 0);
      (cont o compileBas log 0 o V.sub) (bases, 0)
    end

  fun parDeps jobs (cm, log, nsa, { full, bases, paths, dirty, ... } : D.t) =
    let
      val started = BA.array (V.length bases, false)
      val counts  = A.tabulate (V.length bases, fn _ => (M.mutex (), ref ~1))
      val tp      = FTP.new jobs

       fun doCounts (D.N (id, deps)) =
        let
          val (_, r) = A.sub (counts, id)
        in
          if !r > ~1 then () else (r := V.length deps; V.app doCounts deps)
        end

      fun cont (Done (id, ns)) = NSA.set (nsa, id, CM.store (cm, id, ns))
        | cont (Cont (_, p, f)) = (cont o f o NSA.sub') (nsa, p)

      fun elab id =
        ( (logElab log o V.sub) (paths, id)
        ; (cont o compileBas log id o V.sub) (bases, id)
        )

      fun postComp (n as D.N (id, _)) =
        let
          val (m, r) = A.sub (counts, id)
        in
          M.lock m;
          r := !r - 1;
          if !r <= 0 before M.unlock m then
            FTP.submit (tp, fn () => comp n)
          else
            ()
        end

      (* No lock on started since it's only accessed from the original thread. *)
      and comp (D.N (id, revs)) =
        if BA.sub (started, id) then
          ()
        else
          ( BA.update (started, id, true)
          ; if BV.sub (dirty, id) then
              (* If dirty, then submit elab job. *)
              FTP.submit (tp, fn () => (elab id; V.app postComp revs))
            else if V.exists (fn D.N (i, _) => BV.sub (dirty, i)) revs then
              (* Else if any of the revdeps is dirty, then attempt to fetch
               * from cache.
               *)
              FTP.submit
                (tp, fn () =>
                  ( case CM.fetch (cm, id) of
                      SOME ns => NSA.set (nsa, id, ns)
                    | NONE => elab id
                  ; V.app postComp revs
                  ))
            else
              V.app postComp revs
          )
    in
      doCounts (#root full);
      V.app comp (#leaves full);
      case FTP.wait tp of
        NONE => NSA.sub (nsa, 0)
      | SOME e => PolyML.Exception.reraise e
    end

  fun parConc jobs (cm, log, nsa, { full, bases, paths, dirty, getId, ... } : D.t) =
    let
      type c    = FixedInt.int * (FixedInt.int * int) cont
      val m     = M.mutex ()
      val conts = A.tabulate (V.length bases, fn _ => ([] : c list))
      val prios = A.array (V.length bases, ~1 : FixedInt.int)
      val tp    = PTP.new jobs

      fun doPrio i (D.N (id, deps)) =
        let
          val j = A.sub (prios, id)
        in
            if j < i then
              ( A.update (prios, id, i)
              ; if j = ~1 then V.app (doPrio (i + 1)) deps else ()
              )
            else
              ()
        end

      fun cont (Done ((_, id), ns)) = postComp (id, CM.store (cm, id, ns))
        | cont (Cont ((prio, _), p, f)) =
            let
              val id = getId p
            in
              case NSA.get (nsa, id) of
                SOME ns => cont (f ns)
              | NONE =>
                  ( M.lock m
                  ; A.update (conts, id, (prio, f) :: A.sub (conts, id))
                  ; M.unlock m
                  )
            end

      and postComp (id, ns) =
        ( NSA.set (nsa, id, ns)
        ; app
            (fn (prio, f) => PTP.submit (tp, (prio, fn () => cont (f ns))))
            (A.sub (conts, id))
        )

      fun elab (prio, id) =
        ( (logElab log o V.sub) (paths, id)
        ; (cont o compileBas log (prio, id) o V.sub) (bases, id)
        )

      (* No lock on prios since it's only accessed from the original thread. *)
      fun comp (D.N (id, revs)) =
        case A.sub (prios, id) of
          ~1 => ()
        | prio =>
            ( A.update (prios, id, ~1)
            ; if BV.sub (dirty, id) then
                (* If dirty, then submit elab job. *)
                PTP.submit (tp, (prio, fn () => elab (prio, id)))
              else if V.exists (fn D.N (i, _) => BV.sub (dirty, i)) revs then
                (* Else if any of the revdeps is dirty, then attempt to fetch
                 * from cache.
                 *)
                PTP.submit (tp, (prio, fn () =>
                  case CM.fetch (cm, id) of
                    SOME ns => postComp (id, ns)
                  | NONE => elab (prio, id)))
              else
                ()
            ; V.app comp revs
            )
    in
      doPrio 0 (#root full);
      V.app comp (#leaves full);
      case PTP.wait tp of
        NONE => (print "here\n"; NSA.sub (nsa, 0))
      | SOME e => PolyML.Exception.reraise e
    end

  fun numJobs j =
    if j <= 0 then
      Thread.Thread.numProcessors ()
    else
      Int.min (j, Thread.Thread.numProcessors ())

  fun compile { cache, depsFirst, jobs, logger } dag =
    let
      val jobs = numJobs jobs
      val cm = CM.new (cache, logger, dag)
    in
      case CM.fetch (cm, 0) of
        SOME ns => ns
      | NONE =>
          (case (jobs, depsFirst) of
            (1, true) => serialDeps
          | (1, _)    => serialEncounter
          | (n, true) => parDeps n
          | (n, _)    => parConc n) (cm, logger, NSA.new (dag, jobs > 1), dag)
  end
end
