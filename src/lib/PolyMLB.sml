structure PolyMLB :
sig
  datatype opt =
    AnnDefaults of Ann.t list
  | Cache of Cache.t
  | Concurrency of { depsFirst : bool, jobs : int }
    (* dummy values can be used; the anns will be completely disabled *)
  | DisabledAnns of Ann.t list
  | Logger of Log.logger
    (* in addition to the default path map *)
  | PathMap of string HashArray.hash
  | Preprocess of { bas : Basis.t, path : string, root : bool } -> Basis.t

  type opts = opt list

  (* Default path map *)
  val pathMap : string HashArray.hash

  (* Compile the given basis and return the resulting namespace.
   * May raise one of the following:
   * - Lex.Lex;
   * - Parse.Parse;
   * - Basis.Validation;
   * - Dag.Dag;
   * - Compile.Compile;
   * - IO.Io.
   *)
  val compile : opts -> string -> NameSpace.t

  (* Compile and import the content of the basis in the global namespace.
   * See `compile` for errors.
   *)
  val import : opts -> string -> unit

  (* Compile and import an mlb file, much like the top level `use`. *)
  val use : string -> unit
end =
struct
  structure H   = HashArray
  structure OSF = OS.FileSys
  structure OSP = OS.Path

  datatype opt =
    AnnDefaults of Ann.t list
  | Cache of Cache.t
  | Concurrency of { depsFirst : bool, jobs : int }
  | DisabledAnns of Ann.t list
  | Logger of Log.logger
  | PathMap of string HashArray.hash
  | Preprocess of { bas : Basis.t, path : string, root : bool } -> Basis.t

  type opts = opt list

  (* http://mlton.org/MLBasisPathMap
   * https://github.com/MLton/mlton/blob/master/mlton/control/control-flags.sml#L1636
   * - int, word and real can be deduced from precision / wordSize / radix
   * - target_arch from PolyML.architecture ()
   * - target_os from ?
   *   there is LibrarySupport.getOSType but it is not available
   *   val getOSCall: unit -> int = RunCall.rtsCallFast0 "PolyGetOSType"
   *   val getOS: int = getOSCall() -> 0 for Posix, 1 -> Windows
   *)
  val pathMap : string H.hash = H.hash 10

  fun readFile p =
    let
      val s = TextIO.openIn p
    in
      TextIO.inputAll s before TextIO.closeIn s
    end

  local
    fun find f l v =
      let
        fun fd [] = NONE
          | fd (x::xs) = case f x of NONE => fd xs | z => z
      in
        Option.getOpt (fd l, v)
      end

    fun pp { bas : Basis.t, path = _ : string, root = _ : bool } = bas
  in
    fun doOpts opts =
      let
        fun fd f v = find f opts v
      in
        { anns     = fd (fn AnnDefaults l => SOME l | _ => NONE) []
        , cache    = fd (fn Cache c => SOME (SOME c) | _ => NONE) NONE
        , conc     = fd (fn Concurrency z => SOME z | _ => NONE)
            { depsFirst = false, jobs = 1 }
        , dAnns    = fd (fn DisabledAnns l => SOME l | _ => NONE) []
        , logger   = fd (fn Logger l => SOME (SOME l) | _ => NONE) NONE
        , pathMap  = fd (fn PathMap m => SOME m | _ => NONE) (H.hash 1)
        , preproc  = fd (fn Preprocess f => SOME f | _ => NONE) pp
        }
      end
  end

  fun doBasis f opts src =
    let
      val opts as { cache, conc, logger, ... } = doOpts opts
      val copts =
        { depsFirst = #depsFirst conc, jobs = #jobs conc, logger = logger }

      val src = OSF.fullPath src
        handle e =>
          ( case logger of
              SOME { print, ... } => print (Log.Error, fn () => exnMessage e)
            | _ => ()
          ; PolyML.Exception.reraise e
          )

      val pathMap =
        let
          val h : string H.hash = H.hash 10
        in
          H.fold (fn (k, v, _) => H.update (h, k, v)) () pathMap;
          H.fold (fn (k, v, _) => H.update (h, k, v)) () (#pathMap opts);
          h
        end

      fun convOpts p =
        { disabledAnns = #dAnns opts
        , exts = NONE
        , logger = logger
        , pathMap = pathMap
        , path = OSP.dir p
        }

      fun mkBas p =
        ( (fn b =>
            if p = src then
              #preproc opts { bas = b, path = p, root = true }
            else
              #preproc opts { bas = b, path = p, root = false })
        o Basis.fromParse (convOpts p)
        o Parse.parse p
        o Lex.lex p
        o readFile
        ) p
    in
      ( f copts
      o Dag.process { logger = logger, reduce = true } mkBas
      ) src
      handle e =>
        ( Log.log logger Log.Error
            (fn fmt =>
               case e of
                Lex.Lex z          => Lex.errToString     fmt z
              | Parse.Parse z      => Parse.errToString   fmt z
              | Basis.Validation z => Basis.errToString   fmt z
              | Dag.Dag z          => Dag.errToString     fmt z
              | Compile.Compile z  => Compile.errToString fmt z
              | _                  => exnMessage e)
        ; PolyML.Exception.reraise e
        )
    end

  fun compile opts = doBasis Compile.compile opts

  fun import opts src =
    NameSpace.import { src = compile opts src, dst = NameSpace.global }

  local
    fun fmt p = OSP.mkRelative { path = p, relativeTo = (OSF.getDir ()) }
    fun log (Log.Warn, m) = print ("warning: " ^ m () ^ "\n")
      | log (Log.Error, m) = print ("error: " ^ m () ^ "\n")
      | log _ = ()
  in
    fun use s =
      NameSpace.import
        { src = compile [Logger { pathFmt = fmt, print = log }] s
        , dst = NameSpace.global
        }
      handle _ => raise Fail "Static errors"
  end
end


structure PolyMLB =
struct
  structure Ann = Ann
  structure Basis = Basis
  structure Cache = Cache
  structure Compile = Compile
  structure Dag = Dag
  structure Lex = Lex
  structure Log = Log
  structure NameSpace = NameSpace
  structure Parse = Parse
  structure Path = Path
  open PolyMLB
end
