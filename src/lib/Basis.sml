structure Basis :
sig
  (* mirrors the MLB grammar except for repeating rules e.g `dec := <dec> <dec>`,
   * for which lists are used instead, and `<dec> and <dec>`, which are made as
   * as many separate declarations. As a consequence, `<dec> and `dec`> bindings
   * are assumed to be correct, i.e invalid bindings such as
   * `structure Foo = Bar and Foo = Baz`  are to be checked earlier (i.e in
   * `fromParse`).
   * Notes:
   * - file paths are absolute and without variables
   *)
  datatype dec =
    Basis of string * exp
  | BasisFile of string
  | SourceFile of string
  | Ann of Ann.t list * dec list
  | Local of dec list * dec list
  | Open of string
  | Structure of string * string
  | Signature of string * string
  | Functor of string * string

  and exp =
    Bas of dec list
  | Id of string
  | Let of dec list * exp

  type t = dec list

  type opts =
    { disabledAnns : Ann.t list
    (* for SML source files only, replaces default.
     * ["sml", "ml", "ML", "sig", "fun"]
     *)
    , exts         : string list option
    , logger       : Log.logger option
    , pathMap      : string HashArray.hash
    (* absolute path to a directory from which to resolve relative paths *)
    , path         : string
    }

  datatype err_kind = DuplicateBind | Extension | UnboundVariable

  type err = err_kind * string * PolyML.location

  exception Validation of err

  val errToString : (string -> string) -> err -> string

  (* Validate parse results and convert to their Basis counterpart.
   * Will propagate exceptions that were raised during validation.
   * The following operations are performed:
   * - invalid annotatons are removed and the declarations they contained are
   *   inlined;
   * - declarations annotated with `Discard` are discarded;
   * - files whose name is to be ignored are discarded;
   * - paths are resolved and validated (variables and extensions);
   * - duplicate binds are checked;
   * - operation lists are inlined, e.g `open bas1 bas2` or
   *   `structure S1 = S2 and S3 = S4` become two disctint declarations;
   * - completely empty local/in ([], []) are discard;
   * - public declarations of a local/in with empty local are inlined;
   * - the expression of a let/in with an empty let is inlined.
   *)
  val fromParse : opts -> Parse.t -> t
end =
struct
  structure CV = CharVector
  structure H  = HashArray
  structure L  = List
  structure P  = Parse
  structure S  = String

  datatype dec =
    Basis of string * exp
  | BasisFile of string
  | SourceFile of string
  | Ann of Ann.t list * dec list
  | Local of dec list * dec list
  | Open of string
  | Structure of string * string
  | Signature of string * string
  | Functor of string * string

  and exp =
    Bas of dec list
  | Id of string
  | Let of dec list * exp

  type t = dec list

  type opts =
    { disabledAnns : Ann.t list
    , exts         : string list option
    , logger       : Log.logger option
    , pathMap      : string HashArray.hash
    , path         : string
    }

  datatype err_kind = DuplicateBind | Extension | UnboundVariable

  type err = err_kind * string * PolyML.location

  exception Validation of err

  fun errToString fmt (kind, s, at) =
    concat
      [ Log.locFmt fmt at, ": error: "
      , case kind of
          DuplicateBind   => "rebound identifier"
        | Extension       => "invalid file extension"
        | UnboundVariable => "unbound path var"
      , ": '", s, "'"
      ]

  datatype FileType = MLB | SML

  local
    val baseExts = ["sml", "ml", "ML", "fun", "sig"]
  in
    fun ftype (s, exts, loc) =
      case OS.Path.ext s of
        NONE => raise Validation (Extension, s, loc)
      | SOME "mlb" => MLB
      | SOME e =>
          if L.exists (fn e' => e' = e) (getOpt (exts, baseExts)) then
            SML
          else
            raise Validation (Extension, s, loc)
  end

  fun process (m, exts, ignored, p) (s, loc) =
    let
      val s' = OS.Path.file s
    in
      if L.exists (fn x => x = s') ignored then
        NONE
      else
        let
          val t = ftype (s, exts, loc)
          val path =
            case Path.process m s of
              Path.Path p => OS.Path.mkCanonical p
            | Path.Unbound v => raise Validation (UnboundVariable, v, loc)
        in
          SOME (t, OS.Path.mkAbsolute { path = path, relativeTo = p })
        end
    end

  fun mapCheck (f, l, loc) =
    #2 (L.foldl
      (fn ((x1, x2), (xs, r)) =>
        if L.exists (fn x' => x' = x1) xs then
          raise Validation (DuplicateBind, x1, loc)
        else
          (x1::xs, f (x1, x2) :: r))
      ([], []) l)

  fun badAnn log (loc, a, r) = Log.log log Log.Warn
    (fn fmt => concat
      (Log.locFmt fmt loc :: ": " ::
        (case r of
          Ann.BadArg z => ["bad argument '", z, "' for ann '", a, "'"]
        | Ann.MissingArg => ["missing argument for ann '", a, "'"]
        | Ann.UnexpectedArg => ["unexpected arg for ann '", a, "'"]
        | Ann.Unrecognized => ["unrecognized ann '", a, "'"]
        | Ann.Ann _ => raise Fail "Basis.badAnn: impossible")))

  fun annCheck (xs, dis, cb, loc) =
    let
      fun f ([], r, p) = (L.rev r, p)
        | f (x::xs, r, p) =
            case Ann.parse x of
              Ann.Ann a =>
                if Ann.exists a dis then
                  f (xs, r, p)
                else
                  (case a of
                    Ann.IgnoreFiles l => f (xs, a::r, l @ p)
                  | _ => f (xs, a::r, p))
            | z => (cb (loc, x, z); f (xs, r, p))
    in
      f (xs, [], [])
    end

  fun fromParse { disabledAnns, logger, pathMap, path, exts } =
    let
      fun conv ignored ds =
        let
          val path = process (pathMap, exts, ignored, path)

          fun dec ((P.Basis l, loc), ds) =
                mapCheck (fn (s, e) => Basis (s, exp e), l, loc) @ ds
            | dec ((P.File p, loc), ds) =
                (case path (p, loc) of
                  NONE => ds
                | SOME (MLB, p) => BasisFile p :: ds
                | SOME (SML, p) => SourceFile p :: ds)
            | dec ((P.Ann (l, ds'), loc), ds) =
                (case annCheck (l, disabledAnns, badAnn logger, loc) of
                  ([], _) => L.foldl dec ds ds'
                | (l, p) =>
                    if Ann.exists Ann.Discard l then
                      ds
                    else
                      Ann (l, conv (p @ ignored) ds') :: ds)
            | dec ((P.Local (ds1, ds2), _), ds) =
                (case (conv ignored ds1, conv ignored ds2) of
                  ([], []) => ds
                | ([], l2) => L.revAppend (l2, ds)
                | (l1, l2) => Local (l1, l2) :: ds)
            | dec ((P.Open l, _), ds) =
                foldl (fn (s, l) => Open s :: l) [] l @ ds
            | dec ((P.Structure l, loc), ds) =
                mapCheck (Structure, l, loc) @ ds
            | dec ((P.Signature l, loc), ds) =
                mapCheck (Signature, l, loc) @ ds
            | dec ((P.Functor l, loc), ds) =
                mapCheck (Functor, l, loc) @ ds

          and exp (P.Bas ds, _) =
                Bas (conv ignored ds)
            | exp (P.Id s, _) =
                Id s
            | exp (P.Let (ds, e), _) =
                case conv ignored ds of
                  [] => exp e
                | l => Let (l, exp e)
        in
          L.rev (L.foldl dec [] ds)
        end
    in
      conv []
    end
end
