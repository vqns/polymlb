structure H   = HashArray
structure P   = PolyMLB
structure PC  = PolyML.Compiler
structure OSP = OS.Process
structure S   = String
structure SS  = Substring
structure TIO = TextIO

datatype cmd = CompileLink | Compile | SmlLib

type opts =
  { anns     : P.Ann.t list
  , cmd      : cmd
  , depsf    : bool
  , file     : string
  , jobs     : int
  , main     : string
  , out      : string
  , pathMap  : string H.hash
  , polyc    : string
  , pSuccess : bool
  , quiet    : bool
  }

local
  fun name () = CommandLine.name () ^ ": "
  fun f str s =
    ( TIO.output (str, s)
    ; if not (S.isSuffix "\n" s) then TIO.output (str, "\n") else ()
    ; TIO.flushOut str
    )
in
  val println = f TIO.stdOut
  val eprintln = f TIO.stdErr
  fun eprintln' s = (TIO.output (TIO.stdErr, name ()); eprintln s)
end

fun die msg = eprintln' msg before OSP.exit OSP.failure

fun success () = OSP.exit OSP.success

(* todo: PolyML.export automatically adds an extension, which is ".o" on every
 * platform except Windows proper (e.g Cygwin results in ".o")
 * see: libpolyml/exporter.cpp:780:exportNative
 *)
fun objExt () = ".o"

local
  fun usage () = die ("usage: " ^ CommandLine.name () ^ " [OPTIONS] [--] FILE")

  fun help () =
    app println
[ "usage: " ^ CommandLine.name () ^ " [OPTIONS] [--] FILE"
, "Compile and link an MLBasis file with Poly/ML."
, ""
, "OPTIONS"
, "   -ann <ann>                     Wrap FILE with the given annotation"
, "-c -compile                       Compile but do not link"
, "   -deps-first                    Ensure MLB files will only be compiled after"
, "                                  their dependencies"
, "-h -help                          Print help usage"
, "   -info                          Print advanced information"
, "   -ignore-call-main              Equivalent to -ann 'ignoreFiles call-main.sml'"
, "   -ignore-main                   Equivalent to -ann 'ignoreFiles main.sml'"
, "   -jobs <n>                      Maximum number of jobs to run simultaneously"
, "   -mlb-path-map <file>           Additional MLB path map"
, "   -mlb-path-var '<name> <value>' Additional MLB path var"
, "   -main <name>                   Root function to export"
, "-o -output <file>                 Name of output file"
, "   -polyc <polyc>                 Polyc executable instead of 'polyc'"
, "   -print-out                     Print out file name to stdout if success"
, "-q -quiet                         Silence warnings"
, "   -sml-lib                       Print the resolved value of $(SML_LIB)"
, "-v -version                       Print PolyMLB version"
] before success ()

  val VERSION =
    (String.concatWith "." o map Int.toString)
    [VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH]

  fun version () =
    println ("PolyMLB " ^ VERSION ^ " (Poly/ML " ^  PC.compilerVersion ^ ")")
    before success ()

  local
    val l = getOpt (H.sub (PolyMLB.pathMap, "SML_LIB"), "")
    val msg = S.concat
      [   "polymlb: ", VERSION
      , "\npoly:    ", PC.compilerVersion
      , "\nSML_LIB: ", if l = "" then "unset" else "\"" ^ l ^ "\""
      ]
  in
    fun info () = println msg before success ()
  end

  val d =
    { anns     = ref ([] : P.Ann.t list)
    , cmd      = ref CompileLink
    , depsf    = ref false
    , file     = ref ""
    , jobs     = ref 1
    , main     = ref "main"
    , out      = ref ""
    , pathMap  = ref (H.hash 10 : string H.hash)
    , polyc    = ref "polyc"
    , pSuccess = ref false
    , quiet    = ref false
    }

  fun req s = [] before die ("missing required argument for option " ^ s)
  fun inv (s, v) =
    [] before die ("invalid value for option " ^ s ^ ": '" ^ v ^ "'")

  fun strs s =
    let
      val (ss1, ss2) =
        SS.splitl
          (not o Char.isSpace)
          ((SS.dropr Char.isSpace o SS.dropl Char.isSpace o SS.full) s)
    in
      (SS.string ss1, (SS.string o SS.dropl Char.isSpace) ss2)
    end

  fun set ([], s, _, _) = (req s; [])
    | set (x::xs, s, f, p) =
        (case p x of
          NONE => (inv (s, x); [])
        | SOME v => (f d := v; xs))

  fun var [] = req "-mlb-path-var"
    | var (x::xs) =
        case strs x of
          ("", _) => inv ("-mlb-path-var", x)
        | (_, "") => inv ("-mlb-path-var", x)
        | (k, v) => xs before H.update (!(#pathMap d), k, v)

  fun map [] = req "-mlb-path-map"
    | map (x::xs) =
        let
          val s = TIO.openIn x
            handle _ => (inv ("-mlb-path-var", x); TIO.stdIn)
          val l = ref 0
          fun e () =
            die (x ^ ":" ^ Int.toString (!l) ^ ": invalid path map entry")
          fun f () =
            case (l := !l + 1; TIO.inputLine s) of
              NONE => TIO.closeIn s
            | SOME l =>
                (case strs l of
                  ("", _) => e ()
                | (_, "") => e ()
                | (k, v) => H.update (!(#pathMap d), k, v) before f ())
        in
          xs before f () before TIO.closeIn s
        end

  fun ann [] = req "-ann"
    | ann (x::xs) =
        case P.Ann.parse x of
          NONE => inv ("-ann", x)
        | SOME a => xs before #anns d := a :: !(#anns d)

  fun jobs [] = req "-jobs"
    | jobs (x::xs) =
        case Int.fromString x of
          NONE => inv ("-jobs", x)
        | SOME i =>
            if i < 0 then
              inv ("-jobs", x)
            else
              xs before #jobs d := i
in
  fun parseArgs () : opts =
    let
      val l = ref (CommandLine.arguments ())

      fun f () =
        case !l of
          [] => ()
        | (x::xs) =>
            ( l := xs
            ; case x of
                "--" => ((#file d := hd xs) handle Empty => usage ())
              | "-ann" => l := ann xs
              | "-c" => #cmd d := Compile
              | "-deps-first" => #depsf d := true
              | "-h" => help ()
              | "-help" => help ()
              | "-info" => info ()
              | "-ignore-call-main" =>
                  #anns d := P.Ann.IgnoreFiles ["call-main.sml"] :: !(#anns d)
              | "-ignore-main" =>
                  #anns d := P.Ann.IgnoreFiles ["main.sml"] :: !(#anns d)
              | "-jobs" => l := jobs xs
              | "-main" => l := set (xs, "-main", #main, SOME)
              | "-mlb-path-map" => l := map xs
              | "-mlb-path-var" => l := var xs
              | "-o" => l := set (xs, "-o", #out, SOME)
              | "-output" => l := set (xs, "-output", #out, SOME)
              | "-polyc" => l := set (xs, "-p", #polyc, SOME)
              | "-print-out" => #pSuccess d := true
              | "-q" => #quiet d := true
              | "-quiet" => #quiet d := true
              | "-sml-lib" => #cmd d := SmlLib
              | "-v" => version ()
              | "-version" => version ()
              | s =>
                  if S.isPrefix "-" s then
                    die ("invalid option: " ^ x)
                  else
                    (#file d := x; raise Fail "")
            ; f ()
            )

      val _ = f () handle Fail "" => ()

      val out =
        if !(#out d) = "" then
          (OS.Path.base o ! o #file) d
          ^ (case !(#cmd d) of
              (* ".exe" on windows? *)
              CompileLink => ""
            | Compile => objExt ()
            | SmlLib => "")
        else
          !(#out d)

      val opts as { file, ... } : opts =
        { anns     = !(#anns d)
        , cmd      = !(#cmd d)
        , depsf    = !(#depsf d)
        , file     = !(#file d)
        , jobs     = !(#jobs d)
        , main     = !(#main d)
        , out      = out
        , pathMap  = !(#pathMap d)
        , polyc    = !(#polyc d)
        , pSuccess = !(#pSuccess d)
        , quiet    = !(#quiet d)
        }
    in
      if #cmd opts = SmlLib then
        opts
      else if file = "" then
        opts before usage ()
      else if (not o List.null o !) l then
        opts before die "only one input file allowed"
      else if not (S.isSuffix ".mlb" file) then
        opts before die ("invalid extension: " ^ file)
      else
        opts
    end
end

local
  open OS.FileSys

  fun searchDir (s, p) =
    let
      fun f d =
        case readDir d of
          NONE => NONE before closeDir d
        | SOME s' =>
            if s = s' then
              (closeDir d; SOME (OS.Path.joinDirFile { dir = p, file = s }))
            else
              f d
    in
      f (openDir p) handle _ => NONE
    end

  fun searchPath s =
    let
      fun f [] = NONE
        | f (d::ds) = case searchDir (s, d) of NONE => f ds | z => z
    in
      Option.mapPartial (f o S.tokens (fn c => c = #":")) (OSP.getEnv "PATH")
    end
in
  fun cmdExists s =
    if CharVector.exists (fn c => c = #"/") s then
      access (s, [A_EXEC])
    else
      case searchPath s of
        SOME s => access (s, [A_EXEC])
      | NONE => false
end

local
  structure P = PolyMLB

  val warn = eprintln' o P.warnToStringd

  fun o2o ({ anns, depsf, jobs, pathMap, quiet, ... } : opts) =
    let
      val l =
        [ P.PathMap pathMap
        , P.CompileOpts { jobs = jobs, depsFirst = depsf, copts = [] }
        ]
      val l = if not quiet then (P.WarningHandler warn)::l else l
      val l =
        if List.null anns then
          l
        else
          (P.PreprocessRoot (fn b => [P.Basis.Ann (anns, b)])) :: l
    in
      l
    end

  fun err _ = ()
in
  fun doCompile (opts as { file, ... } : opts) : P.NameSpace.t =
    case P.compile (o2o opts) file of
      P.Ok ns => ns
    | P.Error e => (die (PolyMLB.errToStringd e); raise Fail "")
end

local
  fun getVal (ns, id) =
    let
      fun f (_, []) = NONE
        | f (ns, [x]) = #lookupVal ns x
        | f (ns, x::xs) =
            case #lookupStruct ns x of
              NONE => NONE
            | SOME s => f (PolyML.NameSpace.Structures.contents s, xs)
    in
      f (ns, S.tokens (fn c => c = #".") id)
    end

  val msg = ref ([] : string list)
  fun mkMsg { message, hard, ... } =
    if hard then
      PolyML.prettyPrint (fn s => msg := s :: !msg, 80) message
    else
      ()

  val exportFn = (valOf o getVal) (PolyML.globalNameSpace, "PolyML.export")
in
  fun export (out, root, P.NameSpace.N (_, ns)) =
    case getVal (ns, root) of
      NONE =>
        die ("error: cannot export '" ^ root ^ "': value has not been declared")
    | SOME mainFn =>
        let
          val str = TIO.openString ("val _ = export (\"" ^ out ^ "\", main)")
          val P.NameSpace.N (_, ns') = P.NameSpace.empty ()
        in
          #enterVal ns' ("main", mainFn);
          #enterVal ns' ("export", exportFn);
          PolyML.compiler
            ( fn () => TIO.input1 str
            , [PC.CPNameSpace ns', PC.CPErrorMessageProc mkMsg]
            ) ()
        end
        handle _ =>
          (die o S.concat)
            (["error: cannot export '", root, "': "] @ List.rev (!msg))
end

fun compile (opts as { main, out, ... }) = export (out, main, doCompile opts)

local
  fun link (polyc, obj, out) =
    if (OSP.isSuccess o OSP.system o S.concatWith " ") [polyc, "-o", out, obj]
    then
      ()
    else
      die ("error invoking " ^ polyc)
in
  fun compileLink (opts as { file, main, out, polyc,  ... }) =
    if not (cmdExists polyc) then
      die ("command not found: " ^ polyc)
    else
      let
        val obj = file ^ objExt ()
      in
        export (obj, main, doCompile opts);
        link (polyc, obj, out);
        OS.FileSys.remove obj
      end
end

fun printSmlLib ({ pathMap, ... } : opts) =
  ( H.fold (fn (k, v, ()) => H.update (PolyMLB.pathMap, k, v)) () pathMap
  ; case P.Path.process PolyMLB.pathMap "$(SML_LIB)" of
      P.Path.Path s => println s before success ()
    | P.Path.Unbound v => die ("unbound path var: " ^ v)
  )

fun main () =
  let
    val opts as { cmd, out, pSuccess, ... } = parseArgs ()
  in
    (case cmd of
      CompileLink => compileLink
    | Compile => compile
    | SmlLib => printSmlLib) opts;
    if pSuccess then
      print (out ^ "\n")
    else
      ()
  end
