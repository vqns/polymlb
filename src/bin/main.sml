structure H   = HashArray
structure P   = PolyMLB
structure PC  = PolyML.Compiler
structure OSP = OS.Process
structure S   = String
structure SS  = Substring
structure TIO = TextIO

datatype cmd = CompileLink | Compile | SmlLib

type opts =
  { cmd      : cmd
  , defAnns  : P.Ann.t list
  , depsf    : bool
  , disAnns  : P.Ann.t list
  , file     : string
  , jobs     : int
  , main     : string
  , out      : string
  , pathMap  : string H.hash
  , polyc    : string
  , rootAnns : P.Ann.t list
  , verbose  : int
  }

local
  fun f str s =
    if S.isSuffix "\n" s then
      TIO.output (str, s)
    else
      TIO.output (str, s ^ "\n")
in
  val println = f TIO.stdOut
  val eprintln = f TIO.stdErr
  fun die "" = OSP.exit OSP.failure
    | die msg = (eprintln msg; OSP.exit OSP.failure)
end

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
, "   -default-ann <ann>             Set annotation default"
, "   -deps-first                    Ensure MLB files will only be compiled after"
, "                                  their dependencies"
, "   -disable-ann <ann>             Disable the given annotation"
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
, "-q -quiet                         Equivalent to -verbose 1"
, "-Q -reallyquiet                   Equivalent to -verbose 0"
, "   -sml-lib                       Print the resolved value of $(SML_LIB)"
, "-v -verbose <n>                   Set verbosity level"
, "-V -version                       Print PolyMLB version"
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
    { cmd      = ref CompileLink
    , defAnns  = ref ([] : P.Ann.t list)
    , depsf    = ref false
    , disAnns  = ref ([] : P.Ann.t list)
    , file     = ref ""
    , jobs     = ref 1
    , main     = ref "main"
    , out      = ref ""
    , pathMap  = ref (H.hash 10 : string H.hash)
    , polyc    = ref "polyc"
    , rootAnns = ref ([] : P.Ann.t list)
    , verbose  = ref 2
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

  fun ann (_, s) [] = req s
    | ann (field, s) (x::xs) =
        case P.Ann.parse x of
          P.Ann.Ann a => xs before field d := a :: !(field d)
        | _ => inv (s, x)

  fun annName (_, s) [] = req s
    | annName (field, s) (x::xs) =
        case P.Ann.parseName x of
          NONE => inv (s, x)
        | SOME a => xs before field d := a :: !(field d)

  fun posInt s =
    case Int.fromString s of
      NONE => NONE
    | SOME i => if i < 0 then NONE else SOME i
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
              | "-ann" => l := ann (#rootAnns, "-ann") xs
              | "-c" => #cmd d := Compile
              | "-compile" => #cmd d := Compile
              | "-default-ann" => l := ann (#defAnns, "default-ann") xs
              | "-deps-first" => #depsf d := true
              | "-disable-ann" => l := annName (#disAnns, "disable-ann") xs
              | "-h" => help ()
              | "-help" => help ()
              | "-info" => info ()
              | "-ignore-call-main" =>
                  #rootAnns d := P.Ann.IgnoreFiles ["call-main.sml"]
                  :: !(#rootAnns d)
              | "-ignore-main" =>
                  #rootAnns d := P.Ann.IgnoreFiles ["main.sml"]
                  :: !(#rootAnns d)
              | "-jobs" => l := set (xs, "-jobs", #jobs, posInt)
              | "-main" => l := set (xs, "-main", #main, SOME)
              | "-mlb-path-map" => l := map xs
              | "-mlb-path-var" => l := var xs
              | "-o" => l := set (xs, "-o", #out, SOME)
              | "-output" => l := set (xs, "-output", #out, SOME)
              | "-polyc" => l := set (xs, "-p", #polyc, SOME)
              | "-q" => #verbose d := 1
              | "-quiet" => #verbose d := 1
              | "-Q" => #verbose d := 0
              | "-reallyquiet" => #verbose d := 0
              | "-sml-lib" => #cmd d := SmlLib
              | "-v" => l := set (xs, "-v", #verbose, posInt)
              | "-verbose" => l := set (xs, "-verbose", #verbose, posInt)
              | "-V" => version ()
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
        { cmd      = !(#cmd d)
        , defAnns  = !(#defAnns d)
        , depsf    = !(#depsf d)
        , disAnns  = !(#disAnns d)
        , file     = !(#file d)
        , jobs     = !(#jobs d)
        , main     = !(#main d)
        , out      = out
        , pathMap  = !(#pathMap d)
        , polyc    = !(#polyc d)
        , rootAnns = !(#rootAnns d)
        , verbose  = !(#verbose d)
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

  fun log v (l, m) = if v >= P.Log.levelToInt l then println (m ()) else ()

  fun fmt p =
    let
      val cwd = OS.FileSys.getDir ()
    in
      if S.isPrefix cwd p then
        OS.Path.mkRelative { path = p, relativeTo = cwd }
      else
        p
    end

  fun o2o ({ defAnns, depsf, disAnns, jobs, pathMap, rootAnns, verbose, ... } : opts) =
    let
      val l =
        [ P.PathMap pathMap
        , P.Concurrency { depsFirst = depsf, jobs = jobs }
        , P.DisabledAnns disAnns
        ]
      val l =
        if verbose = 0 then
          l
        else
          P.Logger { pathFmt = fmt, print = log verbose } :: l
      val l =
        P.Preprocess
          (fn { bas, root = true, ... } =>
              let
                val anns = defAnns @ rootAnns
              in
                if null anns then bas else [P.Basis.Ann (anns, bas)]
              end
            | { bas, ... } =>
              if null defAnns then bas else [P.Basis.Ann (defAnns, bas)])
        :: l
    in
      l
    end
in
  fun doCompile (opts as { file, ... } : opts) =
    P.compile (o2o opts) file handle _ => (die ""; raise Fail "")
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
          val str = TIO.openString ("export (\"" ^ out ^ "\", main);")
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
    val opts as { cmd, ... } = parseArgs ()
  in
    (case cmd of
      CompileLink => compileLink
    | Compile => compile
    | SmlLib => printSmlLib) opts
  end
