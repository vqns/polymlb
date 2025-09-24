structure C = PolyMLB.Compile
structure N = PolyML.NameSpace

datatype z = datatype PolyMLB.Basis.dec

fun compile b () = C.compile
  { depsFirst = false, jobs = 0, logger = NONE }
  (PolyMLB.Dag.process { cache = NONE, logger = NONE, reduce = true }
    (fn "root" => [Ann ([PolyMLB.Ann.ImportAll], b)]
      | _ => raise Fail "")
    "root");

"Compile.compile raises on invalid sml source"
assert
  compile [SourceFile "sml/source/illegal.sml"]
raisesMatching
  (fn C.Compile (C.Compilation _) => true | _ => false);

"Compile.compile re-raises execution exns"
assert
  compile [SourceFile "sml/source/bad-exec.sml"]
raisesMatching
  (fn C.Compile (C.Execution _) => true | _ => false);

"Compile.compile raises on invalid bind"
assert
  compile [Structure ("S1", "S2")]
raisesExact
  C.Compile (C.UnboundId "S2");

fun compile b = C.compile
  { depsFirst = false, jobs = 0, logger = NONE }
  (PolyMLB.Dag.process
    { cache = NONE, logger = NONE, reduce = true }
    (fn "root" => [Ann ([PolyMLB.Ann.ImportAll], b)]
      | _ => raise Fail "")
    "root");

"Compile.compile valid"
assert
  compile [SourceFile "sml/source/good.sml", Signature ("SIG2", "SIG")]
is
  (fn (PolyMLB.NameSpace.N (_, ns)) =>
    PolyML.pointerEq (valOf (#lookupSig ns "SIG2"), valOf (#lookupSig ns "SIG"))
    andalso
    case #lookupStruct ns "Str" of
      NONE => false
    | SOME s => Option.isSome (#lookupVal (N.Structures.contents s) "i"))
