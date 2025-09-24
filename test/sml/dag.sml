structure A = PolyMLB.Ann
structure D = PolyMLB.Dag

datatype z = datatype PolyMLB.Basis.dec
datatype z = datatype PolyMLB.Basis.exp

val process = D.process { cache = NONE, logger = NONE, reduce = true }

local
  val b1 = [BasisFile "b2", BasisFile "b3"]
  val b2 = [BasisFile "b3"]
  val b3 = [BasisFile "b4"]
  val b4 = [BasisFile "b1"]

  fun b "b1" = b1
    | b "b2" = b2
    | b "b3" = b3
    | b "b4" = b4
    | b _ = raise Fail ""
in
  val _ =
  "Dag.process raises on cycle and returns ordered cycle"
  assert
    (fn () => process b "b1")
  raisesExact
    D.Dag (D.Cycle ["b1", "b2", "b3", "b4"])
end

local
  structure A = Array
  val a = A.tabulate (4, fn _ => 0)
  fun ++ i = A.update (a, i, A.sub (a, i) + 1)

  val b1 = [BasisFile "b2", BasisFile "b3", BasisFile "b4"]
  val b2 = [BasisFile "b3", BasisFile "b4"]
  val b3 = [BasisFile "b4"]
  val b4 = []

  fun b "b1" = (++0; b1)
    | b "b2" = (++1; b2)
    | b "b3" = (++2; b3)
    | b "b4" = (++3; b4)
    | b _ = raise Fail ""
in
  val _ =
  "Dag.process calls the MLB callback exactly once per MLB"
  assert
    process b "b1"
  is
    (fn _ => A.foldl (fn (r, b) => b andalso r = 1) true a)
end

local
  val b = ref true
in
  val _ =
  "Dag.process respects Discard annotations"
  assert
    process
      (fn "b" => [Ann ([A.Discard], [BasisFile "foo"])]
        |  _  => (b := false; []))
      "b"
  is
    (fn _ => !b)
end

local
  val b = ref true
in
  val _ =
  "Dag.process respects IgnoreFiles annotations"
  assert
    process
      (fn "b" => [Ann ([A.IgnoreFiles ["foo"]], [BasisFile "foo"])]
        |  _  => (b := false; []))
      "b"
  is
    (fn _ => !b)
end

local
(* input:
digraph G {
  b1 -> b2;
  b1 -> b3;
  b2 -> b3;
  b2 -> b5;
  b2 -> b4;
  b4 -> b5;
}
 *)
  val b1 = [BasisFile "b2", BasisFile "b3"]
  val b2 = [BasisFile "b3", BasisFile "b4", BasisFile "b5"]
  val b3 = []
  val b4 = [BasisFile "b5"]
  val b5 = []

  fun b "b1" = b1
    | b "b2" = b2
    | b "b3" = b3
    | b "b4" = b4
    | b "b5" = b5
    | b _ = raise Fail ""

(* reduced output:
digraph G {
  b1 -> b2;
  b2 -> b3;
  b2 -> b4;
  b4 -> b5;
}
*)

  val fl = Vector.fromList
in
  val _ =
  "Dag.process reduces input graph"
  assert
    (#root o #dag o process b) "b1"
  eq
    D.N (0, fl
      [ D.N (1, fl
        [ D.N (2, fl [])
        , D.N (3, fl [D.N (4, fl [])])
        ])
      ])
end
