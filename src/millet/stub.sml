structure HashArray :>
sig
  type 'a hash
  val hash : int -> 'a hash
  val update : 'a hash * string * 'a -> unit
  val sub : 'a hash * string -> 'a option
  val delete : 'a hash * string -> unit
  val fold : (string * 'a * 'b -> 'b) -> 'b -> 'a hash -> 'b
end =
struct
end

structure Universal :>
sig
    type universal
    type 'a tag
    val tag : unit -> 'a tag
    val tagInject  : 'a tag -> 'a -> universal
    val tagIs      : 'a tag -> universal -> bool
    val tagProject : 'a tag -> universal -> 'a
end =
struct
end

structure PolyML :
sig
  type location =
    { file : string
    , startLine : int, endLine : int
    , startPosition : int, endPosition : int
    }

  structure Exception :
  sig
    val exceptionLocation : exn -> location option
    val raiseWithLocation : exn * location -> 'a
    val reraise : exn -> 'a

    val traceException : (unit -> 'a) * (string list * exn -> 'a) -> 'a
    val exception_trace : (unit -> 'a) -> 'a
  end

  datatype context =
    ContextLocation of location
  | ContextProperty of string * string

  datatype pretty =
    PrettyBlock of int * bool * context list * pretty list
  | PrettyBreak of int * int
  | PrettyLineBreak
  | PrettyString of string
  | PrettyStringWithWidth of string * int

  val prettyPrint : (string -> unit) * int -> pretty -> unit

  type typeExpression

  datatype ptProperties =
    PTbreakPoint of bool ref
  | PTcompletions of string list
  | PTdeclaredAt of location
  | PTdefId of int
  | PTfirstChild of unit -> location * ptProperties list
  | PTnextSibling of unit -> location * ptProperties list
  | PTopenedAt of location
  | PTparent of unit -> location * ptProperties list
  | PTpreviousSibling of unit -> location * ptProperties list
  | PTprint of int -> pretty
  | PTreferences of bool * location list
  | PTrefId of int
  | PTstructureAt of location
  | PTtype of typeExpression

  type parseTree = location * ptProperties list

  structure CodeTree :
  sig
    type codeBinding
    type codeTree
    type machineWord
  end

  structure NameSpace :
  sig
    type fixity
    type functorVal
    type signatureVal
    type structureVal
    type typeConstr
    type value

    type nameSpace =
      { allFix : unit -> (string * fixity) list
      , allFunct : unit -> (string * functorVal) list
      , allSig : unit -> (string * signatureVal) list
      , allStruct : unit -> (string * structureVal) list
      , allType : unit -> (string * typeConstr) list
      , allVal : unit -> (string * value) list
      , enterFix : string * fixity -> unit
      , enterFunct : string * functorVal -> unit
      , enterSig : string * signatureVal -> unit
      , enterStruct : string * structureVal -> unit
      , enterType : string * typeConstr -> unit
      , enterVal : string * value -> unit
      , lookupFix : string -> fixity option
      , lookupFunct : string -> functorVal option
      , lookupSig : string -> signatureVal option
      , lookupStruct : string -> structureVal option
      , lookupType : string -> typeConstr option
      , lookupVal : string -> value option
      }

    structure Functors :
    sig
      type functorVal
      val code : functorVal -> CodeTree.codetree
      val name : functorVal -> string
      val print : functorVal * int * nameSpace option -> pretty
      val properties : functorVal -> ptProperties list
    end where type functorVal = functorVal
    structure Infixes :
    sig
      type fixity
      val name : fixity -> string
      val print : fixity -> pretty
    end where type fixity = fixity
    structure Signatures :
    sig
      type signatureVal
      val name : signatureVal -> string
      val print : signatureVal * int * nameSpace option -> pretty
      val properties : signatureVal -> ptProperties list
    end where type signatureVal = signatureVal
    structure Structures :
    sig
      type structureVal
      val code : structureVal -> CodeTree.codetree
      val contents : structureVal -> nameSpace
      val name : structureVal -> string
      val print : structureVal * int * nameSpace option -> pretty
      val properties : structureVal -> ptProperties list
    end where type structureVal = structureVal
    structure TypeConstrs :
    sig
      type typeConstr
      val name : typeConstr -> string
      val print : typeConstr * int * nameSpace option -> pretty
      val properties : typeConstr -> ptProperties list
    end where type typeConstr = typeConstr
    structure Values :
    sig
      type typeExpression
      type value
      val code : value -> CodeTree.codetree
      val isConstructor : value -> bool
      val isException : value -> bool
      val name : value -> string
      val print : value * int -> pretty
      val printType : typeExpression * int * nameSpace option -> pretty
      val printWithType : value * int * nameSpace option -> pretty
      val properties : value -> ptProperties list
      val typeof : value -> typeExpression
    end where type value = value and type typeExpression = typeExpression

    val globalNameSpace : nameSpace
  end

  val globalNameSpace : NameSpace.nameSpace
  structure SaveState:
  sig
    val saveState : string -> unit
    val loadState : string -> unit
    val saveChild : string * int -> unit
    val renameParent : {child: string, newParent: string} -> unit
    val showHierarchy : unit -> string list
    val showParent : string -> string option
    val loadHierarchy : string list -> unit
    structure Tags:
    sig
      val fixityTag : (string * NameSpace.Infixes.fixity) Universal.tag
      val functorTag : (string * NameSpace.Functors.functorVal) Universal.tag
      val signatureTag : (string * NameSpace.Signatures.signatureVal) Universal.tag
      val structureTag : (string * NameSpace.Structures.structureVal) Universal.tag
      val typeTag : (string * NameSpace.TypeConstrs.typeConstr) Universal.tag
      val valueTag : (string * NameSpace.Values.value) Universal.tag
      val startupTag : (unit -> unit) Universal.tag
    end
    val loadModule : string -> unit
    val loadModuleBasic : string -> Universal.universal list
    val saveModule :
        string
      * { functors : string list
        , onStartup : (unit -> unit) option
        , sigs : string list
        , structs : string list
        }
      -> unit
    val saveModuleBasic : string * Universal.universal list -> unit
  end

  structure Compiler :
  sig
    datatype compilerParameters =
      CPOutStream of string->unit
    | CPNameSpace of NameSpace.nameSpace
    | CPErrorMessageProc of
        { message : pretty
        , hard : bool
        , location : location
        , context : pretty option
        } -> unit
    | CPLineNo of unit -> int
    | CPLineOffset of unit -> int
    | CPFileName of string
    | CPPrintInAlphabeticalOrder of bool
    | CPResultFun of
        { fixes : (string * NameSpace.Infixes.fixity) list
        , values : (string * NameSpace.Values.value) list
        , structures : (string * NameSpace.Structures.structureVal) list
        , signatures : (string * NameSpace.Signatures.signatureVal) list
        , functors : (string * NameSpace.Functors.functorVal) list
        , types : (string * NameSpace.TypeConstrs.typeConstr) list
        } -> unit
  	| CPCompilerResultFun of
        parseTree option *
			  ( unit ->
          { fixes : (string * NameSpace.Infixes.fixity) list
          , values : (string * NameSpace.Values.value) list
          , structures : (string * NameSpace.Structures.structureVal) list
          , signatures : (string * NameSpace.Signatures.signatureVal) list
          , functors : (string * NameSpace.Functors.functorVal) list
          , types : (string * NameSpace.TypeConstrs.typeConstr) list
        }
        ) option -> unit -> unit
  	| CPProfiling of int
  	| CPTiming of bool
  	| CPDebug of bool
  	| CPPrintDepth of unit->int
  	| CPPrintStream of string->unit
  	| CPErrorDepth of int
  	| CPLineLength of int
  	| CPRootTree of
        { parent : (unit -> parseTree) option
        , next : (unit -> parseTree) option
        , previous : (unit -> parseTree) option
        }
  	(* | CPAllocationProfiling of int *)
  	| CPDebuggerFunction of
        int * NameSpace.Values.value * int * string * string * NameSpace.nameSpace
        -> unit

    val compilerVersion : string
    val compilerVersionNumber : int

    val printDepth : int ref
    val errorDepth : int ref
    val lineLength : int ref

    val printInAlphabeticalOrder : bool ref

    val prompt1 : string ref
    val prompt2 : string ref

    val reportExhaustiveHandlers : bool ref
    val reportUnreferencedIds : bool ref
    val reportDiscardFunction : bool ref
    val reportDiscardNonUnit : bool ref

    val debug : bool ref
    val timing : bool ref
    val profiling : int ref
    val allocationProfiling : int ref

    val lowlevelOptimise : bool ref
    val inlineFunctors : bool ref
    val createPrintFunctions : bool ref
    val maxInlineSize : int ref
    val narrowOverloadFlexRecord : bool ref
    val traceCompiler : bool ref

    val parsetree : bool ref
    val codetree : bool ref
    val codetreeAfterOpt : bool ref
    val assemblyCode : bool ref
    val pstackTrace : bool ref

    val fixityNames : unit -> string list
    val functorNames : unit -> string list
    val signatureNames : unit -> string list
    val structureNames : unit -> string list
    val typeNames : unit -> string list
    val valueNames : unit -> string list

    val forgetFixity : string -> unit
    val forgetFunctor : string -> unit
    val forgetSignature : string -> unit
    val forgetStructure : string -> unit
    val forgetType : string -> unit
    val forgetValue : string -> unit
  end

  val compiler :
    (unit -> char option) * Compiler.compilerParameters list
    -> unit -> unit

  val make : string -> unit
  val getUseFileName : unit -> string option
  val suffixes : string list ref
  val export : string * (unit -> unit) -> unit
  val makestring : 'a -> string
end =
struct
end

signature THREAD =
sig
  exception Thread of string
  structure Thread :
  sig
    eqtype thread
    datatype threadAttribute =
      EnableBroadcastInterrupt of bool
    | InterruptState of interruptState
    | MaximumMLStack of int option
    and interruptState =
      InterruptDefer
    | InterruptSynch
    | InterruptAsynch
    | InterruptAsynchOnce
    val fork : (unit->unit) * threadAttribute list -> thread
    val exit : unit -> unit
    val isActive : thread -> bool
    val equal : thread * thread -> bool
    val self : unit -> thread
    exception Interrupt
    val interrupt : thread -> unit
    val broadcastInterrupt : unit -> unit
    val testInterrupt : unit -> unit
    val kill : thread -> unit
    val getLocal : 'a Universal.tag -> 'a option
    and setLocal : 'a Universal.tag * 'a -> unit
    val setAttributes : threadAttribute list -> unit
    val getAttributes : unit -> threadAttribute list
    val numProcessors : unit -> int
    and numPhysicalProcessors : unit -> int option
  end
  structure Mutex :
  sig
    type mutex
    val mutex : unit -> mutex
    val lock : mutex -> unit
    val unlock : mutex -> unit
    val trylock : mutex -> bool
  end
  structure ConditionVar :
  sig
    type conditionVar
    val conditionVar : unit -> conditionVar
    val wait : conditionVar * Mutex.mutex -> unit
    val waitUntil : conditionVar * Mutex.mutex * Time.time -> bool
    val signal : conditionVar -> unit
    val broadcast : conditionVar -> unit
  end
end

structure Thread :> THREAD =
struct
end

structure ThreadLib :
sig
  val protect : Thread.Mutex.mutex -> ('a -> 'b) -> 'a -> 'b
end =
struct
end
