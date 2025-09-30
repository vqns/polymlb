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


  (* File sys based cache; the given string is the base dir to fetch/store from.
   * Safe to use across multiple threads and concurrent compilation instances
   * but not from multiple processes.
   * If the given directory exists and is not a readable or writable directory,
   * an in memory cache is instead returned.
   *)
  val fileSys : { logger : Log.logger option, dir : string } -> t
end =
struct
  structure H  = HashArray
  structure M  = Thread.Mutex
  structure PS = PolyML.SaveState
  structure U  = Universal

  type t =
    { time     : string -> Time.time option
    , hasOrSet : { id : string, bas : Basis.t, deps : string vector } -> bool
    , set      : { id : string, bas : Basis.t, deps : string vector } -> unit
    , fetch    : string -> NameSpace.t option
    , store    : string * Time.time * NameSpace.t -> NameSpace.t option
    }

  (* Simple implementation that holds everything in hash maps guarded by a lock.
   *)
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

  (* The fs based implementation is a little more involved.
   *
   * It acts as a two-level cache where values are stored and fetched through
   * PolyML.SaveState.{load,save}ModuleBasic and also kept in memory in order
   * to avoid excessive reloading.
   * Basis.t are stored in .bas files, dependencies in .deps files and
   * NameSpace.t in .ns files, with a single value in each file. The filename
   * for a given id is the unpadded ilename-safe (§5) base64 encoding of its
   * SHA-1 hash, appended with the correct extension.
   *
   * E.g, for cache path = "/foo/bar" and id = "abc", we have the following:
   *   sha1 "abc" = a9993e364706816aba3e25717850c26c9cd0d89d
   *   (base64 o sha1) "abc" = qZk-NkcGagC6PiVxeFDCbJzQNid
   *   Basis.t = "/foo/bar/qZk-NkcGagC6PiVxeFDCbJzQNid.bas"
   *   Dependecies = "/foo/bar/qZk-NkcGagC6PiVxeFDCbJzQNid.deps"
   *   NameSpace.t = "/foo/bar/qZk-NkcGagC6PiVxeFDCbJzQNid.ns"
   *
   * Concurrency safety is implemented with mutexes.
   *
   * There is a few caveats, namely:
   * - there is no hash collision detection;
   * - all locking is blocking without timeout.
   *
   * see:
   * SHA-1:
   *   https://en.wikipedia.org/wiki/SHA-1
   *   https://datatracker.ietf.org/doc/html/rfc3174
   * base64:
   *   https://en.wikipedia.org/wiki/Base64
   *   https://datatracker.ietf.org/doc/html/rfc4648
   *)

  local
    structure A  = Array
    structure AS = ArraySlice
    structure CA = CharArray
    structure V  = Vector
    structure VS = VectorSlice
    structure W  = Word

    val `& = Word32.andb
    val `| = Word32.orb
    val `^ = Word32.xorb
    val << = Word32.<<
    val >> = Word32.>>
    val `~ = Word32.notb

    infix 6 `|
    infix 7 `^ << >>
    infix 8 `&

    fun rotl (w, d) = w << d `| w >> (0w32 - d)

    fun pad s =
      let
        val sz = size s + 9
        val sz = (sz + 64 - (sz mod 64)) div 4
        val sz32 = size s div 4
        val bitLength = W.fromInt (size s * 8)
        val one = ref false
        val fromLarge = Word32.fromLarge
        val bytes = Byte.stringToBytes s
        fun f i =
          (fromLarge o Word8.toLarge o Word8Vector.sub) (bytes, size s - i)
      in
        V.tabulate
          (sz, fn i =>
            if i < sz32 then
              fromLarge (PackWord32Big.subVec (bytes, i))
            else if i * 4 < size s then
              case (one := true; size s - i * 4) of
                1 => f 1 << 0w24 `| 0w1 << 0w23
              | 2 => f 2 << 0w24 `| f 1 << 0w16 `| 0w1 << 0w15
              | 3 => f 3 << 0w24 `| f 2 << 0w16 `| f 1 << 0w8 `| 0w1 << 0w7
              | _ => raise Fail "Cache.pad: impossible"
            else if i = sz - 2 then
              (fromLarge o W.toLarge o W.andb) (bitLength, 0wx7FFF0000)
            else if i = sz - 1 then
              (fromLarge o W.toLarge o W.andb) (bitLength, 0wxFFFF)
            else if not (!one) then
              (one := true; 0w1 << 0w31)
            else
              0w0)
      end

    fun sha1 v =
      let
        val w = A.array (80, 0w0 : Word32.word)

        fun init i =
          let
            val ! = A.sub infix 8 ! infix 9 -
            fun loop 80 = ()
              | loop i =
                  ( A.update (w, i,
                      rotl (w!i-3 `^ w!i-8 `^ w!i-14 `^ w!i-16, 0w1))
                  ; loop (i + 1)
                  )
          in
            AS.copyVec { src = VS.slice (v, i, SOME 16), dst = w, di = 0 };
            loop 16
          end

        fun hash (hs as (h0, h1, h2, h3, h4)) =
          let
            fun loop 80 (a, b, c, d, e) =
                  (h0 + a, h1 + b, h2 + c, h3 + d, h4 + e)
              | loop i (a, b, c, d, e) =
                  let
                    val (f, k) =
                      if i < 20 then
                        (b `& c `| `~b `& d, 0wx5A827999)
                      else if i < 40 then
                        (b `^ c `^ d, 0wx6ED9EBA1)
                      else if i < 60 then
                        (b `& c `^ b `& d `^ c `& d, 0wx8F1BBCDC)
                      else
                        (b `^ c `^ d, 0wxCA62C1D6)
                  in
                    loop (i + 1)
                      ( rotl (a, 0w5) + f + e + k + A.sub (w, i)
                      , a, rotl (b, 0w30), c, d
                      )
                  end
          in
            loop 0 hs
          end

        fun chunk i hs =
          if i = V.length v then
            hs
          else
            (init i; chunk (i + 16) (hash hs))
      in
        chunk 0 (0wx67452301, 0wxEFCDAB89, 0wx98BADCFE, 0wx10325476, 0wxC3D2E1F0)
      end

    val chars =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

    fun ch w = String.sub (chars, Word32.toInt (w `& 0wx3F))

    fun enc3 (a, i, w) =
      ( CA.update (a, i,     ch (w >> 0w18))
      ; CA.update (a, i + 1, ch (w >> 0w12))
      ; CA.update (a, i + 2, ch (w >> 0w6))
      ; CA.update (a, i + 3, ch  w)
      )

    fun enc2 (a, i, w) =
      ( CA.update (a, i,     ch (w >> 0w12))
      ; CA.update (a, i + 1, ch (w >> 0w6))
      ; CA.update (a, i + 2, ch  w)
      )

    fun base64 (w0, w1, w2, w3, w4) =
      let
        val a = CA.array (27, #" ")
      in
        enc3 (a, 0,  w0 >> 0w8);
        enc3 (a, 4,  w0 `& 0wxFF   << 0w16 `| w1 >> 0w16);
        enc3 (a, 8,  w1 `& 0wxFFFF << 0w16 `| w2 >> 0w24);
        enc3 (a, 12, w2 `& 0wxFFFFFF);
        enc3 (a, 16, w3 >> 0w8);
        enc3 (a, 20, w3 `& 0wxFF   << 0w16 `| w4 >> 0w16);
        enc2 (a, 24, w4 `& 0wxFFFF);
        CA.vector a
      end
  in
    val fileName = base64 o sha1 o pad
  end

  val basTag  : Basis.t U.tag = U.tag ()
  val depsTag : string vector U.tag = U.tag ()
  val nsTag   : NameSpace.t U.tag = U.tag ()

  val empty = Vector.fromList ([] : string list)

  fun fileSys { log, dir } : t =
    let
      val err = Log.log log Log.Error
      val dbg = Log.log log Log.Debug
      val trc = Log.log log Log.Trace

      (* Filename cache (without extension). *)
      local
        val names : string H.hash = H.hash 10
        val m = M.mutex ()
      in
        fun fname s =
          (M.lock m
          ; case H.sub (names, s) of
              SOME n => n before M.unlock m
            | NONE =>
                let
                  val n = OS.Path.joinDirFile { dir = dir, file = fileName s }
                in
                  (H.update (names, s, n); M.unlock m; n)
                end
          )
      end

      local
        val bss : Basis.t H.hash = H.hash 10
        val dss : string vector H.hash = H.hash 10
        val nss : NameSpace.t H.hash = H.hash 10
        val bm = M.mutex ()
        val dm = M.mutex ()
        val nm = M.mutex ()

        fun upd (h, m) (k, v) = (M.lock m; H.update (h, k, v); M.unlock m)

        fun modInfo f =
          let
            val { moduleSignature = s, dependencies = d, ... } =
              PS.getModuleInfo f
          in
              "sig = "
            :: Word8Vector.foldr (fn (b, l) => Word8.toString b :: l) [] s
            @ [", deps = [", String.concatWith ", " (map #1 d), "]"]
          end

        fun load (kind, tag) (id, file) =
          let
            fun bad m =
              ( err (fn fmt => concat
                  [fmt id, ": could not load module ", fmt file, ": ", m])
              ; NONE
              )
          in
            dbg (fn fmt => concat [fmt id, ": loading ", kind, " from ", fmt file]);
            (case PS.loadModuleBasic file of
              [v] =>
                if U.tagIs tag v then
                  SOME (U.tagProject tag v)
                else
                  bad "tag mismatch"
            | l => bad ("expected one value, found " ^ Int.toString (length l)))
              handle e => bad (exnMessage e)
          end

        fun save (kind, tag) (id, file) v =
          ( dbg (fn fmt => concat [fmt id, ": saving ", kind, " to ", fmt file])
          ; (true before
              PS.saveModuleBasic (file, [U.tagInject tag v])
              handle e => false before err
                (fn fmt => concat
                  [ fmt id, ": could not save module ", fmt file, ": "
                  , exnMessage e
                  ]))
            andalso
              (trc (fn fmt => concat (fmt file :: ": " :: modInfo file)); true)
          )

        fun loadDeps (id, file) =
          case (M.lock dm; H.sub (dss, id)) of
            SOME v => v before M.unlock dm
          | NONE =>
               case load ("deps", depsTag) (id, file) of
                NONE => empty
              | SOME v =>
                  ( H.update (dss, id, v)
                  ; M.unlock dm
                  ; v
                  )
      in
        fun saveBas { id, bas, deps } =
          let
            val file = fname id
            val bFile = file ^ ".bas"
            val dFile = file ^ ".deps"
          in
            upd (bss, bm) (id, bas);
            save ("bas", basTag) (id, bFile) bas;
            upd (dss, dm) (id, deps);
            save ("deps", depsTag) (id, dFile) deps;
            ()
          end

        fun cosBas { id, bas, deps } =
          let
            val file = fname id
            val bFile = file ^ ".bas"
            val dFile = file ^ ".deps"
          in
            dbg (fn fmt => fmt id ^ ": checking bas");
            M.lock bm;
            if
              H.sub (bss, id) = SOME bas
                orelse load ("bas", basTag) (id, bFile) = SOME bas
            then
              (M.unlock bm; true)
            else
              ( H.update (bss, id, bas)
              ; M.unlock bm
              ; save ("bas", basTag) (id, bFile) bas
              ; upd (dss, dm) (id, deps)
              ; save ("deps", depsTag) (id, dFile) deps
              ; false
              )
          end

        fun loadNs id =
          case (M.lock nm; H.sub (nss, id) before M.unlock nm) of
            SOME ns => SOME ns
          | NONE =>
              (* todo: need to keep track that it is currently being loaded *)
              let
                val file = fname id
                val dsFile = file ^ ".deps"
                val nsFile = file ^ ".ns"
              in
                (Vector.app (ignore o loadNs) o loadDeps) (id, dsFile);
                case load ("ns", nsTag) (id, nsFile) of
                  NONE => NONE
                | SOME ns => SOME ns before upd (nss, nm) (id, ns)
              end

        fun saveNs (id, t, ns) =
          let
            val file = fname id ^ ".ns"
          in
            if save ("ns", nsTag) (id, file) ns then
              OS.FileSys.setTime (file, SOME t)
            else
              ();
            upd (nss, nm) (id, ns);
            SOME ns
          end
      end

      fun time s = (SOME o OS.FileSys.modTime) (fname s ^ ".ns")
        handle _ => NONE
    in
      { time     = time
      , hasOrSet = cosBas
      , set      = saveBas
      , fetch    = loadNs
      , store    = saveNs
      }
    end

  val fileSys =
    fn { logger, dir } =>
      let
        fun mem m =
          ( Log.log logger Log.Error
              (fn fmt => "could not create cache in " ^ fmt dir ^ ": " ^ m)
          ; memory ()
          )
        fun fs () =
          fileSys
            { log = logger
            , dir = OS.Path.mkAbsolute
                { path = dir, relativeTo = OS.FileSys.getDir () }
            }
        open OS.FileSys
      in
        (if isDir dir then
          if access (dir, [A_READ, A_WRITE]) then
            fs ()
          else
            mem "bad permissions"
        else
          mem "not a directory")
        (* OS.FileSys.isDir raises if either the directory does not exist
         * or is not accessible. Try to create the dir: if it fails, then
         * it is the latter case and we return an in memory cache.
         *)
        handle _ =>
          let
            val { arcs, isAbs, ... } = OS.Path.fromString dir
          in
            foldl
              (fn (arc, parent) =>
                let
                  val p = OS.Path.concat (parent, arc)
                in
                  if isDir p handle _ => false then
                    ()
                  else
                    mkDir p;
                  p
                end)
              (if isAbs then "/" else "")
              arcs;
            fs ()
          end
          handle e => mem (exnMessage e)
      end
end
