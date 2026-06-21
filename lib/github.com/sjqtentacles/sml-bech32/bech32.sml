structure Bech32 :> BECH32 =
struct
  val charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

  val gen0 : Word32.word = 0wx3b6a57b2
  val gen1 : Word32.word = 0wx26508e6d
  val gen2 : Word32.word = 0wx1ea119fa
  val gen3 : Word32.word = 0wx3d4233dd
  val gen4 : Word32.word = 0wx2a1462b3

  val bech32Const  : Word32.word = 0wx1
  val bech32mConst : Word32.word = 0wx2bc830a3

  fun polymod (values : Word8.word list) : Word32.word =
    let
      fun step (chk : Word32.word) (v : Word8.word) : Word32.word =
        let
          val b = Word32.>> (chk, 0w25)
          val chk' = Word32.xorb (
                       Word32.<< (Word32.andb (chk, 0wx1ffffff), 0w5),
                       Word32.fromLarge (Word8.toLarge v)
                     )
          fun xorIf (bit : Word32.word) (gen : Word32.word) (acc : Word32.word) : Word32.word =
            if Word32.andb (b, bit) <> 0w0 then Word32.xorb (acc, gen) else acc
          val chk'' = xorIf 0w1  gen0
                    ( xorIf 0w2  gen1
                    ( xorIf 0w4  gen2
                    ( xorIf 0w8  gen3
                    ( xorIf 0w16 gen4 chk'))))
        in
          chk''
        end
    in
      List.foldl (fn (v, acc) => step acc v) 0w1 values
    end

  fun hrpExpand (hrp : string) : Word8.word list =
    let
      val chars = String.explode hrp
      val high = List.map (fn c => Word8.fromInt (Char.ord c div 32)) chars
      val low  = List.map (fn c => Word8.fromInt (Char.ord c mod 32)) chars
    in
      high @ [0w0] @ low
    end

  fun verifyChecksum (hrp : string) (dataWords : Word8.word list) (const : Word32.word) : bool =
    polymod (hrpExpand hrp @ dataWords) = const

  fun createChecksum (hrp : string) (dataWords : Word8.word list) (const : Word32.word) : Word8.word list =
    let
      val values = hrpExpand hrp @ dataWords @ [0w0, 0w0, 0w0, 0w0, 0w0, 0w0]
      val pm = Word32.xorb (polymod values, const)
    in
      List.tabulate (6, fn i =>
        Word8.fromLarge (Word32.toLarge (
          Word32.andb (Word32.>> (pm, Word.fromInt ((5 - i) * 5)), 0wx1f)
        ))
      )
    end

  fun charsetIndex (c : char) : int option =
    let
      fun findIdx i =
        if i >= String.size charset then NONE
        else if String.sub (charset, i) = c then SOME i
        else findIdx (i + 1)
    in
      findIdx 0
    end

  fun hasUpper (s : string) : bool =
    CharVector.exists Char.isUpper s

  fun hasLower (s : string) : bool =
    CharVector.exists Char.isLower s

  fun encodeWith (const : Word32.word) (hrp : string) (dataStr : string) : string =
    let
      val dataBytes = List.map (fn c => Word8.fromInt (Char.ord c)) (String.explode dataStr)
      val checksum = createChecksum hrp dataBytes const
      val combined = dataBytes @ checksum
      val encoded = String.implode (List.map (fn w =>
        String.sub (charset, Word8.toInt w)) combined)
    in
      hrp ^ "1" ^ encoded
    end

  fun decodeWith (const : Word32.word) (s : string) : {hrp: string, data: string} option =
    if hasUpper s andalso hasLower s then NONE
    else
      let
        val str = String.map Char.toLower s
        val len = String.size str
        fun findLastOne i =
          if i < 0 then ~1
          else if String.sub (str, i) = #"1" then i
          else findLastOne (i - 1)
        val sepPos = findLastOne (len - 1)
      in
        if sepPos < 1 orelse len - sepPos - 1 < 6 orelse len > 90 then NONE
        else
          let
            val hrp = String.substring (str, 0, sepPos)
            val dataStr = String.substring (str, sepPos + 1, len - sepPos - 1)
            fun decodeChars [] acc = SOME (List.rev acc)
              | decodeChars (c :: cs) acc =
                  (case charsetIndex c of
                     NONE => NONE
                   | SOME i => decodeChars cs (Word8.fromInt i :: acc))
            val decodedOpt = decodeChars (String.explode dataStr) []
          in
            case decodedOpt of
              NONE => NONE
            | SOME decoded =>
                if verifyChecksum hrp decoded const then
                  let
                    val dataWords = List.take (decoded, length decoded - 6)
                    val dataResult = String.implode (List.map (fn w => Char.chr (Word8.toInt w)) dataWords)
                  in
                    SOME {hrp = hrp, data = dataResult}
                  end
                else NONE
          end
      end

  fun encode hrp dataStr = encodeWith bech32Const hrp dataStr
  fun decode s = decodeWith bech32Const s
  fun encodeM hrp dataStr = encodeWith bech32mConst hrp dataStr
  fun decodeM s = decodeWith bech32mConst s
end

structure SegwitAddr :> SEGWIT_ADDR =
struct
  (* 2^n for small n *)
  fun pow2 (n : int) : int =
    if n = 0 then 1 else 2 * pow2 (n - 1)

  fun convertbits (dataBytes : int list) (fromBits : int) (toBits : int) (pad : bool) : int list option =
    let
      val accRef  = ref (0w0 : Word.word)
      val bitsRef = ref 0
      val maxv    = Word.fromInt (pow2 toBits - 1)
      val fromMax = pow2 fromBits
      val result  = ref ([] : int list)
      fun extract () =
        let
          val shifted = !bitsRef - toBits
          val v = Word.toInt (Word.andb (Word.>> (!accRef, Word.fromInt shifted), maxv))
        in
          result := v :: !result;
          bitsRef := !bitsRef - toBits
        end
      fun processOne v =
        if v < 0 orelse v >= fromMax then false
        else
          ( accRef := Word.orb (Word.<< (!accRef, Word.fromInt fromBits), Word.fromInt v)
          ; bitsRef := !bitsRef + fromBits
          ; while !bitsRef >= toBits do extract ()
          ; true
          )
      val ok = List.all processOne dataBytes
    in
      if not ok then NONE
      else if pad then
        ( if !bitsRef > 0 then
            result := Word.toInt (Word.andb (
                        Word.<< (!accRef, Word.fromInt (toBits - !bitsRef)),
                        maxv)) :: !result
          else ()
        ; SOME (List.rev (!result))
        )
      else
        ( if !bitsRef >= fromBits
             orelse Word.andb (Word.<< (!accRef, Word.fromInt (toBits - !bitsRef)), maxv) <> 0w0
          then NONE
          else SOME (List.rev (!result))
        )
    end

  fun encode hrp witver prog =
    let
      val progBytes = List.map Char.ord (String.explode prog)
      val progLen = String.size prog
    in
      if witver < 0 orelse witver > 16 then NONE
      else if progLen < 2 orelse progLen > 40 then NONE
      else if witver = 0 andalso progLen <> 20 andalso progLen <> 32 then NONE
      else
        case convertbits progBytes 8 5 true of
          NONE => NONE
        | SOME converted =>
            let
              val dataWords = String.implode (List.map Char.chr converted)
              val dataFull = String.str (Char.chr witver) ^ dataWords
              val addr = if witver = 0
                         then Bech32.encode hrp dataFull
                         else Bech32.encodeM hrp dataFull
            in
              SOME addr
            end
    end

  fun decode addr =
    let
      val lower = String.map Char.toLower addr
      fun tryDecodeWith dec minVer maxVer =
        case dec lower of
          NONE => NONE
        | SOME {hrp, data} =>
            if String.size data < 1 then NONE
            else
              let
                val witver = Char.ord (String.sub (data, 0))
                val dataWords = List.map Char.ord
                                  (String.explode (String.substring (data, 1, String.size data - 1)))
              in
                if witver < minVer orelse witver > maxVer then NONE
                else
                  case convertbits dataWords 5 8 false of
                    NONE => NONE
                  | SOME prog =>
                      let val progLen = length prog
                      in
                        if progLen < 2 orelse progLen > 40 then NONE
                        else if witver = 0 andalso progLen <> 20 andalso progLen <> 32 then NONE
                        else
                          SOME {hrp = hrp, version = witver,
                                program = String.implode (List.map Char.chr prog)}
                      end
              end
      val v0res = tryDecodeWith Bech32.decode 0 0
    in
      case v0res of
        SOME r => SOME r
      | NONE   => tryDecodeWith Bech32.decodeM 1 16
    end
end
