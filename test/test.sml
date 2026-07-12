structure Tests =
struct
  fun decodeOpt opt =
    case opt of
      NONE => "NONE"
    | SOME {hrp, data=_} => "SOME{hrp=" ^ hrp ^ "}"

  fun run () =
    let
      val () = Harness.reset ()

      (* ---- BIP-173 valid addresses ---- *)
      val () = Harness.section "BIP-173 segwit decode"

      val bc1q = "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"
      val () = Harness.check "P2WPKH mainnet decodes" (isSome (SegwitAddr.decode bc1q))
      val () = (case SegwitAddr.decode bc1q of
                  SOME {hrp, version, program=_} =>
                    ( Harness.checkString "P2WPKH mainnet hrp" ("bc", hrp)
                    ; Harness.checkInt    "P2WPKH mainnet version" (0, version)
                    )
                | NONE => Harness.check "P2WPKH mainnet parsed" false)

      val tb1q = "tb1qw508d6qejxtdg4y5r3zarvary0c5xw7kxpjzsx"
      val () = Harness.check "P2WPKH testnet decodes" (isSome (SegwitAddr.decode tb1q))
      val () = (case SegwitAddr.decode tb1q of
                  SOME {hrp, version, program=_} =>
                    ( Harness.checkString "P2WPKH testnet hrp" ("tb", hrp)
                    ; Harness.checkInt    "P2WPKH testnet version" (0, version)
                    )
                | NONE => Harness.check "P2WPKH testnet parsed" false)

      val bc1p2wsh = "bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3"
      val () = Harness.check "P2WSH mainnet decodes" (isSome (SegwitAddr.decode bc1p2wsh))
      val () = (case SegwitAddr.decode bc1p2wsh of
                  SOME {hrp, version, program} =>
                    ( Harness.checkString "P2WSH mainnet hrp" ("bc", hrp)
                    ; Harness.checkInt    "P2WSH mainnet version" (0, version)
                    ; Harness.checkInt    "P2WSH mainnet prog len" (32, String.size program)
                    )
                | NONE => Harness.check "P2WSH mainnet parsed" false)

      (* ---- BIP-350 Bech32m addresses ---- *)
      val () = Harness.section "BIP-350 bech32m decode"

      val bc1p = "bc1p5cyxnuxmeuwuvkwfem96lqzszd02n6xdcjrs20cac6yqjjwudpxqkedrcr"
      val () = Harness.check "P2TR mainnet decodes" (isSome (SegwitAddr.decode bc1p))
      val () = (case SegwitAddr.decode bc1p of
                  SOME {hrp, version, program} =>
                    ( Harness.checkString "P2TR mainnet hrp" ("bc", hrp)
                    ; Harness.checkInt    "P2TR mainnet version" (1, version)
                    ; Harness.checkInt    "P2TR mainnet prog len" (32, String.size program)
                    )
                | NONE => Harness.check "P2TR mainnet parsed" false)

      val tb1p = "tb1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vq47zagq"
      val () = Harness.check "P2TR testnet decodes" (isSome (SegwitAddr.decode tb1p))
      val () = (case SegwitAddr.decode tb1p of
                  SOME {hrp, version, program} =>
                    ( Harness.checkString "P2TR testnet hrp" ("tb", hrp)
                    ; Harness.checkInt    "P2TR testnet version" (1, version)
                    ; Harness.checkInt    "P2TR testnet prog len" (32, String.size program)
                    )
                | NONE => Harness.check "P2TR testnet parsed" false)

      (* ---- Bech32 encode/decode roundtrip ---- *)
      val () = Harness.section "Bech32 roundtrip"

      val hrp1 = "test"
      val data1 = String.implode (List.map Char.chr [0, 1, 2, 3, 4, 5, 6, 7])
      val encoded1 = Bech32.encode hrp1 data1
      val () = (case Bech32.decode encoded1 of
                  SOME {hrp, data} =>
                    ( Harness.checkString "roundtrip hrp" (hrp1, hrp)
                    ; Harness.checkString "roundtrip data" (data1, data)
                    )
                | NONE => Harness.check "roundtrip decoded" false)

      val hrp2 = "bc"
      val data2 = String.implode (List.map Char.chr (List.tabulate (20, fn i => i mod 32)))
      val encoded2 = Bech32.encode hrp2 data2
      val () = (case Bech32.decode encoded2 of
                  SOME {hrp, data} =>
                    ( Harness.checkString "bc roundtrip hrp" (hrp2, hrp)
                    ; Harness.checkString "bc roundtrip data" (data2, data)
                    )
                | NONE => Harness.check "bc roundtrip decoded" false)

      (* ---- Bech32m encode/decode roundtrip ---- *)
      val () = Harness.section "Bech32m roundtrip"

      val hrp3 = "tb"
      val data3 = String.implode (List.map Char.chr [1, 5, 10, 15, 20, 25, 30, 0])
      val encoded3 = Bech32.encodeM hrp3 data3
      val () = (case Bech32.decodeM encoded3 of
                  SOME {hrp, data} =>
                    ( Harness.checkString "bech32m roundtrip hrp" (hrp3, hrp)
                    ; Harness.checkString "bech32m roundtrip data" (data3, data)
                    )
                | NONE => Harness.check "bech32m roundtrip decoded" false)

      (* ---- Cross-variant rejection ---- *)
      val () = Harness.section "Cross-variant rejection"

      val bech32Addr = Bech32.encode "test" (String.implode (List.map Char.chr [0,1,2,3,4]))
      val () = Harness.check "bech32 addr rejected by decodeM"
                             (not (isSome (Bech32.decodeM bech32Addr)))

      val bech32mAddr = Bech32.encodeM "test" (String.implode (List.map Char.chr [0,1,2,3,4]))
      val () = Harness.check "bech32m addr rejected by decode"
                             (not (isSome (Bech32.decode bech32mAddr)))

      (* ---- Invalid inputs ---- *)
      val () = Harness.section "Invalid inputs"

      val () = Harness.check "mixed case rejected"
                             (not (isSome (Bech32.decode "Test1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw")))

      val () = Harness.check "wrong checksum rejected"
                             (not (isSome (Bech32.decode "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t5")))

      val () = Harness.check "no separator rejected"
                             (not (isSome (Bech32.decode "qpzry9x8gf2tvdw0")))

      val () = Harness.check "too long rejected"
                             (not (isSome (Bech32.decode (String.implode (List.tabulate (91, fn _ => #"a"))))))

      val () = Harness.check "invalid char rejected"
                             (not (isSome (Bech32.decode "bc1qinvalidchar!")))

      (* ---- SegwitAddr encode/decode roundtrip ---- *)
      val () = Harness.section "SegwitAddr roundtrip"

      val prog20 = String.implode (List.map Char.chr (List.tabulate (20, fn i => i)))
      val () = (case SegwitAddr.encode "bc" 0 prog20 of
                  NONE => Harness.check "v0 p2wpkh encode" false
                | SOME addr =>
                    (case SegwitAddr.decode addr of
                       NONE => Harness.check "v0 p2wpkh roundtrip decode" false
                     | SOME {hrp, version, program} =>
                         ( Harness.checkString "v0 p2wpkh hrp" ("bc", hrp)
                         ; Harness.checkInt    "v0 p2wpkh version" (0, version)
                         ; Harness.checkString "v0 p2wpkh program" (prog20, program)
                         )))

      val prog32 = String.implode (List.map Char.chr (List.tabulate (32, fn i => i mod 256)))
      val () = (case SegwitAddr.encode "bc" 1 prog32 of
                  NONE => Harness.check "v1 p2tr encode" false
                | SOME addr =>
                    (case SegwitAddr.decode addr of
                       NONE => Harness.check "v1 p2tr roundtrip decode" false
                     | SOME {hrp, version, program} =>
                         ( Harness.checkString "v1 p2tr hrp" ("bc", hrp)
                         ; Harness.checkInt    "v1 p2tr version" (1, version)
                         ; Harness.checkString "v1 p2tr program" (prog32, program)
                         )))

      (* ---- SegwitAddr validation ---- *)
      val () = Harness.section "SegwitAddr validation"

      val () = Harness.check "invalid version 17 rejected"
                             (not (isSome (SegwitAddr.encode "bc" 17 prog20)))

      val () = Harness.check "prog too short rejected"
                             (not (isSome (SegwitAddr.encode "bc" 0 (String.str (Char.chr 0)))))

      val () = Harness.check "v0 prog 21 bytes rejected"
                             (not (isSome (SegwitAddr.encode "bc" 0 (String.implode (List.map Char.chr (List.tabulate (21, fn i => i)))))))

      val () = Harness.check "v0 32-byte prog ok"
                             (isSome (SegwitAddr.encode "bc" 0 prog32))

      (* ---- Properties (sml-check) ----
         decode-of-encode is the central law for this codec: for any valid
         (hrp, data) pair, decoding what we just encoded must return the
         original hrp and data. Fixed vector tests above only cover a
         handful of hand-picked addresses; these properties fuzz across
         random hrps, random 5-bit data words, and random SegwitAddr
         programs. *)
      val () = Harness.section "Properties (sml-check)"

      (* hrp: 1..20 lowercase alphanumeric chars (Bech32.decode lowercases
         the whole string before comparing, and rejects mixed case, so a
         round-trip needs an hrp that is already lowercase). *)
      val hrpAlphabet = "abcdefghijklmnopqrstuvwxyz0123456789"
      val genHrp : string Check.gen =
        Check.map String.implode
          (Check.resize 20
             (Check.nonEmptyListOf
                (Check.map (fn i => String.sub (hrpAlphabet, i))
                   (Check.choose (0, String.size hrpAlphabet - 1)))))

      (* data: a string of 5-bit words (chars valued 0..31), length 0..40 -
         Bech32.encode indexes its 32-symbol charset directly by each data
         char's ordinal, so values outside 0..31 would crash rather than
         fail gracefully. *)
      val genData : string Check.gen =
        Check.map (String.implode o List.map Char.chr)
          (Check.resize 40 (Check.listOf (Check.choose (0, 31))))

      fun showHrpData (hrp, data) =
        "hrp=\"" ^ hrp ^ "\" data=[" ^
        String.concatWith "," (List.map (Int.toString o Char.ord) (String.explode data))
        ^ "]"

      val genHrpData = Check.tuple2 (genHrp, genData)

      val () =
        Harness.check "prop: Bech32 encode/decode round-trips"
          (case Check.quickCheck
                  (Check.forAll genHrpData showHrpData
                     (fn (hrp, data) =>
                        case Bech32.decode (Bech32.encode hrp data) of
                            SOME {hrp = h, data = d} => h = hrp andalso d = data
                          | NONE => false)) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      val () =
        Harness.check "prop: Bech32m encode/decode round-trips"
          (case Check.quickCheck
                  (Check.forAll genHrpData showHrpData
                     (fn (hrp, data) =>
                        case Bech32.decodeM (Bech32.encodeM hrp data) of
                            SOME {hrp = h, data = d} => h = hrp andalso d = data
                          | NONE => false)) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      val () =
        Harness.check "prop: a Bech32m address is always rejected by plain decode"
          (case Check.quickCheck
                  (Check.forAll genHrpData showHrpData
                     (fn (hrp, data) => not (isSome (Bech32.decode (Bech32.encodeM hrp data))))) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      (* SegwitAddr round-trip: witness version 0..16, program length 2..40
         (exactly 20 or 32 when witver = 0, per BIP-141/173). *)
      val genSegwit : (int * string) Check.gen =
        Check.bind (Check.choose (0, 16)) (fn witver =>
          Check.bind
            (if witver = 0 then Check.elements [20, 32] else Check.choose (2, 40))
            (fn progLen =>
               Check.map
                 (fn bytes => (witver, String.implode (List.map Char.chr bytes)))
                 (Check.listOfLen progLen (Check.choose (0, 255)))))

      fun showSegwit (witver, prog) =
        "witver=" ^ Int.toString witver ^ " proglen=" ^ Int.toString (String.size prog)

      val () =
        Harness.check "prop: SegwitAddr encode/decode round-trips"
          (case Check.quickCheck
                  (Check.forAll genSegwit showSegwit
                     (fn (witver, prog) =>
                        case SegwitAddr.encode "bc" witver prog of
                            NONE => false
                          | SOME addr =>
                              (case SegwitAddr.decode addr of
                                   SOME {hrp, version, program} =>
                                     hrp = "bc" andalso version = witver andalso program = prog
                                 | NONE => false))) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      (* Format invariant: the data+checksum portion of a Bech32 address
         (everything after "hrp1") only ever uses the 32-symbol charset. *)
      val bech32Charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
      val () =
        Harness.check "prop: encoded data+checksum stays within the Bech32 charset"
          (case Check.quickCheck
                  (Check.forAll genHrpData showHrpData
                     (fn (hrp, data) =>
                        let
                          val addr = Bech32.encode hrp data
                          val tail = String.extract (addr, String.size hrp + 1, NONE)
                        in
                          List.all (fn c => Char.contains bech32Charset c) (String.explode tail)
                        end)) of
               Check.Passed _ => true
             | Check.Failed _ => false)

    in
      Harness.run ()
    end
end
