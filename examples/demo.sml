(* demo.sml - decode and re-encode fixed BIP-173 / BIP-350 segwit addresses and
   round-trip raw bech32 data. Deterministic: same output on every run and
   compiler (no RNG, no clock; witness programs shown in hex). *)

fun hex s =
  let val d = "0123456789abcdef"
  in String.concat (List.map
       (fn c => let val b = Char.ord c
                in String.implode [String.sub (d, b div 16), String.sub (d, b mod 16)] end)
       (String.explode s))
  end

fun showSegwit label addr =
  ( print (label ^ ":\n")
  ; print ("  address = " ^ addr ^ "\n")
  ; case SegwitAddr.decode addr of
      SOME {hrp, version, program} =>
        ( print ("  hrp     = " ^ hrp ^ "\n")
        ; print ("  version = " ^ Int.toString version ^ "\n")
        ; print ("  program = " ^ hex program ^ "\n")
        ; print ("  re-encode = "
                 ^ (case SegwitAddr.encode hrp version program
                      of SOME a => a | NONE => "<encode failed>") ^ "\n") )
    | NONE => print "  <decode failed>\n" )

(* BIP-173 P2WPKH mainnet (bech32, witness v0) *)
val () = showSegwit "P2WPKH mainnet (BIP-173, bech32 v0)"
           "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"

(* BIP-350 P2TR mainnet (bech32m, witness v1) *)
val () = print "\n"
val () = showSegwit "P2TR mainnet (BIP-350, bech32m v1)"
           "bc1p5cyxnuxmeuwuvkwfem96lqzszd02n6xdcjrs20cac6yqjjwudpxqkedrcr"

(* Raw bech32 encode/decode round-trip of fixed 5-bit data words *)
val () = print "\nRaw bech32 round-trip (hrp=\"test\"):\n"
val data = String.implode (List.map Char.chr [0,1,2,3,4,5,6,7])
val encoded = Bech32.encode "test" data
val () = print ("  data (hex) = " ^ hex data ^ "\n")
val () = print ("  encoded    = " ^ encoded ^ "\n")
val () = print ("  decode ok  = "
                ^ (case Bech32.decode encoded
                     of SOME {hrp, data=d} => Bool.toString (hrp = "test" andalso d = data)
                      | NONE => "false") ^ "\n")
