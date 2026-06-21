signature BECH32 = sig
  (* Encode hrp + 5-bit data words to bech32 string *)
  val encode    : string -> string -> string
  (* Decode bech32 string to {hrp, data} where data is 5-bit words as string *)
  val decode    : string -> {hrp: string, data: string} option
  (* Bech32m variant (constant 0x2bc830a3) *)
  val encodeM   : string -> string -> string
  val decodeM   : string -> {hrp: string, data: string} option
end

signature SEGWIT_ADDR = sig
  (* encode witness version (0-16) + witness program bytes -> bech32/bech32m address *)
  val encode : string -> int -> string -> string option
  (* decode bech32/bech32m address -> {hrp, version, program} *)
  val decode : string -> {hrp: string, version: int, program: string} option
end
