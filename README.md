# sml-bech32

Bech32 and Bech32m encoding with SegwitAddr support in pure Standard ML (BIP-173/350)

## Installation

```
smlpkg add github.com/sjqtentacles/sml-bech32
smlpkg sync
```

## Usage

```sml
(* Encode arbitrary data with a human-readable part *)
val encoded = Bech32.encode {hrp = "bc", dataWords = [...]}
val encoded_m = Bech32.encodem {hrp = "bc", dataWords = [...]}  (* Bech32m *)

(* Decode — returns NONE on invalid checksum or encoding *)
val SOME {hrp, dataWords} = Bech32.decode "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"

(* SegwitAddr: decode a Bitcoin/Lightning bech32 address *)
val SOME {hrp, version, program} =
  SegwitAddr.decode "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"
(* version = 0 (P2WPKH/P2WSH), version = 1 (Taproot/P2TR) *)

(* Encode a segwit address from witness version and program bytes *)
val addr = SegwitAddr.encode {hrp = "bc", version = 0, program = programBytes}
```

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
```

## License

MIT
