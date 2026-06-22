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

## Example

`make example` builds and runs [`examples/demo.sml`](examples/demo.sml), which
decodes and re-encodes fixed BIP-173 / BIP-350 segwit addresses and round-trips
raw bech32 data (witness programs shown in hex):

```
$ make example
P2WPKH mainnet (BIP-173, bech32 v0):
  address = bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4
  hrp     = bc
  version = 0
  program = 751e76e8199196d454941c45d1b3a323f1433bd6
  re-encode = bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4

P2TR mainnet (BIP-350, bech32m v1):
  address = bc1p5cyxnuxmeuwuvkwfem96lqzszd02n6xdcjrs20cac6yqjjwudpxqkedrcr
  hrp     = bc
  version = 1
  program = a60869f0dbcf1dc659c9cecbaf8050135ea9e8cdc487053f1dc6880949dc684c
  re-encode = bc1p5cyxnuxmeuwuvkwfem96lqzszd02n6xdcjrs20cac6yqjjwudpxqkedrcr

Raw bech32 round-trip (hrp="test"):
  data (hex) = 0001020304050607
  encoded    = test1qpzry9x83k7jf6
  decode ok  = true
```

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
make example    # build + run the demo
```

## License

MIT
