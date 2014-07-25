# RFC-1751

Convert between 128-bit bytestrings and mnemonic 12-word strings according to
the RFC-1751. This library was originally written for the Haskoin project, but
that one went on to implement a Bitcoin-specific scheme.

## Installation

```sh
cabal install RFC1751
```

## Functions

```haskell
keyToMnemonic  :: ByteString -> Maybe String
mnemonicToSeed :: String -> Maybe ByteString
```
