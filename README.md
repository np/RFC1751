# RFC-1751

Convert between 128-bit bytestrings and mnemonic 12-word strings according to
RFC-1751. This library was originally written for the Haskoin project, but
then replaced there by a Bitcoin-specific scheme.

## Installation

```shell
cabal install RFC1751
```

## Functions

```haskell
keyToMnemonic :: ByteString -> Maybe String
mnemonicToKey :: String -> Maybe ByteString
```
