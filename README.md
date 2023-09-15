# unleash-client-haskell-core

This is a library for evaluating [Unleash](https://www.getunleash.io/) feature toggles. It currently meets all [client specifications per v4.2.2](https://github.com/Unleash/client-specification/releases/tag/v4.2.2).

The `Unleash` module provides functions and types for checking feature toggles and variants.

Does not include an HTTP client. See [unleash-client-haskell](https://github.com/finn-no/unleash-client-haskell) for a ready-to-use Haskell Unleash client SDK.

## Build

```
nix-build
```

## Run tests

```
# Fetch Unleash specification
git submodule init
git submodule update

nix-shell
cabal test
```

## Maintainers

- [Even Brenden](mailto:uch@anythingexternal.com)
- [Eirik Meland](mailto:eirik.meland@gmail.com)

## Dependencies

- aeson (BSD-3-Clause)
- aeson-pretty (BSD-3-Clause)
- attoparsec (BSD-3-Clause)
- bytestring (BSD-3-Clause)
- containers (BSD-3-Clause)
- hspec (MIT)
- murmur3 (MIT)
- random (BSD-3-Clause)
- text (BSD-2-Clause)
- text-show (BSD-3-Clause)
- time (BSD-2-Clause)
- versions (BSD-3-Clause)
