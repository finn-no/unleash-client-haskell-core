# unleash-client-haskell-core

This is the core functionality for an [Unleash](https://www.getunleash.io/) Haskell client. It currently meets all [client specificions per v4.2.2](https://github.com/Unleash/client-specification/releases/tag/v4.2.2) and exports:

- `Unleash`: Data structures and functions for getting features and variants.
- `Unleash.HttpClient`: HTTP client for interacting with an Unleash server.

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

- [Eirik Meland](mailto:eirik.meland@gmail.com)
- [Even Brenden](mailto:evenbrenden@gmail.com)

## Dependencies

- aeson (BSD-3-Clause)
- aeson-pretty (BSD-3-Clause)
- attoparsec (BSD-3-Clause)
- bytestring (BSD-3-Clause)
- containers (BSD-3-Clause)
- hspec (MIT)
- http-client (MIT)
- http-media (MIT)
- murmur3 (MIT)
- random (BSD-3-Clause)
- servant (BSD-3-Clause)
- servant-client (BSD-3-Clause)
- text (BSD-2-Clause)
- text-show (BSD-3-Clause)
- time (BSD-2-Clause)
- versions (BSD-3-Clause)

## Things to do

- [ ] Create an `unleash-client-haskell-example`.

- [ ] FOSS it and add to Hackage and Unleash docs.
