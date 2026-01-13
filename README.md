# WingRiders Launchpad

## Setting up

You probably want to tweak your local cabal settings for faster compilation and dynamic linking:
```sh
  cabal configure --disable-optimization --ghc-options="-j +RTS -A128m -n2m -RTS" --disable-library-vanilla --enable-executable-dynamic
```
Note that this degrades runtime performance (relevant for integration tests), so we don't do that on CI.

### Cachix binary caches
- Install the cachix client from nix shell
  ```bash
  nix-env -iA cachix -f https://cachix.org/api/v1/install
  ```
- Add IOHK cache
  ```
  cachix use iohk
  ```

## Build

```bash
make shell # Enter the nix shell
make build # Run build
make test # Run tests
make export # Export the contracts
```

## Contracts

Contracts are defined in `src/`.

- Compiled scripts are in `artifacts/*.plutus`;
- Contract parameters and constants are in `artifacts/export-info.json`

### Traces
By default all contracts are stripped of the error messages, if you want them in, please set the `CONTRACTS_TRACING` env variable to true before running the code.

