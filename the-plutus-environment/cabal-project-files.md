# Cabal project files

A cabal project normally consists of two configuration files:

1. `<project_name>.cabal` - defines the project metadata.
2. `cabal.project` - defines the build configuration options of the project.

### \<project\_name>.cabal

[_Reference_](https://cabal.readthedocs.io/en/3.10/cabal-projectindex.html)

Let's start with the `project.cabal` file. This file is named as `<project_name>.cabal`. We will use the `plutus-scripts` files as examples so this file is `plutus-scripts.cabal`. The first line of the file states the `Cabal-Version` of the project which is the specification version that this package description uses. The following lines contain some basic metadata about the project (author, licence, etc...):

```haskell
# plutus-scripts/plutus-scripts.cabal

Cabal-Version:      2.4
Name:               plutus-script
Version:            0.1.0.0
Author:             James Browning
Maintainer:         james.browning@iohk.io
Build-Type:         Simple
Copyright:          © 2022 James Browning
License:            Apache-2.0
License-files:      LICENSE
```

After that, we get the definition of the project type. A project can be a `library` or an `executable`. It can also be both of those. The `plutus-scripts` project is a `library` with source code located in the `src` directory and exposes the listed modules:

```haskell
...
library
  hs-source-dirs:      src
  exposed-modules:     UntypedHelloWorld
                     , TypedDatumRedeemer42
                     , Deadline
                     ...
```

The next section `build-depends` specifies the _library_ dependencies required to build the project. Dependencies can have specific versions specified:

```haskell
...
    build-depends:     aeson
                     , base ^>=4.14.1.0
                     , bytestring
                     , containers
                     ...
```

The following two options `default-language` and `ghc-options` specify the language specification to use (in this case `Haskell2010`), and the GHC options for the compiler to use when building the project:

```haskell
...
  default-language:    Haskell2010
  ghc-options:         -Wall -fobject-code -fno-ignore-interface-pragmas ...
```

### cabal.project&#x20;

[_Reference_](https://cabal.readthedocs.io/en/stable/cabal-project.html)

The `cabal.project` file supports a variety of options that configure the details of your build. Perhaps most importantly, it specifies dependencies which are not on Hackage. In `plutus-scripts`, the first definition in the file is for the [Cardano Haskell package repository](https://intersectMBO.github.io/cardano-haskell-packages/) which aims to be the central package repository for Cardano-related packages not found on Hackage.

```haskell
-- Custom repository for cardano haskell packages
-- See https://github.com/intersectMBO/cardano-haskell-packages on how to use CHaP in a Haskell project.
repository cardano-haskell-packages
  url: https://intersectMBO.github.io/cardano-haskell-packages
  secure: True
  root-keys:
    3e0cce471cf09815f930210f7827266fd09045445d65923e6d0238a6cd15126f
    443abb7fb497a134c343faf52f0b659bd7999bc06b7f63fa76dc99d631f9bea1
    a86a1f6ce86c449c46666bda44268677abf29b5b2d2eb5ec7af903ec2f117a82
    bcec67e8e99cabfa7764d75ad9b158d72bfacf70ca1d0ec8bc6b4406d1bf8413
    c00aae8461a256275598500ea0e187588c35a5d5d7454fb57eac18d9edb86a56
    d4a35cd3121aa00d18544bb0ac01c3e1691d618f462c46129271bccf39f7e8ee
```

This instructs `cabal` to look for packages in that repository as well. The `packages` field specifies the list of package locations that contain the local packages to be built by this project. The `index-state` field instructs `cabal` to look for packages in repositories (Hackage/CHaP etc.) with the state of those repositories as it was at the given time.

```haskell
...

packages: plutus-scripts.cabal

index-state: 2022-11-14T00:20:02Z

index-state:
  , hackage.haskell.org 2022-11-14T00:20:02Z
  , cardano-haskell-packages 2022-11-17T04:56:26Z
  
...
```

The `constraints` field defines specific version constraints and can also specify flag settings.

```haskell
...

constraints:
  -- cardano-prelude-0.1.0.0 needs
  , protolude <0.3.1

  -- cardano-ledger-byron-0.1.0.0 needs
  , cardano-binary <1.5.0.1

...
```

A `source-repository-package` field provides a location from where to build a certain dependency that is not found on central repositories such as Hackage or CHaP, as is the case here for `plutus-apps` packages.

```haskell
...

source-repository-package
  type: git
  location: https://github.com/intersectMBO/plutus-apps.git
  tag: 65ddfa5d467ed64f8709d7db9faf96151942da82
  subdir:
    cardano-streaming
    doc
    freer-extras
    marconi
    marconi-mamba
    playground-common
    pab-blockfrost
    plutus-chain-index
    plutus-chain-index-core
    plutus-contract
    plutus-contract-certification
    plutus-example
    plutus-ledger
    plutus-ledger-constraints
    plutus-pab
    plutus-pab-executables
    plutus-script-utils
    plutus-tx-constraints
    plutus-use-cases
    rewindable-index

...
```
