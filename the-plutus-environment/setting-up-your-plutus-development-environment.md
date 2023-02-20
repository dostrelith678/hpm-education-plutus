# Setting up your Plutus development environment

This guide will cover setting up the minimal environment for writing and compiling Plutus scripts. It does not include any of the development components such as the PAB. The guide is written for the Ubuntu OS but uses Nix (as favoured by IOG) so it should be easily replicated on other systems.

To set up a Plutus environment, we need a `cabal.project` file that will specify the dependencies required to develop Plutus scripts. For our `cabal.project` reference, we are using the following repository: [https://github.com/james-iohk/plutus-scripts](https://github.com/james-iohk/plutus-scripts). I have found it to be the most up-to-date reference for Plutus with respect to new `cardano-node` versions and `plutus-apps` repository updates.

In particular, the `cabal.project` file specifies a commit in the `plutus-apps` repository that we can use to build our `nix-shell`, and this will match the latest node version most of the time. In addition, the `plutus-scripts` repository contains a number of script examples that serve as very useful references.

* install Nix [https://nixos.org/download.html](https://nixos.org/download.html) (after installation we need to refresh the terminal session)
* setup Nix cache: add the following lines to `/etc/nix/nix.conf` (from [https://github.com/input-output-hk/plutus-apps/blob/main/CONTRIBUTING.adoc](https://github.com/input-output-hk/plutus-apps/blob/main/CONTRIBUTING.adoc)):

```
substituters = https://cache.zw3rk.com https://cache.iog.io https://cache.nixos.org/
trusted-public-keys = loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
extra-experimental-features = nix-command flakes
```

* clone the `plutus-apps` repository: [https://github.com/input-output-hk/plutus-apps](https://github.com/input-output-hk/plutus-apps)
* clone the `plutus-scripts` repository as well: [https://github.com/james-iohk/plutus-scripts](https://github.com/james-iohk/plutus-scripts)&#x20;
* find the `plutus-apps` commit hash from its `plutus-scripts/cabal.project` file
* checkout to the specified commit inside `plutus-apps`: `git checkout 890476...`
* enter a `nix-shell` from inside the `plutus-apps` repository: `nix-shell` (for verbose output `nix-shell -vvv`)
* navigate back to `plutus-scripts` directory where `cabal.project` is located and run `cabal update`
* you should now be able to successfully run `cabal build` - this will compile all the script examples from that repo
* you should also be able to use `cabal repl` which gives you an interactive GHCi REPL in which you can use Plutus Tx

Before that, we will go over some `cabal.project` references and `my-project.cabal` setup.
