# Setting up your Plutus development environment

This guide will cover setting up the environment for writing and compiling Plutus scripts. It does not include any of the development components such as the PAB. The guide is written for the Ubuntu OS but uses Nix (as favoured by IOG) so it should be easily replicated on other systems.

To set up a Plutus environment, we need a `cabal.project` file that will specify the dependencies required to develop Plutus scripts. It is important to note that both the Plutus and plutus-apps repositories are constantly under development with new releases. Combined with releases of cardano-node and cardano-wallet, this may cause various complications with regards to compatibility between the components. Therefore, we will use the following repository as a reference, which seems to be a stable and reasonably up-to-date way to get started: [https://github.com/james-iohk/plutus-scripts](https://github.com/james-iohk/plutus-scripts). Besides providing us with the `cabal.project` template, it also contains various Plutus scripts which serve as a good reference when learning.

In particular, the `cabal.project` file specifies a commit in the `plutus-apps` repository along with its compatible versions of dependencies such as `cardano-node` and `cardano-wallet` that we can use to build our `nix-shell` and start writing and compiling Plutus scripts.

Here are step-by-step instructions on how to get started:

* install Nix [https://nixos.org/download.html](https://nixos.org/download.html) (after installation we need to reload the terminal session or open a new one in order to have `nix` in our `$PATH`)
* setup Nix cache: add the following lines to `/etc/nix/nix.conf` (from [https://github.com/input-output-hk/plutus-apps/blob/main/CONTRIBUTING.adoc](https://github.com/input-output-hk/plutus-apps/blob/main/CONTRIBUTING.adoc)):

```
substituters = https://cache.zw3rk.com https://cache.iog.io https://cache.nixos.org/
trusted-public-keys = loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
extra-experimental-features = nix-command flakes
```

* clone the `plutus-apps` repository: [https://github.com/input-output-hk/plutus-apps](https://github.com/input-output-hk/plutus-apps)
* clone the `plutus-scripts` repository as well: [https://github.com/james-iohk/plutus-scripts](https://github.com/james-iohk/plutus-scripts)&#x20;
* find the `plutus-apps` commit hash from its `plutus-scripts/cabal.project` file
* checkout to the specified commit inside `plutus-apps`, e.g.: &#x20;

`git checkout 65ddfa5d467ed64f8709d7db9faf96151942da82`

* enter a `nix-shell` from inside the `plutus-apps` repository: `nix-shell` (for verbose output `nix-shell -vvv`)
* navigate back to `plutus-scripts` directory where `cabal.project` is located and run `cabal update`
* you should now be able to successfully run `cabal build` - this will compile all the script examples from the `plutus-scripts` repository which means your dev environment works - you can compile Plutus scripts!
* you should also be able to use `cabal repl` which gives you an interactive GHCi REPL in which you can use PlutusTx

Before that, we will go over some `cabal.project` references and `my-project.cabal` setup.
