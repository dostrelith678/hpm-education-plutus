# Setting up your Plutus development environment

This guide will cover setting up the environment for writing and compiling Plutus scripts. It does not include any of the development components such as the PAB. The guide is written for the Ubuntu OS but uses Nix (as favoured by IOG) so it should be easily replicated on other systems.

To set up a Plutus environment, we need a `cabal.project` file that will specify the dependencies required to develop Plutus scripts. It is important to note that both the [plutus](https://github.com/intersectMBO/plutus) and [plutus-apps](https://github.com/intersectMBO/plutus-apps) repositories are constantly under development with new releases. Combined with releases of cardano-node and cardano-wallet, this may cause various complications with regard to compatibility between the components.

Therefore, we will use the following repository as a reference, which seems to be a stable and reasonably up-to-date way to get started: [https://github.com/james-iohk/plutus-scripts](https://github.com/james-iohk/plutus-scripts). Besides providing us with the `cabal.project` template, it also contains various Plutus scripts which serve as a good reference when learning.

In particular, the `cabal.project` file specifies a commit in the `plutus-apps` repository along with its compatible versions of dependencies such as `cardano-node` and `cardano-wallet` that we can use to build our `nix-shell` and start writing and compiling Plutus scripts.

Here are step-by-step instructions on how to get started:

* Install Nix: the Package Manager from [https://nixos.org/download.html](https://nixos.org/download.html) (after installation we need to reload the terminal session or open a new one in order to have `nix` in our `$PATH`).
* Configure Nix cache: Add the following lines to `/etc/nix/nix.conf` (from [https://github.com/intersectMBO/plutus-apps/blob/main/CONTRIBUTING.adoc](https://github.com/intersectMBO/plutus-apps/blob/main/CONTRIBUTING.adoc)):

```nix
substituters = https://cache.zw3rk.com https://cache.iog.io https://cache.nixos.org/
trusted-public-keys = loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
extra-experimental-features = nix-command flakes
```

* After editing the Nix configuration file `nix.conf`, we need to restart the Nix daemon to apply the changes. We can send a SIGKILL signal to trigger a restart:

```
sudo pkill nix-daemon
```

* Clone the `plutus-apps` repository: [https://github.com/intersectMBO/plutus-apps](https://github.com/intersectMBO/plutus-apps):

```git
git clone https://github.com/intersectMBO/plutus-apps
```

* Clone the `plutus-scripts` repository as well: [https://github.com/james-iohk/plutus-scripts](https://github.com/james-iohk/plutus-scripts):

```git
git clone https://github.com/james-iohk/plutus-scripts
```

* Find the `plutus-apps` commit hash from its `plutus-scripts/cabal.project` file
* Checkout to the specified commit inside `plutus-apps`, e.g.:

```bash
cd plutus-apps
git checkout 65ddfa5d467ed64f8709d7db9faf96151942da82
```

* Enter a `nix-shell` from inside the `plutus-apps` repository: `nix-shell` (for verbose output `nix-shell -vvv`). With verbose output, we should see some cache references in the output if we correctly configured the cache config, e.g.:

```bash
finished download of 'https://cache.zw3rk.com/nar/159x1j930z2fs9frii74fsanza6h0hg0h35i2q825nj3qa43gp13.nar.zst'; curl status = 0, HTTP status = 200, body = 44600355 bytes
```

* Now that we are inside a Nix shell created from the `plutus-apps` repo, navigate back to `plutus-scripts` directory where `cabal.project` is located and run `cabal update`:

```
cabal update
```

* We should now be able to run `cabal build` - this will compile all the script examples from the `plutus-scripts` repository which means your development environment works - you can compile Plutus scripts!

```
cabal build
```

* We should also be able to use `cabal repl` which gives you an interactive GHCi REPL in which you can use PlutusTx.

```
cabal repl
```

Before that, we will go over the project files `cabal.project` and `plutus-scripts.cabal`.
