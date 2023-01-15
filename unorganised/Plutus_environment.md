# What is Plutus?

Plutus is generally referred to as the smart contract language for Cardano. But IOG engineers more often refer to it as the *Plutus Platform*, which is made up of several different parts that enable writing applications that interact with the Cardano blockchain and take advantage of the validator script capabilities of the [EUTxO model](linkhere).

The Plutus Platform can therefore be separated into two main parts:
**1) The Plutus Foundation**
   The Plutus Foundation provides the ledger with a way of specifying and executing scripts. This is done through a programming language called *Plutus Core*. Plutus Core is a small low-level functional programming language that acts as an on-chain "assembly language". Because it is so low-level, it is not meant to be written by developers. Instead, it is a compilation target. The idea being to write Haskell code that then gets compiled down to Plutus Core for on-chain execution. This is where *Plutus Tx* comes in as it provides a mechanism for compiling Haskell to Plutus Core. Plutus Tx contains the libraries and the compiler for such compilations, and their results form the on-chain parts of smart contract applications.

**2) The Plutus Application Framework**
  The Plutus Application Framework provides support tools for writing applications in Plutus. It consists of several different tools such as:
    - *Plutus Playground* - an online development environment featuring a code editor, a graphical user interface and evaluation visualisation. maybe link here
    - *Contract API* - a component that provides an interface for writing the off-chain parts of Plutus applications. A useful tool that goes along is the *Contract monad emulator* that is used to emulate a blockchain for testing contract instances.
    - *Plutus Application Backend (PAB)* - a web server library that manages the state of Plutus contract instances and executes the off-chain components of Plutus applications. It currently does so by interacting with the cardano-wallet backend and caradno-node components while providing a client/application interface for the Plutus application. Not production ready (only hosted option available).
   - *Various other libraries* - to provide a full framework for writing Plutus application.
  
  All these tools are located in the plutus-apps repository.
  NOTE: All these tools are under heavy development and may not be production-ready. 

* Could add the overview figure here of the Plutus Platform


## System requirements for this course

To fully follow the course, you will need a running `cardano-node` instance on one of the testnets (preferably `preview`). The `cardano-node` instance is required to actually interact with and test the scripts you build. The other requirement is having an environment for writing and compiling Plutus scripts. Together, these two roughly add up to the following two minimal system requirements:
  - working memory: 8GB RAM
  - disk space:  60-70GB (nix store will take ~50GB for a Plutus dev envirnoment) note this is the minimum


## Optional setup ZRAM (ZFS)

## Setting up cardano-node

There are many ways to set-up a `cardano-node`. This guide favours the use of the guild operators' guide: https://cardano-community.github.io/guild-operators/. To avoid building the binaries, you can use the already compiled binaries from IOG (found on `cardano-node` release pages) instead. The guide also mentions this procedure in the *Node & CLI* section.

## Setting up your Plutus development environment

This guide will cover setting up the minimal environment for writing and compiling Plutus scripts. It does not include any of the development components such as the PAB. The guide is written for the Ubuntu OS, but uses Nix (as favoured by IOG) so it should be easily replicated on other systems.

To set up a Plutus environment, we need a `cabal.project` file that will specify dependencies required to develop Plutus scripts. For our `cabal.project` reference, we are using the following repository: https://github.com/james-iohk/plutus-scripts. I have found it to be the most up-to-date reference for Plutus with respect to new node versions and `plutus-apps` repository updates. In particular, the `cabal.project` file specifies a commit in the `plutus-apps` repository (https://github.com/input-output-hk/plutus-apps) that we can use to build our `nix-shell`, and this will match the latest node version most of the time.
In addition, the `plutus-scripts` repository contains a number of script examples that serve as a very useful reference.

- install Nix https://nixos.org/download.html (after install need refresh session)
- setup Nix cache: add the following lines to `/etc/nix/nix.conf`: (trusted public key link not updated in https://github.com/input-output-hk/plutus-apps#how-to-set-up-the-iohk-binary-caches)
```
substituters        = https://cache.iog.io https://cache.nixos.org/
trusted-public-keys = cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
extra-experimental-features = nix-command flakes
```
    -> This does seem to be working fine

- clone the `plutus-apps` repository: https://github.com/input-output-hk/plutus-apps
- optionally clone the `plutus-scripts` repository as well: https://github.com/james-iohk/plutus-scripts (or just find the `plutus-apps` commit hash from its `cabal.project` file)
- checkout to the specified commit inside `plutus-apps`: `git checkout 890476...`
- enter a `nix-shell` from inside the `plutus-apps` repository: `nix-shell`
- navigate to the `cabal.project` directory and run `cabal update` and then `cabal repl` or `cabal build` should work fine and should be able to use Plutus Tx in repl (I mean what do we put in our cabal.project file for starting? We can just copy the plutus-scripts one and specify the packages differently.)

Before that we will go over some `cabal.project` references and `my-project.cabal` setup.