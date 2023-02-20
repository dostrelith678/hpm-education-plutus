# System requirements for this course

To fully follow the course, you will need a running `cardano-node` instance on one of the testnets (preferably `preview`). The `cardano-node` instance is required to actually interact with and test the scripts you build. The other requirement is having an environment for writing and compiling Plutus scripts. Together, these two roughly add up to the following two minimal system requirements:

* working memory: 8GB RAM
* disk space: 60-70GB (nix store will take \~50GB for a Plutus dev environment)

### Setting up cardano-node

There are many ways to set up a `cardano-node`. This guide favours the use of the guild operators' guide: [https://cardano-community.github.io/guild-operators](https://cardano-community.github.io/guild-operators/). To avoid building the binaries, you can use the already compiled binaries from IOG (found on `cardano-node` release pages) instead.&#x20;
