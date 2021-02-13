# turing-machine

This package includes a library for a simple but complete Turing machine.
It also includes a console program to run a Turing machine which is read in by a yaml-file.

## How to use

```
turing-machine - a Turing Machine Simulator

Usage: turing-machine COMMAND [-s|--start-state START-STATE] [-q|--quiet] 
                      [-p|--print]
  Run or take a step in a Turing Machine

Available options:
  -s,--start-state START-STATE
                           Which state the Turing machine should start
                           in (default: "")
  -q,--quiet               Whether to be quiet
  -p,--print               Whether to print out the step/s
  -h,--help                Show this help text

Available commands:
  run                      Run the Turing machine through all steps
  step                     Take one step with the Turing machine
  count                    Run the Turing machine and count the number of ones;
                           Mostly for Busy Beavers or similar programs
```

### Structure of a Turing machine file
```yaml
---
state: "A"
table:
  A:
    0: ["1","R","B"]
    1: ["1","R","HALT"]
  B:
    0: ["1","L","B"]
    1: ["0","R","C"]
  C:
    0: ["1,"L,"C"]
    1: ["1","L","A"]
tape:
  - "00000000"
  - "0"
  - "00000000"
steps: 0
...
```

## How to install
### Using cabal
You need to download and install cabal or install it through your package manager of choice. Nix is a good choice.

Then you download the repository or clone it with git.

After unpacking or cloning from git you have to switch into the folder of turing-machine.

Then run `cabal new-install`.

```
$ > cd turing-machine
$ > cabal new-install
```

### Using stack
You need to install stack on your system. Either by downloading it or by your package manager of choice.

Then you download the repository like with cabal. You can also clone it with git.

Afterwards you go into the folder and run `stack setup` followed by `stack install`.

```
$ > cd turing-machine
$ > stack setup; stack install
```

### Using nix
You need to install stack on your system. Either by downloading it or by your package manager of choice.

Then you download the repository like with cabal. You can also clone it with git.

Afterwards you go into the folder and run `nix-build release.nix`.

```
$ > cd turing-machine
$ > nix-build release.nix
```

## Credits

- [Alan Turing](https://en.wikipedia.org/wiki/Alan_Turing) for the idea of the [Turing machine](https://en.wikipedia.org/wiki/Turing_machine)
