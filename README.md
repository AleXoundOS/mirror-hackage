# mirror hackage program

Download hackage contents with the purpose of mirroring with http server.

### input:
- path of [hackage.json](https://github.com/input-output-hk/hackage.nix/blob/master/hackage.json)
- path which is a base for downloaded files
- path of excluded packages list file

_All inputs have default values, refer to `mirror-hackage --help`_.

### output:
- files and directories, preserving original url structure, containing:
  - packages archives
  - all revisions (metadata) of cabal package descriptions
  - [_TODO: html pages_](https://github.com/AleXoundOS/mirror-hackage/issues/1)

## build

### nix
```console
nix-build -A mirror-hackage.components.exes.mirror-hackage
```
The built binary gets available in `./result/bin/mirror-hackage`.

### stack

```console
$ stack build
```
The built executable gets available as `stack exec -- mirror-hackage`.

## usage

Get recent [hackage.json](https://github.com/input-output-hk/hackage.nix/blob/master/hackage.json).
Run `mirror-hackage` provided with `hackage.json` in the current directory or passed
via `--hackage-json` command line option.

Download can be stopped (`Ctrl+C`) at any moment and resumed later.

## stats

As of 2020-07-06 running `mirror-hackage` means downloading of 251k files, 13 GiB.
