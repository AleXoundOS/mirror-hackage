# mirror hackage program

Download hackage contents with the purpose of mirroring with http server.

Input:
- [hackage.json](https://github.com/input-output-hk/hackage.nix/blob/master/hackage.json) file path
- base path for downloaded files

Output:
- directories and files, preserving original url structure, containing:
  - packages archives
  - all revisions (metadata) of cabal package descriptions
  - _TODO: html pages_

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
Run `mirror-hackage` provided `hackage.json` in the current directory or passed
via `--hackage-json` command line option.

Download can be stopped (`Ctrl+C`) at any moment and resumed after.

## stats

As of 2020-07-06 running `mirror-hackage` means downloading of 251211 files.
