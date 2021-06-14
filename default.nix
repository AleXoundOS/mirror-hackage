{ # Fetch the latest haskell.nix and import its default.nix
  haskellNix ? import (builtins.fetchTarball { url = "https://github.com/input-output-hk/haskell.nix/archive/01003480bdee1717b95ca99049d283168b405925.tar.gz"; sha256 = "06j381kix405k3x8yqlccwqckjrc3qrdqchbbd80cd6slf6ybg9c"; }) {}

# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003

# haskell.nix provides some arguments to be passed to nixpkgs, including some
# patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs

# import nixpkgs with overlays
, pkgs ? import nixpkgsSrc nixpkgsArgs
}: pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "mirror-hackage";
    src = ./.;
  };
}
