{ mkDerivation, base, lens, stdenv }:
mkDerivation {
  pname = "nix-haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base lens ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
