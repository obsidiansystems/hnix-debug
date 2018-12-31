{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, hackGet, ... }: {
  packages = {
    hnix = hackGet ./dep/hnix;
    # Cabal snapshot too old:
    megaparsec = hackGet ./dep/megaparsec;
    hspec-megaparsec = hackGet ./dep/hspec-megaparsec;
    modern-uri = hackGet ./dep/modern-uri;
  };
  overrides = with pkgs.haskell.lib; self: super: if super.ghc.isGhcjs or false then {
    hnix = dontCheck super.hnix;
    Glob = dontCheck super.Glob;
    conduit = dontCheck super.conduit;
    yaml = dontCheck super.yaml;
    hpack = dontCheck super.hpack;
  } else {
    hnix = overrideCabal super.hnix (attrs: {
      testHaskellDepends = (attrs.testHaskellDepends or []) ++
        [ pkgs.nix

          # Use the same version of hpack no matter what the compiler version
          # is, so that we know exactly what the contents of the generated
          # .cabal file will be. Otherwise, Travis may error out claiming that
          # the cabal file needs to be updated because the result is different
          # that the version we committed to Git.
          (justStaticExecutables pkgs.haskell.packages.ghc843.hpack)
          (justStaticExecutables pkgs.haskell.packages.ghc843.criterion)
        ];
    });
  };
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
})
