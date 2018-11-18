{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, hackGet, ... }: {
  packages = {
    hnix = hackGet ./dep/hnix;
  };
  overrides = with pkgs.haskell.lib; self: super: if super.ghc.isGhcjs or false then {
    hnix = dontCheck super.hnix;
    Glob = dontCheck super.Glob;
    conduit = dontCheck super.conduit;
    yaml = dontCheck super.yaml;
    hpack = dontCheck super.hpack;
  } else {};
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
})
