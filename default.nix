{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
let
  pkgs = import <nixpkgs> {};
  scrappyPkg = pkgs.fetchFromGitHub {
    owner = "Ace-Interview-Prep";
    repo = "scrappy";
    rev = "tag";
    sha256 = "17s66x4njr6dm8l32gcbcdf63rq9xxlhjlc7h3pnb6yysz7lfycr";
  };
in
with obelisk;
project ./. ({ ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  # packages = scrappyPkg;
  # overrides = haskellPackagesNew: haskellPackagesOld: rec {
    # scrappy = nixpkgs.haskell.lib.dontCheck scrappy;
  # };
  # overrides = haskellPackagesNew: haskellPackagesOld: rec {
    # scrappy = scrappy;
  # };
  overrides =
    self: super:
    let
      scrappySrc = pkgs.fetchFromGitHub {
        owner = "Ace-Interview-Prep";
        repo = "scrappy";
        rev = "tag";
        sha256 = "17s66x4njr6dm8l32gcbcdf63rq9xxlhjlc7h3pnb6yysz7lfycr";
      };
    in
      {
        scrappy = self.callCabal2nix "scrappy" scrappySrc {};
      };
    
  # overrides = haskellPackagesNew: hpOld: rec {
    # scrappy = nixpkgs.haskell.lib.dontCheck (nixpkgs.fetchFromGitHub {
      # owner = "Ace-Interview-Prep";
      # repo = "scrappy";
      # rev = "tag";
      # sha256 = "17s66x4njr6dm8l32gcbcdf63rq9xxlhjlc7h3pnb6yysz7lfycr";
    # });
  # };
  withHoogle = true;
})
