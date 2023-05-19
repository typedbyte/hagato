{ pkgs ? import <nixpkgs> {} }:
  let
    pkgDeps = with pkgs; [
      cabal-install
      haskell.compiler.ghc943
      libGL
      pkg-config
      shaderc
      vulkan-loader
      vulkan-validation-layers
      zlib
    ];
    xorgDeps = with pkgs.xorg; [
      libX11
      libXi
      libXinerama
      libXrandr
      libXxf86vm
      libXcursor
      libXext
    ];
  in
    pkgs.mkShell {
      nativeBuildInputs = pkgDeps ++ xorgDeps;
    }