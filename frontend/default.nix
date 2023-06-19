{ pkgs
, system
}:

let
  mkDerivation =
    { srcs ? ./elm-srcs.nix
    , src
    , name
    , registryDat ? ./registry.dat
    }:
    pkgs.stdenv.mkDerivation {
      inherit name src system;

      buildInputs = [
        pkgs.elmPackages.elm
        pkgs.nodePackages.uglify-js
      ];

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import srcs;
        elmVersion = "0.19.1";
        inherit registryDat;
      };

      installPhase =
        ''
          echo "compiling src/Main.elm"
          elm make src/Main.elm --output $out/main.js

          echo "minifying src/Main.elm"
          uglifyjs $out/main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
              | uglifyjs --mangle --output $out/main.min.js

          echo "replacing unminified main.js with the minified one"
          mv $out/main.min.js $out/main.js

          echo "copying over index.{html,js}"
          cp src/index.{html,js} $out

          echo "copying vendored deps"
          cp -r vendor/*.js $out
        '';
    };
in
mkDerivation {
  name = "sdc-map-frontend-0.1.0";
  srcs = ./elm-srcs.nix;
  src = ./.;
}
