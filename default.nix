{ mkDerivation, async, base, contravariant, foldl, managed, mmorph
, pipes, pipes-concurrency, stdenv, transformers
}:
mkDerivation {
  pname = "mvc";
  version = "1.1.7";
  src = ./.;
  libraryHaskellDepends = [
    async base contravariant foldl managed mmorph pipes
    pipes-concurrency transformers
  ];
  description = "Model-view-controller";
  license = stdenv.lib.licenses.bsd3;
}
