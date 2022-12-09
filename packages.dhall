let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221209/packages.dhall
        sha256:b55c24bf585df4041ae6e87124cab7b35d474a9a77c9c3862ee6bf1dae618d72

in  upstream
  with fast-vect.version = "45177a6d51806fc6a293dcd94f67231a772726d3"
  with fast-vect.repo = "https://github.com/artemisSystem/purescript-fast-vect"
