%heading#cabalinstall Installing with Cabal

You know the drill:

```bash
cabal update && cabal install regex
```

%heading#stackinstall Installing with Stack

Choose your poison:

```bash
stack --stack-yaml stack-8.0.yaml install regex
```

or

```bash
stack --stack-yaml stack-7.10.yaml install regex
```

or

```bash
stack --stack-yaml stack-7.8.yaml install regex
```


%heading#cabaltutorial Loading the Tutorial with Cabal

First unpack the source distribution and change into the root folder
```bash
cabal unpack regex
cd regex-*
```

And load the tutorial into ghci with cabal:
```bash
cabal configure
cabal repl re-tutorial
```


%heading#stacktutorial Loading the Tutorial with Stack

To load the tutorial into ghc-8.0 with stack (from the unpacked root folder):
```bash
stack --stack-yaml stack-8.0.yaml exec ghci -- -ghci-script lib/ghci examples/re-tutorial.lhs
```


%heading#cabaltest Running the tests with Cabal

To run the tests with cabal, change into the root folder and:

```bash
cabal test
```


%heading#stacktest Running the tests with Stack

To test with GHC-8.0 from the root folder:
```bash
stack test --stack-yaml stack-8.0.yaml
```
