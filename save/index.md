regex
=====

_A Regular Expression Toolkit for regex-base_

regex extends regex-base with:

  * a text-replacement toolkit
  * special datatypes for many matches, first match and individual captures
  * compile-time checking of RE syntax
  * a unified means of controlling case-sensitivity and multi-line options
  * high-level AWK-like tools for building text processing apps
  * the option of using match operators with reduced polymorphism on the
    text and/or result types
  * regular expression macros including
      + a number of useful RE macros
      + a test bench for testing and documenting new macro environments
  * built-in support for the TDFA and PCRE backends
  * comprehensive documentation and copious examples


Road Map
--------

&nbsp;&nbsp;&#9745;&nbsp;**2017-01-26**&nbsp;&nbsp;0.0.0.1&nbsp;&nbsp;Pre-release (I)<br/>
&nbsp;&nbsp;&#9745;&nbsp;**2017-01-30**&nbsp;&nbsp;0.0.0.2&nbsp;&nbsp;Pre-release (II)<br/>
&nbsp;&nbsp;&#9744;&nbsp;**2017-02-06**&nbsp;&nbsp;0.0.1.0&nbsp;&nbsp;RFC<br/>
&nbsp;&nbsp;&#9744;&nbsp;**2017-02-20**&nbsp;&nbsp;0.1.0.0&nbsp;&nbsp;a candidate stable release<br/>
&nbsp;&nbsp;&#9744;&nbsp;**2017-03-20**&nbsp;&nbsp;1.0.0.0&nbsp;&nbsp;first stable release<br/>


Helping Out
-----------

If you have any feedback or suggestion then please drop me a line.

&nbsp;&nbsp;&nbsp;&nbsp;`t`: [@cdornan](https://twitter.com/cdornan)<br/>
&nbsp;&nbsp;&nbsp;&nbsp;`e`: chris.dornan@irisconnect.co.uk<br/>
&nbsp;&nbsp;&nbsp;&nbsp;`w`: https://github.com/iconnect/regex/issues


The Builds
----------

[![Hackage](badges/hackage.svg)](https://hackage.haskell.org/package/regex)
[![BSD3 License](badges/license.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)
[![Un*x build](badges/unix-build.svg)](https://travis-ci.org/iconnect/regex)
[![Windows build](badges/windows-build.svg)](https://ci.appveyor.com/project/engineerirngirisconnectcouk/regex/branch/master)
[![Coverage](badges/coverage.svg)](https://coveralls.io/github/iconnect/regex?branch=master)

For details of the current release and the build pipelines see the
**[Build Status](build-status)** page.


Installation Instructions
-------------------------

Either

```bash
cabal update && cabal install regex
```

or

```bash
stack install regex
```


Loading up the Tutorial into ghci
---------------------------------

First unpack the source distribution.
```bash
cabal unpack regex
cd regex-*
```

Loading the tutorial into ghci with cabal:
```
cabal configure
cabal repl re-tutorial
```

Loading the tutorial into ghci with stack:
```
stack --stack-yaml stack-8.0.yaml exec ghci -- -ghci-script lib/ghci examples/re-tutorial.lhs
```


Table of Contents
=================


## The Tutorial, Tests and Examples

### The Tutorial [examples/re-tutorial.lhs](re-tutorial.html)
provides an introduction to the package with simple examples that you can try
out in your favourite Haskell REPL and references to examples in the example
programs and library code.

### The Log Processor Example [examples/re-nginx-log-processor.lhs](re-nginx-log-processor.html)
provides an extended example of large-scale RE development with the regex test bench.

### The Include Processor Example [examples/re-include.lhs](re-include.html)
is the starting point for the preprocessor that we use to generate the tutorial
HTML and its derived test suite.

### The Cabal Processor Example [examples/re-gen-cabals.lhs](re-gen-cabals.html)
is the Sed preprocessor we use to generate our cabal file from the template
in [lib/regex-master.cabal](https://github.com/iconnect/regex/blob/master/lib/regex-master.cabal).

### The Library Tests [examples/re-tests.lhs](re-tests.html)
contains various test suites for exercising various components of the library.

### The Tutorial Preprocessor [examples/re-prep.lhs](re-prep.html)
contains the tool we use to generate the tutorial HTML and its derived test suite.

### The API Module Generator [examples/re-gen-modules.lhs](re-gen-modules.html)
contains a tool for generating the parts of the API that can be easily synthesized from a
seed/master module.


## Selected Library Modules

Some of the library modules have been prepared as literate programs for easy
browsing of their underlying source code.

### [Text.RE.Capture](Capture.html)
contains the definitions of the
`Matches`, `Match` and `Capture` data types (with helpers) that form the
foundations for everything else.

### [Text.RE.Replace](Replace.html)
contains the text-replacement toolkit.

### [Text.RE.Options](Options.html)
contains the `Options` types for controlling RE parsing and compilation.

### [Text.RE.IsRegex](IsRegex.html)
contains the IsRegex class for writing polymorphic regex tools that work with
all regex back ends and text type combinations.

### [Text.RE.TestBench](TestBench.html)
contains the test bench used to build the standard macro environment and can be
used for developing other macro environments with test and documentation.

### [Text.RE.Edit](Edit.html)
contains the polymorphic editing toolkit used by `Text.RE.Tools.Sed`.

### [Text.RE.Tools.Sed](Sed.html)
contains the Sed tool for building awk-like text processors.

### [Text.RE.Tools.Grep](Grep.html)
contains a simple grep tool for extracting lines that match a RE from a file.

### [Text.RE.Tools.Lex](Lex.html)
contains a simple scanning tool for building prototype scanners before being
discarded or converted into Alex scanners.

### [Text.RE.Internal.NamedCaptures](NamedCaptures.html)
is an internal library module for dealing with named captures in REs.
