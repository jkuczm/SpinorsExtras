# Spinors Extras

Mathematica implementation of massive spinor-helicity formalism.

[![latest release](http://img.shields.io/github/release/jkuczm/SpinorsExtras.svg)](https://github.com/jkuczm/SpinorsExtras/releases/latest)
[![semantic versioning](http://jkuczm.github.io/media/images/SemVer-2.0.0-brightgreen.svg)](http://semver.org/spec/v2.0.0.html)
[![license: MIT](http://jkuczm.github.io/media/images/license-MIT-blue.svg)](https://github.com/jkuczm/SpinorsExtras/blob/master/LICENSE)

* [Features](#features)
* [Installation](#installation)
    * [Automatic installation](#automatic-installation)
    * [Manual installation](#manual-installation)
* [Documentation](#documentation)
* [Tests](#tests)
* [Bugs and requests](#bugs-and-requests)
* [Contributing](#contributing)
* [License](#license)
* [Versioning](#versioning)



## Features

Implementation of massive spinor-helicity formalism in Mathematica on top of
[S@M package](http://www.slac.stanford.edu/~maitreda/Spinors/).

Spinors Extras package extends S@M capabilities by introducing
symbols representing states of massive fermions: massive spinors, and vector
bosons: polarization vectors along with their properties, relations between
them and functions to manipulate and interchange them within the Mathematica
code.

Package provides also tools for management of reference vectors that
determine the quantization axes, proportionality relation among spinors and
various phase conventions for spinors and polarization vectors.

Package is well suited for calculation of helicity amplitudes involving massive
particles using recursive (including on-shell) methods, Feynman diagrams, or
other techniques employing massive spinor helicity formalism, but some of
provided tools are also useful for easier manipulations of purely massless
amplitudes.


For simple usage example see
[tutorial on calculating e μ → e μ amplitude](http://www.fuw.edu.pl/~jkuczm/SpinorsExtras/reference/tutorial/QEDWithMuons.html).


## Installation

Since Spinors Extras is built on top of
[S@M package](http://www.slac.stanford.edu/~maitreda/Spinors/)
you need to install it first.


### Automatic installation

To install Spinors Extras package automatically evaluate:
```Mathematica
Get["https://raw.githubusercontent.com/jkuczm/SpinorsExtras/master/BootstrapInstall.m"]
```

Note that this will also install dependencies:
[EvaluationUtilities](https://github.com/jkuczm/MathematicaEvaluationUtilities),
[MessagesUtilities](https://github.com/jkuczm/MathematicaMessagesUtilities),
[OptionsUtilities](https://github.com/jkuczm/MathematicaOptionsUtilities),
[PatternUtilities](https://github.com/jkuczm/MathematicaPatternUtilities),
[ProtectionUtilities](https://github.com/jkuczm/MathematicaProtectionUtilities),
[StringUtilities](https://github.com/jkuczm/MathematicaStringUtilities),
[MUnitExtras](https://github.com/jkuczm/MUnitExtras) and
[ProjectInstaller](https://github.com/lshifr/ProjectInstaller) package, if you
don't have it already installed.


### Manual installation

1. Download latest released
   [SpinorsExtrasWithDependencies.zip](https://github.com/jkuczm/SpinorsExtras/releases/download/v1.0.1/SpinorsExtrasWithDependencies.zip)
   file.

2. Extract downloaded `SpinorsExtrasWithDependencies.zip` to any directory
   which is on Mathematica `$Path`, e.g. to one obtained by evaluating
   `FileNameJoin[{$UserBaseDirectory,"Applications"}]`.



## Documentation

Spinors Extras package comes with documentation integrated with Mathematica
Documentation Center. To use it, open the documentation center
(`Help` > `Documentation Center`) and type "SpinorsExtras" in the search field.

Documentation for any individual symbol from Spinors Extras package, can be
also called by selecting this symbols name in Mathematica and pressing the F1
key.

[Online version of documentation](http://www.fuw.edu.pl/~jkuczm/SpinorsExtras/reference/guide/SpinorsExtras.html)
is available on project website.



## Tests

Spinors Extras package is distributed with an extensive automated test suite
consisting of more than four thousand tests. To run the tests one needs
[MUnit](http://reference.wolfram.com/workbench/index.jsp?topic=/com.wolfram.eclipse.help/html/tasks/tester/tester.html)
package that is distributed with Wolfram Workbench.



## Bugs and requests

Bug reports and feature requests can be posted as
[GitHub issues](https://github.com/jkuczm/SpinorsExtras/issues)
(the preferred way) or sent directly to
[author’s email](mailto:Jakub.Kuczmarski@fuw.edu.pl).



## Contributing

Feel free to fork and send pull requests.

If you want to use Ant scripts from this repository you will also need to
install [WWBCommon](https://github.com/jkuczm/WWBCommon) project.

All contributions are welcome!



## License

This package is released under
[The MIT License](https://github.com/jkuczm/SpinorsExtras/blob/master/LICENSE).



## Versioning

Releases of this package will be numbered using
[Semantic Versioning guidelines](http://semver.org/).
