# buttercup-junit - JUnit XML reports from buttercup results

[![CircleCI](https://circleci.com/bb/olanilsson/buttercup-junit/tree/master.svg?style=shield&circle-token=0114ae411116984dae168da481f600ef300e78e4)](https://circleci.com/bb/olanilsson/buttercup-junit/tree/master)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/buttercup-junit-badge.svg)](https://melpa.org/#/buttercup-junit)
[![MELPA Stable](https://stable.melpa.org/packages/buttercup-junit-badge.svg)](https://stable.melpa.org/#/buttercup-junit)

![emacs 26.3](https://img.shields.io/badge/emacs-26.3-brightgreen.svg)
![emacs 26.2](https://img.shields.io/badge/emacs-26.2-brightgreen.svg)
![emacs 26.1](https://img.shields.io/badge/emacs-26.1-brightgreen.svg)
![emacs 25.3](https://img.shields.io/badge/emacs-25.3-brightgreen.svg)
![emacs 25.2](https://img.shields.io/badge/emacs-25.2-brightgreen.svg)
![emacs 25.1](https://img.shields.io/badge/emacs-25.1-brightgreen.svg)
![emacs 24.4](https://img.shields.io/badge/emacs-24.4-brightgreen.svg)
![emacs 24.3](https://img.shields.io/badge/emacs-24.3-brightgreen.svg)

buttercup-junit publishes [buttercup][BUTTERCUP] test results as
[JUnit][JUNIT] XML files.

[Buttercup][BUTTERCUP] recommends running tests in batch mode
from the command line.  The results are printed on stdout.  buttercup-junit
does the same, but also writes the test results to a [JUnit][JUNIT]
compatible [XML-file][JUNITXSD].

buttercup-junit was originally conceived to facilitate [CI][CI]
testing on [Shippable][SHIPPABLE] and [CircleCI][CIRCLECI].

[SHIPPABLE]: https://shippable.com "Shippable"
[CIRCLECI]: https://circleci.com "CircleCI"
[BUTTERCUP]: https://github.com/jorgenschaefer/emacs-buttercup "Emacs Buttercup"
[JUNIT]: https://junit.org "JUnit Home"
[JUNITXSD]: https://windyroad.com.au/dl/Open%20Source/JUnit.xsd "JUnit xsd"
[CI]: https://en.wikipedia.org/wiki/Continuous_integration "Continous Integration on Wikipedia"

## git repo

buttercup-junit is hosted on [bitbucket.org][BITBUCKET]:
https://bitbucket.org/olanilsson/buttercup-junit

Pull requests welcome.

[BITBUCKET]: https://bitbucket.org "BitBucket"

## Issue tracker

[https://bitbucket.org/olanilsson/buttercup-junit/issues][ISSUES]

[ISSUES]: https://bitbucket.org/olanilsson/buttercup-junit/issues "Buttercup JUnit Issues"

# Installation

Use `package.el` to install from [MELPA][MELPA] or [MELPA
Stable][MELPASTABLE].

[MELPA]: https://melpa.org "MELPA"
[MELPASTABLE]: https://stable.melpa.org "MELPA Stable"

# Documentation

## buttercup-junit-run-discover ()

A wrapper around `buttercup-run-discover` that sets
`buttercup-reporter` to also output results to `results.xml`.

buttercup-junit-run-discover will consume the following flags from
`command-line-args-left` and pass the rest to
`buttercup-run-discover`.

 * `--xmlfile XMLFILE` - Write the report to XMLFILE.
 * `--junit-stdout` - Write the report both to file and stdout.
 * `--outer-suite SUITE` - Add an outer testsuite SUITE to the report.

# More information about the JUnit format

These are the best resources on the [JUnit XML format][JUNITXSD] that I've found,
but they are still not definitive as each service chooses how to
interpret and display the data.  If you find that the test results are
not interpreted as expected on some service, please raise an
[issue][ISSUES].

 * https://windyroad.com.au/dl/Open%20Source/JUnit.xsd
 * http://help.catchsoftware.com/display/ET/JUnit+Format
 * http://llg.cubic.org/docs/junit/
