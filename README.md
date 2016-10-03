<!-- 
[![MELPA Stable](http://stable.melpa.org/packages/buttercup-junit-badge.svg)](http://stable.melpa.org/#/buttercup-junit)
[![MELPA](http://melpa.org/packages/buttercup-junit-badge.svg)](http://melpa.org/#/buttercup-junit)  
-->

# buttercup-junit - JUnit XML reports from buttercup results

buttercup-junit publishes buttercup test results as [JUnit][JUNIT] XML files.

[Buttercup][BUTTERCUP] recommends running tests in batch mode
from the command line.  The results are printed on stdout.  buttercup-junit
does the same, but writes the test results to a [JUnit][JUNIT]
compatible [XML-file][JUNITXSD].

buttercup-junit was originally conceived to facilitate [CI][CI] testing on
http://shippable.com.

[BUTTERCUP]: https://github.com/jorgenschaefer/emacs-buttercup
[JUNIT]: http://junit.org "JUnit Home"
[JUNITXSD]: http://windyroad.com.au/dl/Open%20Source/JUnit.xsd "JUnit xsd"
[CI]: http://en.wikipedia.org/wiki/Continuous_integration "Continous Integration on Wikipedia"

## git repo

buttercup-junit is hosted on [bitbucket.org][BITBUCKET]:
https://bitbucket.org/olanilsson/buttercup-junit

Pull requests welcome.

[BITBUCKET]: http://bitbucket.org "BitBucket"

## Issue tracker

https://bitbucket.org/olanilsson/buttercup-junit/issues

# Documentation

## buttercup-junit-run-discover ()

A wrapper around `buttercup-run-discover` that sets
`buttercup-reporter` to also output results to `results.xml`.  

buttercup-junit-run-discover will consume the following flags from
`command-line-args-left` and pass the rest to
`buttercup-run-discover`.

 * `--xmlfile XMLFILE` - Write the report to XMLFILE.
 * `--junit-stdout` - Write the report both to file and stdout.


