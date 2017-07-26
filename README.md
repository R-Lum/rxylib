# rxylib

The **R** package `rxylib` provides and access to the C++ library [xylib](http://xylib.sourceforge.net)
by wrapping the library using Rcpp. For supported data formats see https://github.com/wojdyr/xylib.

## Documentation
[![Rdoc](http://www.rdocumentation.org/badges/version/rxylib)](http://www.rdocumentation.org/packages/rxylib)

## Package metrics 
### CRAN statistics
[![CRAN](http://www.r-pkg.org/badges/version/rxylib)](http://cran.rstudio.com/package=rxylib)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/rxylib)](http://www.r-pkg.org/pkg/rxylib)
[![Downloads](http://cranlogs.r-pkg.org/badges/rxylib)](http://www.r-pkg.org/pkg/rxylib)
[![Downloads](http://cranlogs.r-pkg.org/badges/last-week/rxylib)](http://www.r-pkg.org/pkg/rxylib)
[![Downloads](http://cranlogs.r-pkg.org/badges/last-day/rxylib)](http://www.r-pkg.org/pkg/rxylib)

### Impact metrics
[![Research software impact](http://depsy.org/api/package/cran/rxylib/badge.svg)](http://depsy.org/package/r/rxylib)
[![Github Stars](https://img.shields.io/github/stars/R-Lum/rxylib.svg?style=social&label=Github)](https://github.com/R-Lum/rxylib)

### Platform tests status 
[![Build status](https://ci.appveyor.com/api/projects/status/7mmfpmo23k3iaq57/branch/master?svg=true)](https://ci.appveyor.com/project/RLumSK/rxylib/branch/master)
[![Build Status](https://travis-ci.org/R-Lum/rxylib.svg?branch=master)](https://travis-ci.org/R-Lum/rxylib)

### Unit test status
[![Coverage Status](https://img.shields.io/codecov/c/github/R-Lum/rxylib.svg)](https://codecov.io/github/R-Lum/rxylib?branch=master)

## Installation

#### i. Requirements

**Windows (32/64bit)** - 'Rtools' (provided by CRAN)

   http://cran.r-project.org/bin/windows/Rtools/

**MacOSX and macOS** - 'Xcode' (provided by Apple)

   https://developer.apple.com/xcode/downloads/

For **Linux** users *gcc* often comes pre-installed in most distributions. Should *gcc* be not available, however, we kindly refer to the exhaustive collection of installation guides depending on the linux distribution.

#### ii. Install the package

To install the stable version from CRAN, simply run the following from an R console:

```r
install.packages("rxylib")
```

To install the latest development builds directly from GitHub, run

```r
if(!require("devtools"))
  install.packages("devtools")
devtools::install_github("R-Lum/rxylib@master")
```
To install a developer build other than 'master', replace the term 'master' in the codeline by the name
of the wanted developer build. 

# Releated projects

* [rxylibShiny](https://github.com/JohannesFriedrich/rxylibShiny)

# Licenses

Please note that the package comprises two different licences 

## Package `rxylib` license 

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
[GNU General Public License](https://github.com/R-Lum/rxylib/blob/master/LICENSE) for more details.

## Library `xylib` library license 

The library itself is **NOT** part of the GPL-3 license conditions of the package 
and available under [LGPL](https://github.com/wojdyr/xylib/blob/master/COPYING) 
license conditions only cf. https://github.com/wojdyr/xylib

