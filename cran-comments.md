## Release summary

This release addresses the CRAN R-devel notes and updates to a newer xy-library version. 

## Known issues

The compilation of the file `xylib/util.cpp` throws out a warning, however, 
this is related to boost library (package 'BH') and there is nothing 
we can do about. 

## Addressed CRAN issues

> Result: NOTE 
>     PKG_CFLAGS set in 'src/Makevars' without any corresponding files 

Indeed, we have only *.cpp files. The corresponding line in 'src/Makevars' was removed. 

## Test environments
* local macOS High Sierra 10.14.4-xcode10.2, R-devel
* on AppVeyor CI
    * i386-w64-mingw32/i386 (32-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R version 3.4.3 (2017-11-30)
    * i386-w64-mingw32/i386 (32-bit), R version 3.4.3 (2017-11-30)
* on Travis CI
  * Ubuntu 14.04.5 LTS, R-devel
  * MacOSX, 10.12-xcode8.3, R 3.4.4

## R CMD check results
There were no ERRORs or WARNINGs.

## License questions

The `xylib` is published under LGPL-2.1. This is mentioned in the copyright statement.
