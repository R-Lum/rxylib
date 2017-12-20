## Release summary

This is a minor release, polishing the package
and adding new functionality.

## Test environments
* local macOS High Sierra 10.13.2-xcode9.2, R-devel
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

## CRAN notes

Some check systems on CRAN complain about the installed package size due to 
a large libs directory after the compilation. We tried 
to overcome this by following the suggestions made
by Dirk Eddelbuettel (http://dirk.eddelbuettel.com/blog/2017/08/14/#009_compact_shared_libraries). 
Local tests indicate a signficant reduction of the library 
size, whether this will stand on the CRAN servers we don't know. 

## License questions

The `xylib` is published under LGPL-2.1. This is mentioned in the copyright statement.
