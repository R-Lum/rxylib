## Release summary

This is a minor release, polishing the package
and adding new functionality.

## Test environments
* local macOS High Sierra 10.13.2, R-devel
* on AppVeyor CI
    * i386-w64-mingw32/i386 (32-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R 3.4.0 (2017-04-21)
    * i386-w64-mingw32/i386 (32-bit), R 3.4.0 (2017-04-21)
* on Travis CI
  * Ubuntu 12.04.5 LTS, R-devel
  * MacOSX, 10.11-xcode7.3, R 3.4.0

## R CMD check results
There were no ERRORs or WARNINGs.

## CRAN notes

Some systems complain about the installed package size due to 
a large libs directory after the compilation. We tried 
to overcome this by following the suggestions made
by Dirk Eddelbuettel (http://dirk.eddelbuettel.com/blog/2017/08/14/#009_compact_shared_libraries). 
Local tests indicate a signficant reduction of the library 
size, whether this will stand on the CRAN servers we don't know. 

## License questions

The `xylib` is published under LGPL-2.1. This is mentioned in the copyright statement.
