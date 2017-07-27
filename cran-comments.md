## Release summary

This is a major release, inlcuding various new features 
and it hands in some missing featues with regard 
to the underlying C++ library `xylib`

## Test environments
* local macOS Sierra 10.12.6, R-devel
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

There was 1 NOTE:

Some systems complain about the installed package size due to 
a large libs directory after the compilation. From my understanding 
I can do nothing about it.  

## License questions

The `xylib` is published under LGPL-2.1. This is mentioned in the copyright statement.
