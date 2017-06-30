## Release summary

New package submission. 


## Addressed CRAN requests

Request by Uwe Ligges: 
*Thanks, can you please unwrap the example from \sontrun{} so that the example also gets checked?*

Done. 

## Test environments
* local macOS Sierra 10.12.5, R version 3.4.1 RC (2017-06-22 r72859); currently no R-devel available
* on AppVeyor CI
    * i386-w64-mingw32/i386 (32-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R-devel
    * x86_64_w64-mingw32/64 (64-bit), R 3.4.0 (2017-04-21)
    * i386-w64-mingw32/i386 (32-bit), R 3.4.0 (2017-04-21)
* on Travis CI
  * Ubuntu 12.04.5 LTS, R-devel
  * MacOSX, 10.11-xcode7.3, R 3.4.0
* R-Hub (all provided test environments)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTES:

* Maintainer: ‘Sebastian Kreutzer <sebastian.kreutzer@u-bordeaux-montaigne.fr>’
  New submission

This is a new submission.

On, e.g., R-Hub CentOS 6, stock R from EPEL

* checking installed package size ... NOTE
  installed size is  8.2Mb
  sub-directories of 1Mb or more:
    libs   7.9Mb
  
From my understanding, the installed package size is large because of the large libs/ directory, 
due to the large size of 'xylib' after the compilation. This occurs only on linux platforms, 
but I don't think I can do something about it. 


## License questions

The `xylib` is published under LGPL-2.1. This is mentioned in the copyright statement and 
I did contact the maintainer before submission. 
