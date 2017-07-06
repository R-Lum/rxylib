## Release summary

This is a bugfix release, in paricular addressing a CRAN compiler error 
on Solaris. Thanks to Prof Ripley for pointing out.

## Addressed CRAN requests

Request by Brain Ripley 

* Fix error on the Solaris platform: https://www.r-project.org/nosvn/R.check/r-patched-solaris-x86/rxylib-00install.html

Thanks to the maintainer of the wrapped 'xylib', it got fixed directly within in the C++ library. However, I have no access to a Solaris system (offered VMs on the web and the given information there never worked out due to problems with 'libcurl'). Means: If there is another problem on Solaris, of course I will do everything I can to fix it, but I cannot test it.

## Test environments
* local macOS Sierra 10.12.5, R-devel
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
