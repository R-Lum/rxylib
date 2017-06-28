### ===============================================================================================
### R package rxylib BUILDSCRIPTS
### EntryPointRegistering
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2017-06-28
### ===============================================================================================

##this script bases on
##http://stackoverflow.com/questions/42313373/r-cmd-check-note-found-no-calls-to-r-registerroutines-r-usedynamicsymbols

# ##run registration
rxylib_init <- utils::capture.output(tools::package_native_routine_registration_skeleton("."))

##add header text
header <-  c(
"/* DO NOT CHANGE MANUALLY! */",
"/* This file was produced by the function rxylib.BuildScripts/rxylib.PBS_EntryPointRegistration.R */")

##write file
write(x = c(header, rxylib_init), file = "src/registerDynamicSymbol.c")
