## PACKAGE MAKEFILE
RSCRIPT = "$(R_HOME)/bin/Rscript"

all:
	${RSCRIPT} -e "RLumBuild::build_package(exclude = c('module_check_ReverseDependencies',  'module_add_RLumTeam'), as_cran = FALSE, write_Rbuildignore = TRUE)";
