## PACKAGE MAKEFILE
RSCRIPT = "$(R_HOME)/bin/Rscript"

all:
	${RSCRIPT} -e "RLumBuild::build_package(exclude = c('module_check_ReverseDependencies', 'module_write_codemetar',  'module_add_RLumTeam'), as_cran = TRUE, write_Rbuildignore = TRUE)";
