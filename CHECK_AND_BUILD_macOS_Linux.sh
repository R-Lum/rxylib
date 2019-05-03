#!/bin/bash
R -q -e "RLumBuild::build_package(
  exclude = c(
  'module_check_ReverseDependencies',
  'module_add_RLumTeam'
  ),
  as_cran = TRUE,
  write_Rbuildignore = TRUE
)"
