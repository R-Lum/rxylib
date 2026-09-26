// ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ## Title:   Return version number of the 'xylib'
// ## Author:  Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
// ## Contact: sebastian.kreutzer@u-bordeaux-montaigne.fr
// ## Date:    Wed Jun 28 10:13:07 2017
// ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include <Rcpp.h>
#include "xylib.h"

// [[Rcpp::export]]
Rcpp::CharacterVector get_version() {

  return  xylib_get_version();

}

