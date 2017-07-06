// ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ## Title:   Return the supported formats from the xylib
// ## Author:  Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
// ## Contact: sebastian.kreutzer@u-bordeaux-montaigne.fr
// ## Date:    Wed Jun 28 10:13:07 2017
// ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include <Rcpp.h>
#include "xylib.h"

// [[Rcpp::export]]
RcppExport SEXP get_supportedFormats() {

  //access the list of supported formats
  const xylibFormat* format = NULL;
  int n_formats = 0;

  //obtain number of formats
  for (int i = 0; (format = xylib_get_format(i)) != NULL; ++i){
    n_formats = i;
  }

  //set needed vectors
  Rcpp::CharacterVector name(n_formats);
  Rcpp::CharacterVector desc(n_formats);
  Rcpp::CharacterVector exts(n_formats);
  Rcpp::CharacterVector binary(n_formats);
  Rcpp::CharacterVector multiblock(n_formats);
  Rcpp::CharacterVector valid_options(n_formats);

  //fill list
  for (int i = 0; i < n_formats; ++i){
    name(i) =  xylib_get_format(i) -> name;
    desc(i) =  xylib_get_format(i) -> desc;
    exts(i) =  xylib_get_format(i) -> exts;

    //binary
    if(xylib_get_format(i) -> binary == 0){
       binary(i) = "ascii";

    }else{
        binary(i) = "binary";

    }

    //multiblock
    if(xylib_get_format(i) -> multiblock == 0){
      multiblock(i) = "single";

    }else{
      multiblock(i) = "multiple";

    }

    //valid_options
    if(xylib_get_format(i) -> valid_options == NULL){
      valid_options(i) = "";

    }else{
      valid_options(i) = xylib_get_format(i) -> valid_options;

    }

  }

  //construc list and return results
  Rcpp::List results;
    results["name"] = name;
    results["desc"] = desc;
    results["exts"] = exts;
    results["binary"] = binary;
    results["multiblock"] = multiblock;
    results["valid_options"] = valid_options;

  return results;

}

