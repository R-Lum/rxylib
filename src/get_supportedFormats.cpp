// ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ## Title:   Return the supported formats from the xylib
// ## Author:  Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
// ## Contact: sebastian.kreutzer@u-bordeaux-montaigne.fr
// ## Date:    Wed Jun 28 10:13:07 2017
// ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include <Rcpp.h>
#include "xylib.h"

using namespace Rcpp;
using namespace xylib;

//TODO LIST
// - valid_options crashes RStudio
// - add other format information

// [[Rcpp::export]]
List get_supportedFormats() {

  //access the list of supported formats
  const xylibFormat* format = NULL;
  int n_formats;

  //obtain number of formats
  for (int i = 0; (format = xylib_get_format(i)) != NULL; ++i){
    n_formats = i;
  }

  //set needed vectors
  CharacterVector name(n_formats);
  CharacterVector desc(n_formats);
  CharacterVector exts(n_formats);
  //CharacterVector valid_options(n_formats); TODO

  //fill list
  for (int i = 0; i < n_formats; ++i){
    name(i) =  xylib_get_format(i) -> name;
    desc(i) =  xylib_get_format(i) -> desc;
    exts(i) =  xylib_get_format(i) -> exts;
    //valid_options(i) =  xylib_get_format(i) -> valid_options; TODO
  }

  List results;
    results["name"] = name;
    results["desc"] = desc;
    results["exts"] = exts;
    //results["valid_options"] = valid_options; TODO

  return results;

}

