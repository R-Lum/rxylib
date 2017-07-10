// ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ## Title:   Read data using the xylib
// ## Author:  Johannes Friedrich, University of Bayreuth (Germany)
// ## Contact: Johannes.Friedrich@uni-bayreuth.de
// ## Date:    Mon Jul 10 12:01:37 2017
// ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include <Rcpp.h>
#include "xylib.h"

using namespace Rcpp;
using namespace xylib;

// [[Rcpp::export]]
Rcpp::DataFrame get_meta_DataSet(std::string path, std::string format_name, std::string options){

  //load dataset
  DataSet* dataset = load_file(path, format_name, options);
  
  size_t meta_size = dataset ->meta.size();
  std::string value, key;
  Rcpp::CharacterVector value_vec, key_vec;
  
  for(int i =0; i < meta_size; i++){
    
    key = dataset ->meta.get_key(i); 
    value = dataset ->meta.get(key);
    key_vec.push_back(key);
    value_vec.push_back(value);

  }

  return(Rcpp::DataFrame::create(
      _["key"] = key_vec,
      _["value"] = value_vec,
      _["stringsAsFactors"] = false
  ));
}
