#include <Rcpp.h>
#include "xylib.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix read_data(std::string path, std::string format_name){

  //define dataset
  xylib::DataSet* dataset;

  dataset = xylib::load_file(path, format_name);

  Rcout << xylib_count_columns(xylib_get_block(dataset, 0));



  return 2;


}
