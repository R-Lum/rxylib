//read_data.cpp
//author: Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
//version: 0.1.0 [2017-06-27]

#include <Rcpp.h>
#include "xylib.h"

using namespace Rcpp;
using namespace xylib;

// [[Rcpp::export]]
List read_data(std::string path, std::string format_name){

  //load dataset
  DataSet* dataset = load_file(path, format_name);

  //initialise values
  int n_blocks = dataset -> get_block_count();
  List results(n_blocks);

  //loop over blocks
  for(int b=0;b<n_blocks;b++){

    //set variables for the block; columns and rows should be equal for each block
    int n_columns = xylib_count_columns(xylib_get_block(dataset, b));
    int n_rows = xylib_count_rows(xylib_get_block(dataset, b), 1);
    NumericMatrix m(n_rows, n_columns);

    //loop over columns
    for (int c=0;c<n_columns; c++){

       //loop over rows
       for (int r=0;r<n_rows; r++){
         m(r,c) = xylib_get_data(xylib_get_block(dataset, b), c + 1, r);

       }

    }

    //write values into list
    results(b) = m;

 }

  return results;
}
