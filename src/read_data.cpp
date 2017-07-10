// ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ## Title:   Read data using the xylib
// ## Author:  Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France),
// ##          Johannes Friedrich, University of Bayreuth (Germany)
// ## Contact: sebastian.kreutzer@u-bordeaux-montaigne.fr
// ## Date:    Wed Jun 28 10:13:07 2017
// ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include <Rcpp.h>
#include "xylib.h"

using namespace Rcpp;
using namespace xylib;

// [[Rcpp::export]]
RcppExport SEXP read_data(std::string path, std::string format_name, std::string options, bool metaData){

  //load dataset
  DataSet* dataset = load_file(path, format_name, options);

  //initialise values
  int n_blocks = dataset -> get_block_count();
  List results(n_blocks);
  List results_block;

  //loop over blocks
  for(int b=0;b<n_blocks;b++){

    //set variables for the block; columns and rows should be equal for each block
    int n_columns = xylib_count_columns(xylib_get_block(dataset, b));
    int n_rows = xylib_count_rows(xylib_get_block(dataset, b), n_columns);

    //set numeric matrix
    NumericMatrix m(n_rows, n_columns);

    //loop over columns
    for (int c=0;c<n_columns; c++){

       //loop over rows
       for (int r=0;r<n_rows; r++){
         m(r,c) = xylib_get_data(xylib_get_block(dataset, b), c + 1, r);

       }

    }

    //get Block meta data
    Rcpp::DataFrame metaData_block;

    if(metaData){
      const Block* block = dataset ->get_block(b);

      size_t meta_size = block ->meta.size();
      std::string value, key;
      Rcpp::CharacterVector value_vec, key_vec;

      for(int i =0; i < meta_size; i++){

        key = block ->meta.get_key(i);
        value = block ->meta.get(key);
        key_vec.push_back(key);
        value_vec.push_back(value);

      }

      metaData_block = Rcpp::DataFrame::create(
        Rcpp::Named("key") = key_vec,
        Rcpp::Named("value") = value_vec);

      //write values into list
      results_block = Rcpp::List::create(
        Rcpp::Named("data_block") = m,
        Rcpp::Named("metadata_block") = metaData_block);

    } else {

      //write values into list
      results_block = Rcpp::List::create(
        Rcpp::Named("data_block") = m);
    }

      results(b) = results_block;

 }

  // destruct DataSet created by xylib_load_file() //
  xylib_free_dataset(dataset);

  return results;
}
