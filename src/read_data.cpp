// ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ## Title:   Read data using the xylib
// ## Author:  Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France),
// ##          Johannes Friedrich, University of Bayreuth (Germany)
// ## Contact: sebastian.kreutzer@u-bordeaux-montaigne.fr
// ## Version: 0.2.0 - 2017-07-14
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
  // std::vector<std::string> col_names;


  //loop over blocks
  for(int b=0;b<n_blocks;b++){

    const Block* block = dataset ->get_block(b);

    //set variables for the block; columns and rows should be equal for each block
    int n_columns = xylib_count_columns(xylib_get_block(dataset, b));
    int n_rows = xylib_count_rows(xylib_get_block(dataset, b), n_columns);
    CharacterVector col_names(n_columns);

    //set numeric matrix
    NumericMatrix m(n_rows, n_columns);

    //loop over columns
    for (int c=0;c<n_columns; c++){
       //loop over rows
       for (int r=0;r<n_rows; r++){
         m(r,c) = xylib_get_data(xylib_get_block(dataset, b), c + 1, r);

       }

       // get column names (if available)
       if(!block ->get_column(c+1).get_name().empty()){
        col_names[c] = block ->get_column(c+1).get_name();

       } else {
        //convert to number and create new column name
        std::ostringstream convert;
        convert << c+1;
        col_names[c] = "V" + convert.str(); //adding the 'V' is somehow consistent with R behaviour

       }
    }

    // set column names
    colnames(m) = col_names;

    //get Block meta data
    if(metaData){
      Rcpp::DataFrame metaData_block;

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
        _["key"] = key_vec,
        _["value"] = value_vec,
        _["stringsAsFactors"] = false );

      //write values into list
      results_block = Rcpp::List::create(
        _["data_block"] = m,
        _["metadata_block"] = metaData_block);

    } else {
     //write values into list
      results_block = Rcpp::List::create(
        _["data_block"] = m);
    }

      results(b) = results_block;

 }

  // destruct DataSet created by xylib_load_file() //
  xylib_free_dataset(dataset);

  return results;
}
