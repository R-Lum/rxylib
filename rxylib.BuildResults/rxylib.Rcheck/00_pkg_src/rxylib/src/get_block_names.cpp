// ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ## Title:   Read block names
// ## Author:  Johannes Friedrich, University of Bayreuth (Germany)
// ## Contact: Johannes.Friedrich@uni-bayreuth.de
// ## Date:    Tue Jul 18 10:06:21 2017
// ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#include <Rcpp.h>
#include "xylib.h"

using namespace Rcpp;
using namespace xylib;

// [[Rcpp::export]]
Rcpp::CharacterVector get_block_names(std::string path, std::string format_name, std::string options){
  
  //load dataset
  DataSet* dataset = load_file(path, format_name, options);
  
  //initialise values
  int n_blocks = dataset -> get_block_count();
  List results(n_blocks);
  Rcpp::CharacterVector block_names(n_blocks);

  //loop over blocks
  for(int b=0;b<n_blocks;b++){
    
    const Block* block = dataset ->get_block(b);
    block_names(b) = block->get_name().c_str();
      
  }
  
  return(block_names);
  
}
