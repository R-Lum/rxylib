// ISO14976 VAMAS Surface Chemical Analysis Standard Data Transfer Format File
// Licence: Lesser GNU Public License 2.1 (LGPL)

// This implementation is based on [1] and on the analysis of sample files.
//
//[1] W.A. Dench, L. B. Hazell and M. P. Seah, VAMAS Surface Chemical Analysis
//    Standard Data Transfer Format with Skeleton Decoding Programs,
//    Surface and Interface Analysis, 13 (1988) 63-122
//    or National Physics Laboratory Report DMA(A)164 July 1988
//

#ifndef XYLIB_VAMAS_H_
#define XYLIB_VAMAS_H_
#include "xylib.h"

namespace xylib {

    class VamasDataSet : public DataSet
    {
        OBLIGATORY_DATASET_MEMBERS(VamasDataSet)

    private:
        int blk_fue_;           // number of future upgrade experiment entries
        std::string exp_mode_;  // experimental mode
        std::string scan_mode_; // scan mode
        int exp_var_cnt_;       // count of experimental variables

        Block *read_block(std::istream &f, bool includes[],
                          const Block* first_block);
    };

} // namespace xylib
#endif // XYLIB_VAMAS_H_

