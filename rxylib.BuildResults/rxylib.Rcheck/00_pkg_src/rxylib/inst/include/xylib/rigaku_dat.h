// Rigaku .dat format - powder diffraction data from Rigaku diffractometers
// Licence: Lesser GNU Public License 2.1 (LGPL)

// Implementation based on the analysis of the sample files.

#ifndef XYLIB_RIGAKU_DAT_H_
#define XYLIB_RIGAKU_DAT_H_
#include "xylib.h"


namespace xylib {

    class RigakuDataSet : public DataSet
    {
        OBLIGATORY_DATASET_MEMBERS(RigakuDataSet)
    };

} // namespace

#endif // XYLIB_RIGAKU_DAT_H_

