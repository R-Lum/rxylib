// DBWS data file
// Licence: Lesser GNU Public License 2.1 (LGPL)

// From DBWS-9807a User's Guide page 37-38:
// The observed data file > Category 1 >
// Case (0): The 'standard' DBWS observed data file (Tape/Unit 4) . IDATA= 0
// Case (1): Free format. IDATA= 1
//
// The first line contains the variables START, STEP, STOP,
// and DATAID in (3F8.2, A48) format.
// The rest of the file consists of the intensity data themselves in:
//  - case (0): (8(F7.0,1X)) format
//  - case (1): in free format. Any number of columns of any width are allowed.
//              The delimiters are commas or spaces.
//
// It's also DMPLOT raw data format. The raw data file names for DMPLOT must
// have the extensions *.rit for x-ray data *.neu for neutron data.


#ifndef XYLIB_DBWS_H_
#define XYLIB_DBWS_H_
#include "xylib.h"

namespace xylib {

    class DbwsDataSet : public DataSet
    {
        OBLIGATORY_DATASET_MEMBERS(DbwsDataSet)
    };

}
#endif // XYLIB_DBWS_H_

