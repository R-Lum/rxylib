// Canberra AccuSpec MCA
// Licence: Lesser GNU Public License 2.1 (LGPL)

#define BUILDING_XYLIB
#include "canberra_mca.h"

#include <cmath>
#include <boost/cstdint.hpp>

#include "util.h"

using namespace std;
using namespace xylib::util;
using boost::uint16_t;
using boost::uint32_t;


namespace xylib {


const FormatInfo CanberraMcaDataSet::fmt_info(
    "canberra_mca",
    "Canberra MCA",
    "mca dat",
    true,                       // whether binary
    false,                      // whether has multi-blocks
    &CanberraMcaDataSet::ctor,
    &CanberraMcaDataSet::check
);

bool CanberraMcaDataSet::check(istream &f, string*)
{
    const int file_size = 2*512+2048*4;
    char *all_data = new char[file_size];
    f.read(all_data, file_size);
    uint16_t word_at_0 = from_le<uint16_t>(all_data + 0);
    uint16_t word_at_34 = from_le<uint16_t>(all_data + 34);
    uint16_t word_at_36 = from_le<uint16_t>(all_data + 36);
    uint16_t word_at_38 = from_le<uint16_t>(all_data + 38);
    delete [] all_data;
    return f.gcount() == file_size
           && word_at_0 == 0
           && word_at_34 == 4
           && word_at_36 == 2048
           && word_at_38 == 1;
}

void CanberraMcaDataSet::load_data(std::istream &f, const char*)
{
    const int file_size = 2*512+2048*4;
    char *all_data = new char[file_size];
    f.read(all_data, file_size);
    if (f.gcount() != file_size) {
        delete [] all_data;
        throw FormatError("Unexpected end of file.");
    }

    double energy_offset = from_pdp11((unsigned char*) all_data + 108);
    double energy_slope = from_pdp11((unsigned char*) all_data + 112);
    double energy_quadr = from_pdp11((unsigned char*) all_data + 116);

    Block* blk = new Block;

    Column *xcol = NULL;
    if (energy_quadr) {
        VecColumn *vc = new VecColumn;
        for (int i = 1; i <= 2048; i++) {
            //FIXME should it be from 1 ?
            // perhaps from 0 to 2047, description was not clear.
            double x = energy_offset + energy_slope * i + energy_quadr * i * i;
            vc->add_val(x);
        }
        xcol = vc;
    }
    else {
        xcol = new StepColumn(energy_offset+energy_slope, energy_slope);
    }
    blk->add_column(xcol);

    VecColumn *ycol = new VecColumn;
    uint16_t data_offset = from_le<uint16_t>(all_data+24);
    for (int i = 0; i < 2048; i++) {
        uint32_t y = from_le<uint32_t>(all_data + data_offset + 4*i);
        ycol->add_val(y);
    }
    delete [] all_data;
    blk->add_column(ycol);

    add_block(blk);
}


} // namespace xylib

