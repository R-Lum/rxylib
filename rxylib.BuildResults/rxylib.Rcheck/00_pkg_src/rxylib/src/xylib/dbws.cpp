// DBWS data file
// Licence: Lesser GNU Public License 2.1 (LGPL)

#define BUILDING_XYLIB
#include "dbws.h"

#include <cmath>
#include <cstdlib>

#include "util.h"

using namespace std;
using namespace xylib::util;

namespace xylib {


const FormatInfo DbwsDataSet::fmt_info(
    "dbws",
    "DBWS data",
    "dbw rit neu",
    false,                      // whether binary
    false,                      // whether has multi-blocks
    &DbwsDataSet::ctor,
    &DbwsDataSet::check
);

bool DbwsDataSet::check(istream &f, string*)
{
    string line;
    getline(f, line);
    if (line.size() < 3*8)
        return false;
    // the first line should be in format (3F8.2, A48), but sometimes
    // the number of digits after the decimal point is 3 or 4
    string start_s(line, 0, 8);
    string step_s(line, 8, 8);
    string stop_s(line, 16, 8);
    char *endptr;
    double start = strtod(start_s.c_str(), &endptr);
    if (*endptr != 0)
        return false;
    double step = strtod(step_s.c_str(), &endptr);
    if (*endptr != 0)
        return false;
    double stop = strtod(stop_s.c_str(), &endptr);
    if (*endptr != 0)
        return false;
    if (step < 0 || start + step > stop)
        return false;
    double count = (stop - start) / step + 1;
    double rounded_count = floor(count + 0.5);
    if (fabs(rounded_count - count) > 1e-6)
        return false;
    return true;
}

void DbwsDataSet::load_data(std::istream &f, const char*)
{
    Block* blk = new Block;

    string s;
    getline(f, s); // first line
    format_assert(this, s.size() >= 3*8);
    blk->set_name(str_trim(s.substr(24)));
    double start = my_strtod(s.substr(0, 8));
    double step = my_strtod(s.substr(8, 8));
    StepColumn *xcol = new StepColumn(start, step);
    blk->add_column(xcol);

    // data
    VecColumn *ycol = new VecColumn;
    while (getline(f, s))
        // numbers delimited by commas or spaces.
        ycol->add_values_from_str(s, ',');
    blk->add_column(ycol);

    add_block(blk);
}

} // namespace xylib

