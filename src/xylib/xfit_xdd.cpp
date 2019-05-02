// FOURYA/XFIT/Koalariet XDD file
// Licence: Lesser GNU Public License 2.1 (LGPL)

#define BUILDING_XYLIB
#include "xfit_xdd.h"
#include "util.h"

using namespace std;
using namespace xylib::util;

namespace xylib {


const FormatInfo XfitXddDataSet::fmt_info(
    "xfit_xdd",
    "XFIT XDD",
    "xdd",
    false,                      // whether binary
    false,                      // whether has multi-blocks
    &XfitXddDataSet::ctor,
    &XfitXddDataSet::check
);

namespace {

void skip_c_style_comments(istream& f)
{
    skip_whitespace(f);
    int a = f.get();
    if (a != '/' || f.peek() != '*') {
        f.unget();
        return;
    }
    f.ignore(); // '*'
    while (f) {
        f.ignore(2048, '*');
        if (f.peek() == '/') {
            f.ignore();
            break;
        }
    }
    skip_whitespace(f);
}

} // anonymous namespace

bool XfitXddDataSet::check(istream &f, string*)
{
    skip_c_style_comments(f);
    Column *c = read_start_step_end_line(f);
    bool ok = (c != NULL);
    delete c;
    return ok;
}

void XfitXddDataSet::load_data(std::istream &f, const char*)
{
    skip_c_style_comments(f);
    Block *blk = read_ssel_and_data(f);
    format_assert(this, blk != NULL);
    add_block(blk);
}

} // namespace xylib

