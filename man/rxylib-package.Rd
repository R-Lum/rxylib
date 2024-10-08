% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rxylib-package.R
\docType{package}
\name{rxylib-package}
\alias{rxylib-package}
\alias{rxylib}
\title{Import XY-Data into R

\if{html}{
\figure{rxylib.svg}{options: width="50"}\cr
}}
\description{
Provides access to the 'xylib' C++ library for to import xy
data from powder diffraction, spectroscopy and other experimental methods,
like gamma-ray spectrometry.\cr

License: GPL-3 | LGPL-2.1 (for the C++ library 'xylib')
}
\details{
\strong{Funding}

Between 2017-2019, the work of Sebastian Kreutzer as maintainer of the package was supported
by LabEx LaScArBx (ANR - n. ANR-10-LABX-52).

From 01/2020-04/2022, Sebastian Kreutzer received funding from the European Union’s Horizon 2020
research and innovation programme under the Marie Skłodowska-Curie grant
agreement No 844457 (project: CREDit).

Supported data formats:
\cr library version: 1.6.0\cr\cr
\tabular{lllllll}{\bold{ID} \tab \bold{NAME} \tab \bold{DESCRIPTION} \tab \bold{FILE EXTENSION}  \tab \bold{VALID_OPTIONS}  \tab \bold{DATATYPE} \tab \bold{BLOCK_TYPE}\cr
[1,] \tab cpi \tab Sietronics Sieray CPI \tab cpi \tab  \tab ascii \tab single\cr
[2,] \tab uxd \tab Bruker Diffrac-AT UXD \tab uxd \tab  \tab ascii \tab multiple\cr
[3,] \tab rigaku_dat \tab Rigaku DAT \tab dat \tab  \tab ascii \tab multiple\cr
[4,] \tab bruker_raw \tab Siemens/Bruker RAW \tab raw \tab  \tab binary \tab multiple\cr
[5,] \tab bruker_spc \tab Bruker ESP300-E SPC \tab spc \tab  \tab binary \tab single\cr
[6,] \tab vamas \tab VAMAS ISO-14976 \tab vms \tab  \tab ascii \tab multiple\cr
[7,] \tab philips_udf \tab Philips UDF \tab udf \tab  \tab ascii \tab single\cr
[8,] \tab spe \tab PI WinSpec SPE \tab spe \tab  \tab binary \tab multiple\cr
[9,] \tab pdcif \tab Powder Diffraction CIF \tab cif \tab  \tab ascii \tab multiple\cr
[10,] \tab philips_rd \tab Philips PC-APD RD/SD \tab rd sd \tab  \tab binary \tab single\cr
[11,] \tab xrdml \tab PANalytical XRDML \tab xrdml \tab  \tab ascii \tab multiple\cr
[12,] \tab canberra_mca \tab Canberra MCA \tab mca dat \tab  \tab binary \tab single\cr
[13,] \tab canberra_cnf \tab Canberra CNF \tab cnf \tab  \tab binary \tab single\cr
[14,] \tab xfit_xdd \tab XFIT XDD \tab xdd \tab  \tab ascii \tab single\cr
[15,] \tab riet7 \tab RIET7/LHPM/PSI_DMC \tab dat \tab  \tab ascii \tab single\cr
[16,] \tab dbws \tab DBWS data \tab dbw rit neu \tab  \tab ascii \tab single\cr
[17,] \tab chiplot \tab ChiPLOT data \tab chi \tab  \tab ascii \tab single\cr
[18,] \tab spectra \tab Spectra / VGX 900 \tab 1 2 3 4 5 6 7 8 9 \tab  \tab ascii \tab multiple\cr
[19,] \tab specsxy \tab SPECS SpecsLab2 xy \tab xy \tab  \tab ascii \tab multiple\cr
[20,] \tab csv \tab CSV or TSV \tab csv tsv tab \tab decimal-comma \tab ascii \tab single\cr
[21,] \tab xsyg \tab Freiberg Instruments XSYG \tab xsyg \tab  \tab ascii \tab multiple\cr
 \tab \tab }
}
\seealso{
Useful links:
\itemize{
  \item \url{https://r-lum.github.io/rxylib/}
  \item Report bugs at \url{https://github.com/R-Lum/rxylib/issues}
}

}
\author{
Sebastian Kreutzer, Institute of Geography, Ruprecht-Karl-University of Heidelberg (Germany), Johannes Friedrich (University of Bayreuth, Germany), RLum Team (family support), Marcin Wojdyr (maintainer and author of the C++ library \code{xylib}), Peng Zhang (author of the C++ library \code{xylib})
}
\keyword{package}
