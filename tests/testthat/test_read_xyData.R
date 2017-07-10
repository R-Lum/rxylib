context("read_xyData")

test_that("Test various examples", {
  testthat::skip_on_cran()

  ##force break (file does not exists)
  expect_error(read_xyData(file = "hi"))

  ##check get_version (internal function)
  expect_type(rxylib:::get_version(), type = "character")

  ##force connection error
  expect_error(read_xyData(file = "https://github.com/wojdyr/xylib/blob/master/samples/03yag02"))

  ##force wrong file format read
  expect_error(read_xyData(file = "https://raw.githubusercontent.com/R-Lum/rxylib/master/R/read_xyData.R"))

  ##check S3 methods
  test_dataset <- read_xyData(file = "https://github.com/wojdyr/xylib/blob/master/samples/03yag02.mca")
  expect_output(print(test_dataset))
  expect_silent(plot(test_dataset))
  rm(test_dataset)

  ##check C++ function
  expect_type(
    rxylib:::get_meta_DataSet(path = system.file("extdata/ExampleSpectrum.CNF", package = "rxylib"),
                              format_name = "canberra_cnf",options = ""),
    type = "list")

  ##check metadata argument
  expect_type(read_xyData(file = system.file("extdata/ExampleSpectrum.CNF", package = "rxylib"), metaData = FALSE), type = "list")

  ##check verbose
  expect_silent(read_xyData(file = system.file("extdata/ExampleSpectrum.CNF", package = "rxylib"), verbose =  FALSE))

  ##load example data step by step from GitHub
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/blob/master/samples/03yag02.mca"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/blob/master/samples/04nacl02.mca"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/1517474.cif"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/1d-1.spe"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/1d-2.spe"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/1d-3.spe"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/BT86.raw"), type = "list")
  expect_type(read_xyData(file = "https://github.com/tzerk/ESR/raw/master/inst/extdata/mollusc.SPC"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/BT86_.UXD"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/Cu3Au-1.raw"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/Cu3Au-2.raw"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/D1A5.dat"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/PSI_DMC.dat"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/SMP00011.CNF"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/Spectra.1"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/background_8.CNF"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/empyrean.xrdml"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/format1.raw"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/mjr9_116a.vms"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/mjr9_59c.vms"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/mjr9_64c.vms"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/mm-specs.xy"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/pesa.txt"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/small.tsv"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/specsxy_example.xy"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/test1.csv"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/test2.csv"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/test3.csv"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/test4.csv"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/with_commas.txt"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/with_sigma.txt"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/xy_text.txt"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/rfqm_uv.xsyg"), type = "list")

})

