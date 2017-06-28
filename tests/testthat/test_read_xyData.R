context("read_xyData")

test_that("Test various examples", {
  testthat::skip_on_cran()

  ##force break
  expect_error(read_xyData(file = "hi"))

  ##load example data step by step from GitHub
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/blob/master/samples/03yag02.mca"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/blob/master/samples/04nacl02.mca"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/1517474.cif"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/1d-1.spe"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/1d-2.spe"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/1d-3.spe"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/BT86.raw"), type = "list")
  expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/BT86_.UXD"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/Cu3Au-1.raw"), type = "list")
  expect_type(read_xyData(file = "https://github.com/wojdyr/xylib/raw/master/samples/Cu3Au-2.raw"), type = "list")

  ##check this, something is fishy here
  #expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/D1A5.dat"), type = "list")
  #expect_type(read_xyData(file = "https://raw.githubusercontent.com/wojdyr/xylib/master/samples/PSI_DMC.dat"), type = "list")

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
})
