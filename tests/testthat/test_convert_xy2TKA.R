test_that("General tests", {
  testthat::skip_on_cran()
  local_edition(3)

  ##general stop ... not object
  expect_error(convert_xy2TKA())

  ##wrong input
  expect_error(convert_xy2TKA(object = data.frame()))

})


test_that("Input and output", {
  testthat::skip_on_cran()
  local_edition(3)

  ##input as xylib object
  expect_type(
    convert_xy2TKA(
      object = read_xyData(system.file("extdata/ExampleSpectrum.CNF", package = "rxylib"))),
    "list")

  ##input as file path
  expect_type(
    convert_xy2TKA(
      object = system.file("extdata/ExampleSpectrum.CNF", package = "rxylib")),
    "list")

  ##input file path not correct
  expect_type(
    convert_xy2TKA(
      object = system.file("extdata/ExampleSpectrum.TEST", package = "rxylib")),
    "NULL")

  ##input format not recognised
  expect_error(
    convert_xy2TKA(
      object = system.file("extdata/ExampleChiPlot.chi", package = "rxylib")))

  ##test output writing
  path <- tempfile()

    ##with input file and no name
    expect_silent(convert_xy2TKA(
      object = system.file("extdata/ExampleSpectrum.CNF", package = "rxylib"),
      file = path
      ))

    ##with file name
    expect_silent(convert_xy2TKA(
      object = system.file("extdata/ExampleSpectrum.CNF", package = "rxylib"),
      file = paste0(path, "test.TKA")
    ))


})

