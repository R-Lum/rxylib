test_that("Slice channels", {
  spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  spc_cnf <- read(spc_file)

  expect_equal(length(spc_cnf), 1024)
  spc1 <- signal_slice(spc_cnf, 1:10)
  expect_equal(length(spc1), 10)
  expect_equal(get_hash(spc_cnf), get_hash(spc1))
  spc2 <- signal_slice(spc_cnf, -1:-10)
  expect_equal(length(spc2), 1014)
  expect_equal(get_hash(spc_cnf), get_hash(spc2))

  expect_error(signal_slice(spc_cnf, 1, -2))

  spc_files <- system.file("extdata/", package = "gamma")
  spc_set <- read(spc_files)

  spc3 <- signal_slice(spc_set, 1:10)
  expect_true(all(lengths(spc3) == 10))
  expect_equal(get_hash(spc_set), get_hash(spc3))
})
test_that("Stabilize signal", {
  spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  spc_cnf <- read(spc_file)

  spc1 <- signal_stabilize(spc_cnf, sqrt)
  expect_equal(get_hash(spc_cnf), get_hash(spc1))

  spc_files <- system.file("extdata/", package = "gamma")
  spc_set <- read(spc_files)

  spc2 <- signal_stabilize(spc_set, sqrt)
  expect_equal(get_hash(spc_set), get_hash(spc2))
})
test_that("Smooth signal", {
  smooth_methods <- c("rectangular", "triangular", "savitzky")

  spc_file <- system.file("extdata/LaBr.CNF", package = "gamma")
  spc_cnf <- read(spc_file)

  for (i in smooth_methods) {
    spc1 <- signal_smooth(spc_cnf, method = i)
    expect_equal(get_hash(spc_cnf), get_hash(spc1))
  }

  expect_error(signal_smooth(spc_cnf, method = "rectangular", m = 2),
               "must be an odd integer")

  spc_files <- system.file("extdata/", package = "gamma")
  spc_set <- read(spc_files)

  for (i in smooth_methods) {
    spc2 <- signal_smooth(spc_set, method = i)
    expect_equal(get_hash(spc_set), get_hash(spc2))
  }
})
