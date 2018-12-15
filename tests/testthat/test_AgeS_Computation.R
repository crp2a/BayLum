context("Test AgeS_Computation()")

test_that("Full function test", {
  testthat::skip_on_cran()

  ## load data
  data(DATA1,envir = environment())
  data(DATA2,envir = environment())
  Data <- combine_DataFiles(DATA2,DATA1)

  ## without common error and without stratigraphic constraints
  expect_is(AgeS_Computation(
    DATA = Data,
    Nb_sample = 2,
    SampleNames = c("GDB5","GDB3"),
    PriorAge = c(1,10,20,60),
    Iter = 50,
    n.chains = 2,
    quiet = TRUE
  ), class = "BayLum.list")


})

