test_that("BayLumFiles", {
  testthat::skip_on_cran()
  local_edition(3)


  ## crash function
    ## wrong number of arguments
    expect_error(
      write_BayLumFiles(
        folder = "test",
        SampleNames = c("sample 1", "sample 2"),
        DRenv = c(0.1, 0.2, 3),
        DRsource = c(3,4,5,19)),
      regexp = "\\[write\\_BayLumFiles\\(\\)\\] It seems that input is missing.*")

})
