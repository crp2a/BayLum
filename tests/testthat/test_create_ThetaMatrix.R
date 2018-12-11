context("Test create_ThetaMatrix()")

test_that("Full function test", {
  testthat::skip_on_cran()

  ##try to crash the function
  expect_error(create_ThetaMatrix(input = 2))
  expect_error(create_ThetaMatrix(input = "test"), regexp = "File test does not exist!")
  expect_error(create_ThetaMatrix(input = data.frame()), regexp = "The input data.frame needs at least 2 rows!")
  expect_error(create_ThetaMatrix(input = data.frame(), sigma_s = c(test = 1)),
               regexp = "Value names do not match in 'sigma_s', please check the manual!")

  ##create reference dataset
  file <- tempfile(fileext = ".csv")
  expect_type(df <- create_ThetaMatrix(output_file = file), type = "list")

  ##add some lines
  df <- rbind(df,df)
  df[] <- 1

  ##execute
  expect_warning(create_ThetaMatrix(input = df))
  create_ThetaMatrix(input = df, output_file = tempfile())

  ##add NA
  df[1,1] <- NA
  df$DR_GAMMA_TOTAL <- 0
  expect_warning(create_ThetaMatrix(input = df), regexp = "NA values found and set to 0.")

})
