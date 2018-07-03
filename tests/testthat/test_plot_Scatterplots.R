context("Test plot_Scatterplots()")

test_that("Full function test", {
  testthat::skip_on_cran()

  #load test data
  data(AgeS,envir = environment())

  ##force function stop
  expect_error(plot_Scatterplots("test"),
               regexp =  "Wrong input, only objects of type 'mcmc.list' are allowed. Please check the manual!")
  expect_error(plot_Scatterplots(object = AgeS$Sampling, variables = c("A", "B")),
               regexp = "You can only select one variable at the time!")
  expect_error(plot_Scatterplots(object = AgeS$Sampling, variables = c("B")),
               regexp = " Invalid 'variables', they did not match your dataset. Variable names of your dataset: A, D, sD.")

  ##run function usual
  expect_silent(plot_Scatterplots(object = AgeS$Sampling))
  expect_warning(plot_Scatterplots(object = AgeS$Sampling, sample_names = "1"),
                 regexp = "length of 'sample_names' shorter than the number of samples; default values used!")
  expect_warning(plot_Scatterplots(object = AgeS$Sampling, sample_selection = 1:100),
                 regexp = "You have only 2 samples. 'sample_selection' wrong, reset to default!")

  expect_warning(plot_Scatterplots(object = AgeS$Sampling, n.chains = 10),
                 regexp = "'n.chains' setting wrong. You have 3 chains, reset to default")
})
