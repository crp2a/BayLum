context("Test plot_MCMC()")

test_that("Full function test", {
  testthat::skip_on_cran()

  ##test function stop
  expect_error(plot_MCMC("character"), label = "[plot_MCMC()] 'sample' has to be of type 'mcmc.list'!", )

  data(MCMCsample,envir = environment())
  object <- coda::as.mcmc(MCMCsample)

  ##test function itself
  expect_silent(plot_MCMC(object))

  ##test arguments

    ##plot.single
    expect_silent(plot_MCMC(object, plot_single = TRUE))

    ##sample_names
    expect_silent(plot_MCMC(object, sample_names = "Test"))
    expect_silent(plot_MCMC(object, mtext = "Test"))
    expect_silent(plot_MCMC(object, sample_names = c("Test", "Test2")))

    ##n.chains
    expect_silent(plot_MCMC(object, n.chains = 1))

    ##n.iter
    expect_silent(plot_MCMC(object, n.iter = 10))
    expect_silent(plot_MCMC(object, n.iter = 10:20))
    expect_warning(plot_MCMC(object, n.iter = c(10,20000)))
    expect_warning(plot_MCMC(object, n.iter = -1))

    #smooth
    expect_silent(plot_MCMC(object, smooth = TRUE))



})

