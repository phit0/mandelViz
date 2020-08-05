test_that("wrong input", {
  expect_error(mandelViz(real_mar = "a", resol = 5))
  expect_error(mandelViz(imag_mar = c(1, 2, 3), resol = 5))
  expect_error(mandelViz(imag_mar = 1, resol = 5))
  expect_error(mandelViz(resol = "5"))
  expect_error(mandelViz(n_cores = parallel::detectCores() + 1, resol = 5))
  expect_error(mandelViz(resol = 5, cmd = "no"))
  expect_error(mandelViz(resol = 5, parallel = 1))
  expect_error(mandelViz(resol = 5, parallel = "sey"))
})

