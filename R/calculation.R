check_infinity <- function(ci, it, bound) {
  assertthat::assert_that(is.complex(ci))
  assertthat::assert_that(length(ci) == 1)
  assertthat::assert_that(is.numeric(it))
  assertthat::assert_that(it %% 1 == 0)
  z <- 0
  k <- 0
  while(Mod(z) < bound & k < it) {
    z <- z**2 + ci
    k <- k + 1
  }
  return(k)
}

mandelCalc <- function(real_margin, imag_margin, resolution, bound,
                      parallel = "no", n_cores) {
  width <- abs(diff(real_margin))
  height <- abs(diff(imag_margin))
  pixel_size <- min(width, height) / resolution
  real_grid <- seq(real_margin[1], real_margin[2], pixel_size)
  imag_grid <- seq(imag_margin[1], imag_margin[2], pixel_size)
  complex_grid <- expand.grid(real_grid, imag_grid)
  comp_v <- complex(real = complex_grid[, 1], imaginary = complex_grid[, 2])

  switch (parallel,
          "no" =  Infty <-
            sapply(comp_v, FUN = check_infinity, it = 100, bound = bound),
          "yes" = Infty <- parallel_run(comp_v, it = 100, bound = bound, n_cores)
  )


  data <- data.frame(grid = comp_v, volat. = Infty)
  return(data)
}

parallel_run <- function(comp_v, it, bound = bound, n_cores) {
  library(foreach)
  cl <- parallel::makeCluster(n_cores, outfile = "")
  doParallel::registerDoParallel(cl)
  pb <- txtProgressBar(0, max = length(comp_v), style = 3)
  infty <- foreach::foreach(i = 1:length(comp_v),
                            .combine = c,
                            .export = "check_infinity") %dopar%
    {
      setTxtProgressBar(pb, i)
      check_infinity(comp_v[i], it = it, bound = bound)
    }
  close(pb)
  parallel::stopCluster(cl)
  return(infty)
}
