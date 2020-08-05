#' Title
#'
#' @param real_mar a vector of two numbers indicating the lower and upper limit
#' on the real axis.
#' @param imag_mar a vector of two numbers indicating the lower and upper limit
#' on the imaginary axis.
#' @param resol the number of equally spaced points on the smaller of the two
#' axes.
#' @param n_cores number of cores if the parallel function is chosen.
#' @param parallel default is "yes". It "no" a sapply is used for calculation
#' @param cmd logical: if the function is used in an executable file,
#'  see details.
#'
#' @return a .tiff image stored in the current working directory, where the
#' function was used.
#' @export
#'
#' @examples
mandelViz <- function(real_mar = c(-1.6, 1), imag_mar = c(-1, 1), resol = 250,
                      n_cores =  parallel::detectCores() - 1, parallel = "yes",
                      cmd = FALSE) {

  if (cmd) {
    args <- commandArgs()
    arg <- tail(args, n = 6)
    assertthat::assert_that(is.character(arg[1:3]))

    real_mar <- as.numeric(eval(parse(text = arg[1])))
    imag_mar <- as.numeric(eval(parse(text = arg[2])))
    resol <- as.numeric(eval(parse(text = arg[3])))
    n_cores <- as.numeric(eval(parse(text = arg[4])))
  } else {
    assertthat::assert_that(is.numeric(c(real_mar, imag_mar, resol, n_cores)))
    assertthat::assert_that(is.character(parallel))
    assertthat::assert_that(tolower(parallel) %in% c("yes" ,"no"))
    assertthat::assert_that(n_cores <= parallel::detectCores())
    assertthat::assert_that(is.logical(cmd))
  }

  assertthat::assert_that(length(real_mar) == 2)
  assertthat::assert_that(length(imag_mar) == 2)
  assertthat::assert_that(length(resol) == 1)
  assertthat::assert_that(!any(is.na(c(real_mar, imag_mar, resol))))

  res <- mandelCalc(real_mar, imag_mar, resolution = resol,
                    parallel = parallel, n_cores = n_cores)

  g <- ggplot2::ggplot(res, ggplot2::aes(x = Re(grid),
                                y = Im(grid),
                                fill = volat.)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradientn(colours = c('#8A0808', '#F4FA58', '#2EFE2E',
                                     '#01A9DB', '#000000', '#0B0B61'))


  # prepare plot dimensions
  sq_n_points <- sqrt(nrow(res))
  aspect <- diff(imag_mar) / diff(real_mar)
  w_px <- 10 * sq_n_points / aspect # each point has 10 pixels to be represented
  h_px <- 10 * sq_n_points * aspect
  plt_name <- paste("mandelbrotset_", Sys.time(), ".tiff", sep = "")
  tiff(filename = plt_name, width = w_px, height = h_px,
       res = resol)
  g
  dev.off()
}

