#' Calculate Sample Size
#'
#' This function calculates the sample size needed based on population variance,
#' margin of error, and confidence level.
#'
#' @param population_variance Numeric. Estimated variance of the population.
#' @param margin_of_error Numeric. Desired margin of error.
#' @param confidence_level Numeric. Desired confidence level (default is 0.95).
#' @return Numeric. The recommended sample size.
#' @examples
#' generate_sample_size(population_variance = 0.25, margin_of_error = 0.05, confidence_level = 0.95)
#' @export
generate_sample_size <- function(population_variance, margin_of_error, confidence_level = 0.95) {
  z <- qnorm(1 - (1 - confidence_level) / 2)
  n <- (z^2 * population_variance) / (margin_of_error^2)
  n <- ceiling(n)
  return(n)
}
