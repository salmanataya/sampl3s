#' Systematic Sampling
#'
#' This function performs systematic sampling, selecting elements from a population
#' at fixed intervals starting from a random point.
#'
#' @param population A vector representing the population elements.
#' @param sample_size Integer. The desired number of elements to sample.
#' @return A vector containing the systematically selected sample.
#' @export
systematic_sampling <- function(population, sample_size) {
  n <- length(population)
  k <- floor(n / sample_size)
  start <- sample(1:k, 1)
  indices <- seq(start, n, by = k)
  if (length(indices) > sample_size) {
    indices <- indices[1:sample_size]
  }
  return(population[indices])
}
