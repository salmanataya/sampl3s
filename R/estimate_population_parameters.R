#' Estimate Population Parameters
#'
#' This function estimates population parameters such as mean, variance,
#' standard deviation, median, mode, and total population value based on
#' a given sample and population size.
#'
#' @param sample A numeric vector representing the sample data (subset of the population).
#' @param population_size Integer. The total size of the population.
#' @return A data frame containing the estimated population parameters: mean,
#' variance, standard deviation, median, mode, and total population value.
#' @export
estimate_population_parameters <- function(sample, population_size) {
  if (!is.numeric(sample)) {
    stop("Sample data must be numeric.")
  }

  if (!is.numeric(population_size) || population_size <= 0 || floor(population_size) != population_size) {
    stop("Population size must be a positive integer.")
  }

  if (length(sample) > population_size) {
    stop("Sample size cannot exceed population size.")
  }

  calculate_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  mean_est <- mean(sample)
  variance_est <- var(sample) * (population_size - 1) / length(sample) # Finite population correction
  sd_est <- sqrt(variance_est)
  median_est <- median(sample)
  mode_est <- calculate_mode(sample)
  total_est <- mean_est * population_size

  result <- data.frame(
    Mean = mean_est,
    Variance = variance_est,
    Standard_Deviation = sd_est,
    Median = median_est,
    Mode = mode_est,
    Total = total_est
  )

  return(result)
}
