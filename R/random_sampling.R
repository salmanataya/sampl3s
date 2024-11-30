#' Random Sampling
#'
#' This function performs random sampling from a given sample frame.
#'
#' @param sample_frame A data frame or vector representing the sampling frame.
#' @param sample_size Integer. The desired size of the sample.
#' @return A data frame or vector containing the random sample.
#' @export
random_sampling <- function(sample_frame, sample_size) {
  if (sample_size > length(sample_frame)) {
    stop("Sample size must be less than or equal to the size of the sample frame.")
  }

  sampled_data <- sample(sample_frame, sample_size)
  return(sampled_data)
}
