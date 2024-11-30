#' Margin of Error
#'
#' This function calculates the margin of error, which is an estimate of the maximum error in sample estimation.
#' It is useful for determining how much a sample estimate can deviate from the actual population value,
#' based on a given confidence level.
#'
#' @param standard_deviation A numeric value representing the standard deviation of the population or sample.
#' @param sample_size A numeric value representing the sample size.
#' @param confidence_level A numeric value between 0 and 1 representing the confidence level (e.g., 0.95 for 95\% confidence).
#' @return A numeric value representing the margin of error.
#' @export
margin_of_error <- function(standard_deviation, sample_size, confidence_level) {
  # Menghitung margin of error berdasarkan deviasi standar, ukuran sampel, dan tingkat kepercayaan.

  z_score <- qnorm((1 + confidence_level) / 2)  # Menghitung z-score untuk tingkat kepercayaan
  margin_of_error <- z_score * (standard_deviation / sqrt(sample_size))

  return(margin_of_error)
}
