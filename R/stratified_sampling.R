#' Stratified Sampling
#'
#' This function performs stratified random sampling, where the population is divided into strata
#' (subgroups) based on a specified variable, and samples are taken from each stratum according to
#' the specified sample size per stratum.
#'
#' @param data A data frame representing the sampling frame.
#' @param strata_var A string specifying the column name in `data` used to define strata.
#' @param sample_size_per_strata A named vector where names are the strata levels and values are the desired sample sizes for each stratum.
#' @return A data frame containing the stratified random sample.
#' @export
stratified_sampling <- function(data, strata_var, sample_size_per_strata) {
  stratified_samples <- list()
  strata_levels <- unique(data[[strata_var]])
  for (stratum in strata_levels) {
    stratum_data <- subset(data, data[[strata_var]] == stratum)
    n_samples <- sample_size_per_strata[stratum]
    if (n_samples > nrow(stratum_data)) {
      stop(paste("Sample size for stratum", stratum, "exceeds available data."))
    }
    stratified_samples[[stratum]] <- stratum_data[sample(1:nrow(stratum_data), n_samples, replace = FALSE), ]
  }
  final_sample <- do.call(rbind, stratified_samples)
  return(final_sample)
}
