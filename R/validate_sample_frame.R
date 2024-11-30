#' Validate Sample Frame
#'
#' This function checks if a given sample frame is complete and free from major biases.
#' It performs a series of validations, including checking for missing values, duplicate entries,
#' empty rows or columns, and potential bias in categorical columns.
#'
#' @param sample_frame A data frame representing the sample frame to validate.
#' @return Messages, warnings, or errors indicating the quality of the sample frame.
#' @examples
#' # Example of a sample frame
#' sample_frame <- data.frame(
#'   ID = 1:10,
#'   Attribute1 = c("A", "B", "A", "A", "B", "B", "A", "A", "B", "B"),
#'   Attribute2 = c(NA, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' )
#'
#' # Validate the sample frame
#' validate_sample_frame(sample_frame)
#' @export
validate_sample_frame <- function(sample_frame) {
  if (!is.data.frame(sample_frame)) {
    stop("Input must be a data frame.")
  }

  # Check for missing rows or columns
  if (nrow(sample_frame) == 0 || ncol(sample_frame) == 0) {
    stop("Sample frame must not be empty.")
  }

  # Check for missing values
  if (any(is.na(sample_frame))) {
    warning("Sample frame contains missing values.")
  }

  # Check for duplicate entries
  if (anyDuplicated(sample_frame[, 1]) > 0) {
    warning("Sample frame contains duplicate entries in the first column (assumed to be IDs).")
  }

  # Check for bias: Evaluate distribution of key attributes
  for (col in names(sample_frame)) {
    if (is.factor(sample_frame[[col]]) || is.character(sample_frame[[col]])) {
      proportions <- prop.table(table(sample_frame[[col]]))
      if (any(proportions > 0.8)) {
        warning(paste("Column", col, "is highly imbalanced. Check for potential bias."))
      }
    }
  }

  # If no major issues are detected
  message("Sample frame validation complete. No critical issues found.")
}
