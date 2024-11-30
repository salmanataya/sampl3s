#' Cluster Sampling
#'
#' This function performs cluster sampling by selecting a specified number of clusters randomly
#' from a given dataset and returning all elements within the selected clusters.
#'
#' @param data A data frame representing the sampling frame.
#' @param cluster_var A string specifying the column name in `data` used to define clusters.
#' @param n_clusters Integer. The number of clusters to select randomly.
#' @return A data frame containing all elements from the selected clusters.
#' @export
cluster_sampling <- function(data, cluster_var, n_clusters) {
  clusters <- unique(data[[cluster_var]])
  if (n_clusters > length(clusters)) {
    stop("The number of clusters requested exceeds the number of available clusters.")
  }
  selected_clusters <- sample(clusters, n_clusters, replace = FALSE)
  cluster_samples <- data[data[[cluster_var]] %in% selected_clusters, ]
  return(cluster_samples)
}
