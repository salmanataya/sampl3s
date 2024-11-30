#' Visualize Sampling
#'
#' This function creates visualizations to represent how samples are selected
#' based on the sampling method. It supports stratified sampling, cluster
#' sampling, or general visualizations for other sampling methods.
#'
#' @param sample_data A data frame containing the sample data.
#' @param sampling_method A string indicating the sampling method. Options
#' are "Stratified", "Cluster", or others.
#' @param strata Optional. A column in the data representing strata for stratified sampling.
#' @param cluster Optional. A column in the data representing cluster for cluster sampling.
#' @return A ggplot object representing the visualization of the sampling process.
#' @export
visualize_sampling <- function(sample_data, sampling_method, strata = NULL, cluster = NULL) {
  # Memvisualisasikan bagaimana sampel diambil berdasarkan metode sampling yang digunakan.

  if (sampling_method == "Stratified") {
    # Memastikan bahwa data memiliki kolom strata
    if (is.null(strata)) {
      stop("Strata harus disediakan untuk stratified sampling.")
    }

    # Menambahkan kolom strata jika belum ada
    if (!"strata" %in% colnames(sample_data)) {
      sample_data <- dplyr::mutate(sample_data, strata = strata)
    }

    # Visualisasi stratified sampling
    ggplot2::ggplot(sample_data, ggplot2::aes(x = sample_data$value, fill = strata)) +
      ggplot2::geom_density(position = "identity", alpha = 0.5) +
      ggplot2::labs(title = "Distribusi Sampel - Stratified Sampling",
                    x = "Nilai",
                    y = "Frekuensi") +
      ggplot2::theme_minimal()

  } else if (sampling_method == "Cluster") {
    # Memastikan data memiliki kolom cluster
    if (is.null(cluster)) {
      stop("Kolom cluster harus disediakan untuk cluster sampling.")
    }

    # Menambahkan kolom cluster jika belum ada
    if (!"cluster" %in% colnames(sample_data)) {
      sample_data <- dplyr::mutate(sample_data, cluster = cluster)
    }

    # Visualisasi cluster sampling
    ggplot2::ggplot(sample_data, ggplot2::aes(x = as.factor(cluster), y = sample_data$value)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(title = "Distribusi Sampel - Cluster Sampling",
                    x = "Cluster",
                    y = "Nilai") +
      ggplot2::theme_minimal()

  } else {
    # Visualisasi untuk metode sampling lainnya
    ggplot2::ggplot(sample_data, ggplot2::aes(x = sample_data$value)) +
      ggplot2::geom_histogram(bins = 30, ggplot2::aes(y = ..density..), fill = "blue", alpha = 0.7) +
      ggplot2::labs(title = paste("Distribusi Sampel -", sampling_method),
                    x = "Nilai",
                    y = "Frekuensi") +
      ggplot2::theme_minimal()
  }
}
