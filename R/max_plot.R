#' Plot 3-Dimensional Images as 2-dimensional Projections
#'
#' Plots a 3-dimensional grayscale raster image as three separate 2-dimensional
#' cimg plots. Each of these plots is the projection of maximum values from a
#' 'face' of the original image onto a plane. These three images give us an
#' approximation of the original image that can be viewed in R.
#'
#' @param array a 3-dimensional image array
#'
#' @return cimg plot
#' @export


max_plot <- function(array) {
  num_rows <- dim(array)[1]
  num_cols <- dim(array)[2]
  num_mtrs <- dim(array)[3]

  max_intensity_1 <- matrix(nrow = num_cols, ncol = num_mtrs)
  for(i in 1:num_cols) {
    for(j in 1:num_mtrs) {
      max_intensity_1[i, j] <- max(array[, i, j])
    }
  }

  max_intensity_2 <- matrix(nrow = num_rows, ncol = num_cols)

  for(i in 1:num_rows) {
    for(j in 1:num_cols) {
      max_intensity_2[i, j] <- max(array[i, j,])
    }
  }
  max_intensity_3 <- matrix(nrow = num_rows, ncol = num_mtrs)
  for(i in 1:num_rows) {
    for(j in 1:num_mtrs) {
      max_intensity_3[i, j] <- max(array[i, , j])
    }
  }

  plot(as.cimg(max_intensity_1))
  plot(as.cimg(max_intensity_2))
  plot(as.cimg(max_intensity_3))
}
