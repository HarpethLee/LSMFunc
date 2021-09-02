#' Convert an Array to xyz Coordinates
#'
#' This function takes a 2 or 3 dimensional image array and returns a matrix
#' containing values in the array along with their xyz coordinates within the
#' array. The xyz coordinates of a value will correspond to its indices in the
#' original array. This is useful for assigning xyz coordinates to values in a
#' raster image in order to find spacial data.
#'
#' @param array an 2 or 3-dimensional array
#'
#' @return matrix
#' @export
#'
#' @examples
#' test_array <- array(c(1,2,3,4), dim = c(2,2))
#' array_to_xyz(test_array)


array_to_xyz <- function(array) {

  array_df <- as.data.frame.table(array)

  if(!is.matrix(array)) {
    colnames(array_df) <- c("x", "y", "z", "intensity")
    array_df$z <- as.numeric(array_df$z)
    array_df$z <- array_df$z - 1
  } else {
    colnames(array_df) <- c("x", "y", "intensity")
  }

  array_df$x <- as.numeric(array_df$x)
  array_df$y <- as.numeric(array_df$y)

  # Subtract 1 so xyz coords in matrix match those in opened image
  array_df$x <- array_df$x - 1
  array_df$y <- array_df$y - 1

  as.matrix(array_df)
}
