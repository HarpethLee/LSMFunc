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
