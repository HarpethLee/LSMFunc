grid_prop <- function(image_array, floor = 1000, grid_dim = c(4,4,4)) {

  div_seq_1 <- seq(1, dim(image_array)[1], by = dim(image_array)[1]/grid_dim)
  div_seq_2 <- seq(1, dim(image_array)[2], by = dim(image_array)[2]/grid_dim)

  section_prop <- matrix(nrow = grid_dim, ncol = grid_dim)

  for(i in 1:grid_dim) {
    for(j in 1:grid_dim) {
      section <- image_array[div_seq_1[i]:(div_seq_1[i] + (dim(image_array)[1]/grid_dim - 1)),
                             div_seq_2[j]:(div_seq_2[j] + (dim(image_array)[2]/grid_dim - 1)), ]
      section_prop[i, j] <- sum(section > floor)/length(section)
    }
  }
  section_prop
}
