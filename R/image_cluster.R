#' Finds Connected Clusters Within an Image
#'
#' This function finds clusters of points in an image and returns a list of points
#' in each cluster. Image arrays are first filtered, removing all points below the
#' set 'min_intensity'. Then, a connectivity graph is made with the remaining points
#' in the array after being given xyz coordinate corresponding to a points location
#' within the image array (this is done using the 'array_to_xyz() function). The
#' maximum distance between connected points in the graph is decided by the variable
#' 'max_dist'. A list is then returned containing the points in each graph.
#'
#' @param x An image array
#' @param max_dist Maximum distance between connected points
#' @param min_intensity Minimum intensity for points
#' @param min_cluster_size Minimum size of cluster in returned list
#' @param sample_prop Proportion of
#' @param sample_type Takes values "grid" or "random"
#'
#' @return List of clusters
#' @export


image_cluster <- function(x,
                          max_dist = 10,
                          min_intensity = 2000,
                          min_cluster_size = 50,
                          sample_prop = 2^(-8),
                          sample_type = "grid") {

  if(!is.array(x)) {
    stop("x must be an object of the class array")
  }

  if(!(length(dim(x)) %in% c(2,3))) {
    stop("x must be an array with either 2 or 3 dimensions")
  }

  # Makes sure sample_prop is a numeric. If it isn't a number between 0 and 1, it sets
  # the prop equal to 0 if the value is negative or 1 if the value is greater than 1.
  if(!is.numeric(sample_prop)) {
    stop("sample_prop must be a numeric")
  } else if(sample_prop > 1) {
    sample_prop = 1
  } else if(sample_prop < 0) {
    sample_prop = 0
  }

  if(!is.numeric(max_dist)) {
    stop("max_dist must be a numeric")
  } else if(max_dist <= 0) {
    stop("max_dist must be a positive number")
  }

  if(!is.numeric(min_intensity)) {
    stop("min_intensity must be a numeric")
  }

  if(!is.numeric(min_cluster_size)) {
    stop("min_cluster_size must be a numeric")
  } else if(min_cluster_size < 0) {
    stop("min_cluster_size must be a positive number")
  }


  # This saves converts the array to a matrix containing x, y, and, z coordinates along with the pixels value
  # as well as removing all entries with an intensity below the min_intensity
  x <- array_to_xyz(x)


  if(dim(x)[2] == 4) {
    x <- x[x[, 4] >= min_intensity,]
  } else if(dim(x)[2] == 3) {
    x <- x[x[, 3] >= min_intensity,]
  }

  # Takes a sample from the matrix. This allows the igraph function to run or at least run faster.
  # When the sample is too large, there is a chance of the dist() function maxing our R's memory.

  if(sample_type %in% c("grid", "g", "Grid", "GRID")) {
    samp <- seq(1, nrow(x), by = 1/sample_prop)
    x <- x[samp, ]
  } else if(sample_type %in% c("random", "r", "Random", "RANDOM")) {
    x <- x[sample(nrow(x), nrow(x)*sample_prop), ]
  } else {
    stop("sample_type takes the values `grid` and `random`")
  }


  if(dim(x)[1] == 0) {
    stop("min_intensity is too large. No pixels in the image have values above this level.")
  }

  # Creates new matrix the doesn't contain the intensity values of pixels
  if(dim(x)[2] == 4) {
    x_alt <- x[, -4]
  } else if(dim(mtrx)[2] == 3) {
    x_alt <- x[, -3]
  }


  # Creates a matrix containing the distance between points
  dist_x <- as.matrix(dist(x_alt))


  # Creates vector to store clustered points in.
  clustered_points <- replicate(ncol(dist_x), NULL)

  # Finds the indices of all points that are within 10 pixels of each other and saves them as a list
  for(i in 1:ncol(dist_x)) {
    for(j in 1:nrow(dist_x))  {
      if(dist_x[i, j] < max_dist & i != j) {
        clustered_points[[i]] <- c(clustered_points[[i]], j)
      } } }


  # Using the clustered_points list, these function from the igraph package create the cluster list
  g = graph_from_adj_list(clustered_points, mode = "all")
  comp = components(g)
  cluster <- igraph::groups(comp)


  # Changes class of cluster from array to list
  dim(cluster) <- NULL


  # Removes all clusters below a certain size from list
  cluster <- cluster[lapply(cluster, length) > min_cluster_size]


  # Turns cluster to list of matrices containing x,y,z coordinates and intensity of pixels in cluster.
  cluster <- lapply(cluster, function(a) {
    x[a,]
  })

  cluster

}
