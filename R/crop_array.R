#' Crops an Image Array
#'
#' This function crops an image array by removing all matrices on outer edges
#' that do not contain any entries greater than a specified value.
#'
#' @param image_array a 3-dimensional image array
#' @param value maximum value that cropped matrices can contain
#'
#' @return array
#' @export


crop_array <- function(image_array, value = 0) {

  if(!is.array(image_array)) {
    stop("image_array must be of class array")
  }

  if(!is.numeric(value)) {
    stop("value must be a numeric")
  }

  if(value < 0) {
    warning("value is a negative number")
  }

  for(i in 1:dim(image_array)[1]) {
    if(any(image_array[i,,] > value) | any(image_array_2[i,,] > value)) {
      i
      break
    }}

  for(j in dim(image_array)[1]:1) {
    if(any(image_array[j,,] > value) | any(image_array_2[j,,] > value)) {
      j
      break
    }}

  for(k in 1:dim(image_array)[2]) {
    if(any(image_array[,k,] > value) | any(image_array_2[,k,] > value)) {
      k
      break
    }}

  for(l in dim(image_array)[2]:1) {
    if(any(image_array[,l,] > value) | any(image_array_2[,l,] > value)) {
      l
      break
    }}

  for(m in 1:dim(image_array)[3]) {
    if(any(image_array[,,m] > value) | any(image_array_2[,,m] > value)) {
      m
      break
    }}

  for(n in dim(image_array)[3]:1) {
    if(any(image_array[,,n] > value) | any(image_array_2[,,n] > value)) {
      n
      break
    }}

  image_array[i:j, k:l, m:n]

}
