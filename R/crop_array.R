crop_array <- function(image_array_1, image_array_2, value = 0) {

  if(!is.array(image_array_1 | !is.array(image_array_2))) {
    stop("image_array must be of class array")
  }

  for(i in 1:dim(image_array_1)[1]) {
    if(any(image_array_1[i,,] > value) | any(image_array_2[i,,] > value)) {
      i
      break
    }}

  for(j in dim(image_array_1)[1]:1) {
    if(any(image_array_1[j,,] > value) | any(image_array_2[j,,] > value)) {
      j
      break
    }}

  for(k in 1:dim(image_array_1)[2]) {
    if(any(image_array_1[,k,] > value) | any(image_array_2[,k,] > value)) {
      k
      break
    }}

  for(l in dim(image_array_1)[2]:1) {
    if(any(image_array_1[,l,] > value) | any(image_array_2[,l,] > value)) {
      l
      break
    }}

  for(m in 1:dim(image_array_1)[3]) {
    if(any(image_array_1[,,m] > value) | any(image_array_2[,,m] > value)) {
      m
      break
    }}

  for(n in dim(image_array_1)[3]:1) {
    if(any(image_array_1[,,n] > value) | any(image_array_2[,,n] > value)) {
      n
      break
    }}

  list(image_array_1[i:j, k:l, m:n], image_array_2[i:j, k:l, m:n])

}
