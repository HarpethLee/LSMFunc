#' Proportion of Overlap Between Image Grayscale Distributions
#'
#' This function compares the grayscale distributions of two images. It finds
#' the histogram for the two images at a given bin size and then compares the
#' proportion of difference between the two.
#'
#' @param x An image array
#' @param y An image array
#' @param to Maximum value included in grayscale distribution
#' @param by Size of bins in the grayscale distribution
#'
#' @return A Data Frame
#' @export


hist_prop_comp <- function(x, y, to = 2^16, by = 2^4) {

  if(!is.numeric(x) | !is.numeric(y)) {
    stop("x and y must be of type numeric")
  }

  if(!is.numeric(to)) {
    stop("to must be a numeric")
  }

  if(to <= 0) {
    stop("to must be a positive number")
  }

  if(!is.numeric(by)) {
    stop("by must be a numeric")
  }

  if(by <= 0) {
    stop("by must be a positive number")
  }

  break_seq = seq(0, to, by = by)

  x_hist <- hist(x, breaks = break_seq, plot = FALSE)
  y_hist <- hist(y, breaks = break_seq, plot = FALSE)

  x_props <- x_hist$counts/length(x)
  y_props <- y_hist$counts/length(y)

  diff_df <- data.frame(bin = x_hist$breaks[2:length(x_hist$breaks)],
                        diff = abs(x_props - y_props))
  diff_df <- arrange(diff_df, desc(diff))
  diff_df <- head(diff_df, 5)

  prop_difference = sum(abs(x_props - y_props))/2
  correlation = cor(x_props, y_props)
  data.frame(prop_difference, correlation)
}
