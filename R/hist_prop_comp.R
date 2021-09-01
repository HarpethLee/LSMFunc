hist_prop_comp <- function(x, y, to = 2^16, by = 2^12) {

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
