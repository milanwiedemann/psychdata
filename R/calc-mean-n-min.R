#' Calculate mean function addressing item-level missing data using proration
#'
#' @param data Dataframe
#' @param n_min Minimum number of scores needed to calculate mean
#'
calc_mean_n_min <- function(data, n_min){
  means <- base::apply(data, 1, mean, na.rm = TRUE)
  nvalid <- base::apply(data, 1, function(data) base::sum(!base::is.na(data)))
  base::ifelse(nvalid >= n_min, means, NA)
}
