#' Create variable names
#'
#' @param var_str String, variable name
#' @param sess_start First session
#' @param sess_end Last session
#' @param sess_str String, abbreviation for timepoint, default s for session
#' @param var_sess_sep String, seperator between var_str and sess_str
#' @export

create_var_names <- function(var_str, sess_start, sess_end, sess_str = "s", var_sess_sep = "_") {

  # Calc number of rep based on start and end session
  rep_num <- length(sess_start:sess_end)

  # Create strings as vector
  map2(rep(paste(var_str, sess_str, sep = var_sess_sep), rep_num), sess_start:sess_end, paste0) %>%
    base::unlist(., use.names = FALSE)
}
