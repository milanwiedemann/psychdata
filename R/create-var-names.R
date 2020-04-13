#' Create variable names
#' @description Create variable names for repeated measures
#' @param var_str String for variable
#' @param sess_start First session
#' @param sess_end Last session
#' @param sess_str String, abbreviation for timepoint, default s for session
#' @param var_sess_sep String, seperator between var_str and sess_str
#' @export

create_var_names <- function(var_str, sess_start, sess_end, sess_str = "s", var_sess_sep = "_", sess = TRUE, items = FALSE, item_str = "i", item_start = NULL, item_end = NULL) {


  if (items == FALSE & sess == TRUE) {

    # Calc number of rep based on start and end session
    rep_num_sess <- base::length(sess_start:sess_end)

    # Create strings as vector
    purrr::map2(base::rep(base::paste(var_str, sess_str, sep = var_sess_sep), rep_num_sess), sess_start:sess_end, base::paste0) %>%
      base::unlist(., use.names = FALSE)

  } else if (items == TRUE & sess == TRUE) {

    message("Items by item for the win!")

    # Calc number of rep based on start and end session
    rep_num_sess <- base::length(sess_start:sess_end)
    rep_num_items <- base::length(item_start:item_end)

    vars_session_by_session <- purrr::map2(base::rep(base::paste(var_str, sess_str, sep = var_sess_sep), rep_num_sess), sess_start:sess_end, base::paste0) %>%
      base::unlist(., use.names = FALSE)

    purrr::map2(.x = base::rep(base::paste(vars_session_by_session, item_str, sep = "_"), rep_num_items), .y = rep(item_start:item_end, each = rep_num_sess), .f = base::paste0) %>%
      base::unlist(., use.names = FALSE)


  } else if (items == TRUE & sess == FALSE) {


    # Calc number of rep based on start and end session
    rep_num_items <- base::length(item_start:item_end)

    # Create strings as vector
    purrr::map2(base::rep(base::paste(var_str, item_str, sep = var_sess_sep), rep_num_items), item_start:item_end, base::paste0) %>%
      base::unlist(., use.names = FALSE)
  }

  }

