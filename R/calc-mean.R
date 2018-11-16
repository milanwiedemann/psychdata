#' Calculate mean of variables
#' @description Calculate mean addressing item-level missing data using proration
#' @param data Wide dataframe.
#' @param id_str String of identifier variable.
#' @param var_str String of variable to calculate mean for.
#' @param session_str String of session number.
#' @param n_min Minimum number of available scores to calculate mean.
#' @param item_scores Add item scores after mean.
#' @param sep seperator for variable names.
#' @param sort_mean_item Logical, if TRUE and multiple sessions then output dataframe will be organised mean_timepoint followed by all items for that timepoint, if FALSE all means will come after id variable followed by all items.
#' @export
#'
calc_mean <- function(data, id_str, var_str, session_str, n_min, item_scores = FALSE, sort_mean_item = TRUE, sep = "_"){

  session_str <- base::c(session_str)

  # Select variables based on variable names
  data_select_var <- data %>%
    dplyr::select(id_str, contains(var_str))

  # Create emptty string for list of variable names
  var_names <- ""

  # Create tibble with only ids used to start joining at the end and also will be the return object
  data_join_start_end <- data %>%
    dplyr::select(id_str)

  # Start looping through list of variables
  for (i in 1:base::length(session_str)) {

    # Select all items from a sepecific session
    data_select_var_ses <- data_select_var %>%
      dplyr::select(id_str, contains(session_str[i]))

    # Get all variable names of items to inclode in rowMeans mutate
    var_names <- data_select_var %>%
      dplyr::select(contains(session_str[i])) %>%
      base::names()
    item_count <- base::length(var_names)

    # Create variable name for mean
    var_str_i <- base::paste(var_str, "mean", session_str[i], sep = sep)

    # # Calvulate number of available scores
    # data_select_var_ses_mean <- data_select_var_ses %>%
    #   mutate(n = sum(is.na(variable)))

    # Calculate mean
    data_select_var_ses_mean <- data_select_var_ses %>%
      dplyr::mutate(!!var_str_i := psychdata:::calc_mean_n_min(.[ , 2:(item_count + 1)], n_min))

    data_loop <- data_select_var_ses_mean %>%
      dplyr::select(id_str, !!var_str_i)

    # Here create dataframe, keep on adding to the same dataframe as looping through sessions
    # I'm not happy with this left_join approach but it works and I cant think of a better way right now

    if (item_scores == FALSE){
    # If item_scores not asked for just add the data_loop (with the session means)
    data_join_start_end <- dplyr::left_join(data_join_start_end, data_loop, by = id_str)
    } else {

    data_join_start_end <- dplyr::left_join(data_join_start_end, data_loop, by = id_str)
    data_join_start_end <- dplyr::left_join(data_join_start_end, data_select_var_ses, by = id_str)
    }

  }


  # Sort everything at the end
  if (sort_mean_item == TRUE) {
    data_join_start_end
  } else {
    dplyr::select(data_join_start_end, id_str, dplyr::contains("mean"), dplyr::everything())
  }

}
