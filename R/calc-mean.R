#' Calculate mean of variables
#'
#' @param data Wide dataframe.
#' @param id_str String of identifier variable.
#' @param var_str String of variable to calculate mean for.
#' @param session_str String of session number.
#' @param n_min Minimum number of available scores to calculate mean.
#'
#' @return
#' @export
#'
#' @examples
calc_mean <- function(data, id_str, var_str, session_str, n_min){

  # Select variables based on variable names
  data_select_var <- data %>%
    select(id_str, contains(var_str))

  # Create emptty string for list of variable names
  var_names <- ""

  # Create tibble with only ids used to start joining at the end and also will be the return object
  data_join_start_end <- data %>%
    select(id_str)

  # Start looping through list of variables
  for (i in 1:length(session_str)) {

    # Select all items from a sepecific session
    data_select_var_ses <- data_select_var %>%
      select(id_str, contains(session_str[i]))

    # Get all variable names of items to inclode in rowMeans mutate
    var_names <- data_select_var %>%
      select(contains(session_str[i])) %>%
      names()
    item_count <- length(var_names)

    # Create variable name for mean
    var_str_i <- paste(var_str, "mean", substr(x = session_str[i], start = 1, stop = (nchar(session_str[i]) - 1)), sep = "_")

    # # Calvulate number of available scores
    # data_select_var_ses_mean <- data_select_var_ses %>%
    #   mutate(n = sum(is.na(variable)))

    # Calculate mean
    data_select_var_ses_mean <- data_select_var_ses %>%
      mutate(!!var_str_i := calc_mean_n_min(.[, 2:(item_count + 1)], n_min))

    data_loop <- data_select_var_ses_mean %>%
      select(id_str, !!var_str_i)



    data_join_start_end <- left_join(data_join_start_end, data_loop)
  }

  data_join_start_end

}
