#' Check range of item scores
#'
#' @description Function to check if all item scores are within the possible range. If values are out of range, show the scores for that case together with id.
#' @param data Wide dataframe.
#' @param id_str String of identifier (id) variable.
#' @param var_str String of variable to check.
#' @param session_str String of session number.
#' @param range_min Number of minimum possible value of scale.
#' @param range_max Number of maximum possible value of scale.
#'
#' @return Dataframe
#' @export
#'
#' @examples
check_item_range <- function(data, id_str, var_str, session_str, range_min, range_max){

  # Do some sort of if else stuff in case id is character

  # Get max of id
  max_id <- max(data$id)

  data_range_min <- data %>%
    select(id_str, contains(var_str)) %>%
    select(id_str, contains(session_str)) %>%
    # mutate(!!id_str := !!rlang::sym(id_str) - max_id) %>%
    filter_all(., any_vars(. < range_min))


  data_range_max <- data %>%
    select(id_str, contains(var_str)) %>%
    select(id_str, contains(session_str)) %>%
    mutate(!!id_str := !!rlang::sym(id_str) - max_id) %>%
    filter_all(., any_vars(. > range_to)) %>%
    mutate(!!id_str := !!rlang::sym(id_str) + max_id)

  data_range_from %>% dplyr::full_join(range_max)

  }
