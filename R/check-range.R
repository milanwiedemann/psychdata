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
#' @export
#'
check_item_range <- function(data, id_str, var_str, session_str, range_min, range_max){

  # Do some sort of if else stuff in case id is character

  # Get max of id
  max_id <- max(data$id)

  data_range_min <- data %>%
    dplyr::select(id_str, contains(var_str)) %>%
    dplyr::select(id_str, contains(session_str)) %>%
    dplyr::filter_all(., any_vars(. < range_min)) %>%
    dplyr::mutate(!!id_str := as.numeric(!!rlang::sym(id_str)))

  data_range_max <- data %>%
    dplyr::select(id_str, contains(var_str)) %>%
    dplyr::select(id_str, contains(session_str)) %>%
    dplyr::mutate(!!id_str := !!rlang::sym(id_str) - max_id) %>%
    dplyr::filter_all(., any_vars(. > range_max)) %>%
    dplyr::mutate(!!id_str := !!rlang::sym(id_str) + max_id) %>%
    dplyr::mutate(!!id_str := as.numeric(!!rlang::sym(id_str)))

  if (base::nrow(data_range_min) == 0 && base::nrow(data_range_max) == 0) {

    base::message("Wooop woooooop, all variables are within the specified range :)\nI'm awarding one data entry point to whoever entered this data!")

  } else if (base::nrow(data_range_min) == 0) {

    base::message("Ahh ahh, at least one value is larger than the value specified in 'range_max'.\nGo back to the paper copies of the questionnaire and sort this out! NOW!")
    data_range_max

  }  else if (base::nrow(data_range_max) == 0) {

    base::message("Ahh ahh, at least one value is smaller than the value specified in 'range_min'.\nGo back to the paper copies of the questionnaire and sort this out! NOW!")
    data_range_min

  } else if (base::nrow(data_range_min) > 0 && base::nrow(data_range_max) > 0) {

    base::message("Uhhh uhhhh uhhh! This does NOT look good!\nSome values are smaller than the value specified in 'range_min', some values are larger than the value specified in 'range_max', ohh maaaaaaan!\nSeriously, this needs some attention, get a coffee and go back to the paper copies of the questionnaire and sort this out! NOW!")

    data_range_min_max <- base::rbind(data_range_min, data_range_max)

    unique(data_range_min_max)

  }

  }
