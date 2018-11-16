#' Rename item variables
#' @description Rename item variables consistently, this works only for variables named InMeasure1 which stands for Item 1 at Intake Assessment for Measure, or Session1Measure5 wich stands for Item 5 at Session 1 of Measure
#' @param data Dataset
#' @param id_str String, id variable
#' @param session Numeric or String identifying session/timepoint
#' @param data_str_measure Sting, unique string identifying measure
#' @param prefix_data_str_session String pattern in data that identifies session/timepoint
#' @param new_str_measure String, if unhappy with name of measure in data, you can change it here
#' @param prefix_new_str_session String, idicator for session/timepoint, default "s"
#' @param prefix_new_str_item String, idicator for item, default "i"
#'
#' @export
#'
rename_item_vars <- function(data, id_str, data_session, new_session = data_session, data_str_measure, prefix_data_str_session, new_str_measure = data_str_measure, prefix_new_str_session = "s", prefix_new_str_item = "i"){

  # This part is for all numeric sessions
  if (base::is.numeric(data_session) == TRUE) {

    string_search <- base::paste(prefix_data_str_session, data_session, data_str_measure, "\\d", sep = "")

    data_select <- dplyr::select(data, id_str, matches(string_search))

    original_varibale_names <- base::names(dplyr::select(data, matches(string_search)))

    string_item_number_search <- base::paste("(?<=", data_str_measure, ")\\d+", sep = "")

    item_number <- stringr::str_extract(original_varibale_names, string_item_number_search)

    session_number <- stringr::str_extract(original_varibale_names, paste("(?<=", prefix_data_str_session, ")\\d+", sep = ""))

    new_variables_names <- paste(stringr::str_to_lower(new_str_measure), "_", prefix_new_str_session, session_number, "_", prefix_new_str_item, item_number, sep = "")

    id_new_variables_names <- c(id_str, new_variables_names)

    names(data_select) <- id_new_variables_names

    return(data_select)

    # This part is for all numeric sessions
    } else if (base::is.character(data_session) == TRUE) {

    string_search <- paste(data_session, data_str_measure, "\\d", sep = "")

    data_select <- dplyr::select(data, id_str, matches(string_search))

    original_varibale_names <- names(dplyr::select(data, matches(string_search)))

    string_item_number_search <- paste("(?<=", data_str_measure, ")\\d+", sep = "")

    item_number <- stringr::str_extract(original_varibale_names, string_item_number_search)

    session_number <- stringr::str_to_lower(new_session)

    new_variables_names <- paste(stringr::str_to_lower(new_str_measure), "_", session_number, "_" , prefix_new_str_item, item_number, sep = "")

    id_new_variables_names <- c(id_str, new_variables_names)

    names(data_select) <- id_new_variables_names

    return(data_select)

    } else {
      stop("BOOOOM ERROR \n something wrong with data_session input.")
  }
}
