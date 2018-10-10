#' Rename item variables consistently
#'
#' @param data Wide dataset.
#' @param id_str
#' @param measure
#' @param session
#' @param new_str_measure
#'
#' @return
#' @export
#'
#' @examples
rename_item_vars <- function(data, id_str, measure, session, new_str_measure = measure){

  # this part is for all weekly measures that are numeric
  if (is.numeric(session) == TRUE) {

    string_search <- paste("Wk", session, measure, "\\d", sep = "")

    data_select <- dplyr::select(data, id_str, matches(string_search))

    original_varibale_names <- names(dplyr::select(data, matches(string_search)))

    string_item_number_search <- paste("(?<=", measure, ")\\d+", sep = "")

    item_number <- stringr::str_extract(original_varibale_names, string_item_number_search)

    session_number <- stringr::str_extract(original_varibale_names, "(?<=Wk)\\d+")

    new_variables_names <- paste(stringr::str_to_lower(new_str_measure), "_s", session_number, "_i", item_number, sep = "")

    id_new_variables_names <- c(id_str, new_variables_names)

    names(data_select) <- id_new_variables_names

    return(data_select)

    # this part is for intake (in) and rebaseline (rb) or any other weekly measure that has a string as input, so no real number
  } else {

    string_search <- paste(session, measure, "\\d", sep = "")

    data_select <- dplyr::select(data, id_str, matches(string_search))

    original_varibale_names <- names(dplyr::select(data, matches(string_search)))

    string_item_number_search <- paste("(?<=", measure, ")\\d+", sep = "")

    item_number <- stringr::str_extract(original_varibale_names, string_item_number_search)

    session_number <- stringr::str_to_lower(session)

    new_variables_names <- paste(stringr::str_to_lower(new_str_measure), "_", session_number, "_i", item_number, sep = "")

    id_new_variables_names <- c(id_str, new_variables_names)

    names(data_select) <- id_new_variables_names

    return(data_select)

  }
}
