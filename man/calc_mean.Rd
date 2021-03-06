% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc-mean.R
\name{calc_mean}
\alias{calc_mean}
\title{Calculate mean of variables}
\usage{
calc_mean(data, id_str, var_str, session_str, n_min, item_scores = FALSE,
  short_var_name = TRUE, timepoint_str = "s", sep = "_")
}
\arguments{
\item{data}{Wide dataframe.}

\item{id_str}{String of identifier variable.}

\item{var_str}{String of variable to calculate mean for.}

\item{session_str}{String of session number.}

\item{n_min}{Minimum number of available scores to calculate mean.}

\item{item_scores}{Add item scores after mean.}

\item{sep}{seperator for variable names.}

\item{sort_mean_item}{Logical, if TRUE and multiple sessions then output dataframe will be organised mean_timepoint followed by all items for that timepoint, if FALSE all means will come after id variable followed by all items.}
}
\description{
Calculate mean addressing item-level missing data using proration
}
