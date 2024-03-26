# TODO
# * DONE Add option whether to emit messages or not
# * DONE Change parameter name fmt to something more sensible
# * Add examples to functions
# * DONE Can exclude be made to work for vectors in addition to dataframe?
# * Use some data from base instead of ggplot2::mpg
# * Maybe add an option that disables exclude completely

#' A constructor for exclude class.
#'
#' @param stats A named list of statistics values.
#' @param df A dataframe of exclusions
#' @param log A character vector of log messages
#' @param statistics A named list of statistic functions.
#'
#' @return An S3 object of class exclude
#'
#' @examples
new_exclude <- function(stats, df, log, statistics) {
  #stopifnot(is.character(prefix))
  structure(list(old=stats, .df=df, .log=log, .statistics=statistics), class = "exclude") # Create S3 object
}

#' Initialize an exclude object
#'
#' @param data A dataframe on which exclusions will be performed.
#' @param e_name Name of the exclude object.
#' @param statistics A named list of statistic functions.
#' @param what Name for the initial dataset.
#'
#' @return Returns the dataframe invisibly.
#' @seealso [exclude()] to log exclusions
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
init_exclude <- function(data, e_name = "default",
                          statistics = function(data) { list(count = nrow(data)) },
                          what = "Original data") {

  .GlobalEnv[[".Exclude"]][[".current_e_name"]] <- e_name  # Set the name of the current exclude object
  stats <- statistics(data)

  pretty <- function(x) format(x, big.mark=",", trim=TRUE)

  remain <- purrr::imap_chr(stats, function(value, key) glue::glue("{key}={pretty(value)}")) %>% paste(collapse=" ")
  diff <- purrr::map_chr(names(stats), function(key) glue::glue("{key}=0")) %>% paste(collapse=" ")
  msg <- glue::glue("{what}: excluded {diff}, remaining {remain}")

  df <- tibble::as_tibble_row(stats) %>% dplyr::mutate(name=what, .before=1)
  .GlobalEnv[[".Exclude"]][[e_name]] <- new_exclude(stats, df, msg, statistics)
  invisible(data)
}

#' Record the exclusions performed on the dataframe.
#'
#' @param data Dataframe
#' @param what Exclusion message.
#' @param e_name Name of the exclude object.
#'
#' @return Returns the dataframe invisibly.
#' @seealso [init_exclude()] to start tracking exclusions
#' @export
#'
#' @examples
exclude <- function(data,
                     what="Exclusion",
                     e_name=NULL) {
  force(data)   # This is needed to prevent lazy evaluation, which breaks the side-effect
              # of setting a global variable.
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Exclude"]]$.current_e_name
  df <- .GlobalEnv[[".Exclude"]][[e_name]]$.df
  log <- .GlobalEnv[[".Exclude"]][[e_name]]$.log
  statistics <- .GlobalEnv[[".Exclude"]][[e_name]]$.statistics

  stats <- statistics(data)
  .GlobalEnv[[".Exclude"]][[e_name]]$new <- stats
  old_stats <- .GlobalEnv[[".Exclude"]][[e_name]]$old

  pretty <- function(x) format(x, big.mark=",", trim=TRUE)

  #remain_names <- names(stats)
  remain <- purrr::imap_chr(stats, function(value, key) glue::glue("{key}={pretty(value)}")) %>% paste(collapse=" ")
  diff <- purrr::map_chr(names(stats), function(key) glue::glue("{key}={pretty(old_stats[[key]]-stats[[key]])}")) %>% paste(collapse=" ")
  msg <- glue::glue("{what}: excluded {diff}, remaining {remain}")
  #msg <- glue(msg, .envir = .GlobalEnv[[".Exclude"]][[e_name]])
  if (getOption("exclude.print_messages")) message(msg)

  df <- df %>% tibble::add_row(tibble::as_tibble_row(stats) %>% dplyr::mutate(name=what))

  log <- append(log, msg)

  .GlobalEnv[[".Exclude"]][[e_name]]$old <- .GlobalEnv[[".Exclude"]][[e_name]]$new

  .GlobalEnv[[".Exclude"]][[e_name]]$.df  <- df
  .GlobalEnv[[".Exclude"]][[e_name]]$.log <- log

  data
}

#' Get the exclude object with name e_name
#'
#' @param e_name Name of the wanted exclude object.
#'
#' @return An exclude object.
#' @export
#'
#' @examples
#' library(magrittr)
#' old <- exclude:::pop("default") # Only needed in the help page. Keeps environment clean.
#' mtcars %>% 
#'   init_exclude() %>%
#'   dplyr::filter(gear != 3) %>%
#'   exclude()
#' get_exclude()
#' invisible(exclude:::push(old))  # Only needed in the help page. Restore environment.
get_exclude <- function(e_name=NULL) {
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Exclude"]]$.current_e_name
  .GlobalEnv[[".Exclude"]][[e_name]]
}

#' Print the exclude object
#'
#' @param x Object of class exclude.
#' @param ... Just to satisfy the requirement of the generic function.
#'
#' @return None
#' @export
#'
#' @examples
print.exclude <- function(x, ...) {
  cat(x$.log, sep="\n")
}

dump_exclude <- function(filename, e_name=NULL) {
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Exclude"]]$.current_e_name
  writeLines(.GlobalEnv[[".Exclude"]][[e_name]]$.log, filename)
}

#' Convert an exclude object to tibble
#'
#' @param x An object of class exclude
#' @param ... Dummy
#'
#' @return A Tibble.
#' @export
#' @importFrom tibble as_tibble
#'
#' @examples
as_tibble.exclude <- function(x, ...) {
  # For some reason I need to use dplyr::lag instead of stats::lag
  x$.df %>%
    dplyr::mutate(dplyr::across(-"name", function (c) {dplyr::lag(c) - c}, .names="diff_{.col}"))
}

#' Convert an exclude object to a data frame
#'
#' @param x An object of class exclude
#' @param ... Dummy
#'
#' @return A data.frame.
#' @export
#' @importFrom tibble as_tibble
#'
#' @examples
as.data.frame.exclude <- function(x, ...) {
  base::as.data.frame(tibble::as_tibble(x))
}



#' Visualize the exclusion flow as a string in the dot language
#'
#' @description
#' The dot string can then be saved to a file with `cat(plot_flow(as_tibble(e)), file="example.dot")`
#' call, and the converted to png format using the following command:
#' `dot example.dot -T png -o example.png`
#'
#' @param df A data frame in form returned by as_tibble or as.data.frame
#'
#' @return A character vector describing are graph in the dot language
#' @export
#'
#' @examples
plot_flow <- function(df) {
  orig <- df %>% purrr::pluck("name", 1)   # Save the name of the original dataset
  remain_cols <- df %>% dplyr::select(!("name" | tidyselect::starts_with("diff_"))) %>% colnames()
  diff_cols <- df %>% dplyr::select(tidyselect::starts_with("diff_")) %>% colnames()

  pretty <- function(x) format(x, big.mark=",", trim=TRUE)

  df <- df %>% dplyr::mutate(dplyr::across(tidyselect::all_of(diff_cols) | "name", dplyr::lead)) # Use thousand separators when printing a number

  # Remain nodes
  df <- df %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(remain_cols), function(x) sprintf("%s=%s", dplyr::cur_column(), pretty(x)))) %>%
    tidyr::unite("remain", tidyselect::all_of(remain_cols), sep="\\l")
  # Diff nodes
  df <- df %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(diff_cols), function(x) sprintf("%s=%s", stringr::str_remove(dplyr::cur_column(), "^diff_"), pretty(x)))) %>%
    tidyr::unite(diff, tidyselect::all_of(diff_cols), sep="\\l")

  #print(df)

  # Remain nodes
  df[[1, 'remain']] <- paste(orig, df[[1, 'remain']], sep="\\l")
  remain_nodes <- purrr::map_chr(1:nrow(df), function(i) glue::glue("P{i} [label = \"{df[[i, 'remain']]}\"]")) %>% paste(collapse="\n")

  # Diff nodes
  diff_nodes <- purrr::map_chr(1:(nrow(df)-1), function(i) glue::glue("M{i} [label = \"{df[[i, 'diff']]}\"]")) %>% paste(collapse="\n")

  # Vertical arrows
  vertical_arrows <- purrr::map_chr(1:(nrow(df)-1), function(i) glue::glue("P{i} -> P{i+1}")) %>% paste(collapse="\n")

  # Horizontal arrows
  horizontal_arrows <- purrr::map_chr(1:(nrow(df)-1), function(i) glue::glue("P{i} -> M{i}[label=\"{df[[i, 'name']]}\"]")) %>% paste(collapse="\n")

  # Force the layout of graph. The remain node and the corresponding diff node should be at the same height.
  layout <- purrr::map_chr(1:(nrow(df)-1), function(i) glue::glue("{{ rank=same; P{i}; M{i}}}")) %>% paste(collapse="\n")

  flow <- glue::glue('digraph graphname {{
  node [shape="box"];
  labeljust = l
               {remain_nodes}
               {diff_nodes}
               {vertical_arrows}
               {horizontal_arrows}
               {layout}
  }}')
  flow
}

#' Plot the exclusion diagram
#'
#' @param x An exclude object
#' @param ... Just to satisfy the requirement of the generic function.
#'
#' @return An object of class htmlwidget
#' @export
#'
#' @examples
#' library(magrittr)
#' old <- exclude:::pop("default") # Only needed in the help page. Keeps environment clean.
#' mtcars %>% 
#' init_exclude() %>%
#'   dplyr::filter(gear != 3) %>%
#'   exclude()
#' e <- get_exclude()
#' plot(e)
#' invisible(exclude:::push(old))  # Only needed in the help page. Restore environment.
plot.exclude <- function(x, ...) {
  tibble::as_tibble(x) %>% plot_flow() %>% DiagrammeR::grViz()
}

# These internal functions are mainly used as helpers in testing

ls <- function() {
  setdiff(names(.GlobalEnv[[".Exclude"]]), ".current_e_name")
}

pop <- function(e_name=NULL) {
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Exclude"]]$.current_e_name
  e <- .GlobalEnv[[".Exclude"]][[e_name]]
  #rm(ename, envir=.GlobalEnv[[".Exclude"]])
  .GlobalEnv[[".Exclude"]][[e_name]] <- NULL
  e
}

push <- function(e, e_name=NULL) {
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Exclude"]]$.current_e_name
  old <- .GlobalEnv[[".Exclude"]][[e_name]]
  .GlobalEnv[[".Exclude"]][[e_name]] <- e
  old
}

