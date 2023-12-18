#library(glue)

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
#' @param name Name for the initial dataset.
#'
#' @return Returns the dataframe invisibly.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
init_exclude <- function(data, e_name,
                          statistics = function(data) { list(count = nrow(data)) },
                          name = "Original data") {
  if (is.null(.GlobalEnv[[".Exclude"]]))
    .GlobalEnv[[".Exclude"]] <- list()   # This will contain all Exclude objects

  .GlobalEnv[[".Exclude"]][[".current_e_name"]] <- e_name  # Set the name of the current exclude object
  stats <- statistics(data)

  pretty <- function(x) format(x, big.mark=",", trim=TRUE)

  remain <- purrr::imap_chr(stats, function(value, key) glue::glue("{key}={pretty(value)}")) %>% paste(collapse=" ")
  diff <- purrr::map_chr(names(stats), function(key) glue::glue("{key}=0")) %>% paste(collapse=" ")
  msg <- glue::glue("{name}: excluded {diff}, remaining {remain}")

  df <- tibble::as_tibble_row(stats) %>% dplyr::mutate(name=name, .before=1)
  .GlobalEnv[[".Exclude"]][[e_name]] <- new_exclude(stats, df, msg, statistics)
  invisible(data)
}

#' Record the exclusions performed on the dataframe.
#'
#' @param data Dataframe
#' @param fmt Exclusion message.
#' @param e_name Name of the exclude object.
#'
#' @return Returns the dataframe invisibly.
#' @export
#'
#' @examples
exclude <- function(data,
                     fmt="Exclusion",
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
  msg <- glue::glue("{fmt}: excluded {diff}, remaining {remain}")
  #msg <- glue(fmt, .envir = .GlobalEnv[[".Exclude"]][[e_name]])
  message(msg)

  df <- df %>% tibble::add_row(tibble::as_tibble_row(stats) %>% dplyr::mutate(name=fmt))

  log <- append(log, msg)

  .GlobalEnv[[".Exclude"]][[e_name]]$old <- .GlobalEnv[[".Exclude"]][[e_name]]$new

  .GlobalEnv[[".Exclude"]][[e_name]]$.df  <- df
  .GlobalEnv[[".Exclude"]][[e_name]]$.log <- log

  data
}

#' Title
#'
#' @param e_name
#'
#' @return
#' @export
#'
#' @examples
get_exclude <- function(e_name=NULL) {
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Exclude"]]$.current_e_name
  .GlobalEnv[[".Exclude"]][[e_name]]
}

#' Title
#'
#' @param x Object of class exclude
#'
#' @return
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

# as_tibble.exclude <- function(object, ...) {
#   object$.df %>%
#     dplyr::mutate(across(-name, function (x) lag(x) - x, .names="diff_{.col}"))
# }

# get_tibble <- function(e_name=NULL) {
#   if (is.null(e_name))
#     e_name <- .GlobalEnv[[".Exclude"]]$.current_e_name
#   .GlobalEnv[[".Exclude"]][[e_name]]$.df %>%
#     mutate(across(-name, function (x) lag(x) - x, .names="diff_{.col}"))
# }

plot_flow <- function(df) {
  orig <- df %>% dplyr::pluck("name", 1)   # Save the name of the original dataset
  remain_cols <- df %>% dplyr::select(!(name | tidyselect::starts_with("diff_"))) %>% colnames()
  diff_cols <- df %>% dplyr::select(tidyselect::starts_with("diff_")) %>% colnames()

  pretty <- function(x) format(x, big.mark=",", trim=TRUE)

  df <- df %>% dplyr::mutate(dplyr::across(tidyselect::all_of(diff_cols) | name, dplyr::lead)) # Use thousand separators when printing a number

  # Remain nodes
  df <- df %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(remain_cols), function(x) sprintf("%s=%s", dplyr::cur_column(), pretty(x)))) %>%
    dplyr::unite(remain, tidyselect::all_of(remain_cols), sep="\\l")
  # Diff nodes
  df <- df %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(diff_cols), function(x) sprintf("%s=%s", stringr::str_remove(cur_column(), "^diff_"), pretty(x)))) %>%
    dplyr::unite(diff, tidyselect::all_of(diff_cols), sep="\\l")

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

plot.exclude <- function(object) {
  tibble::as_tibble(object) %>% plot_flow() %>% DiagrammeR::grViz()
}

