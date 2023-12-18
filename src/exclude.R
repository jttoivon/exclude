library(glue)

new_exclude <- function(stats, df, log, statistics) {
  #stopifnot(is.character(prefix))
  structure(list(old=stats, .df=df, .log=log, .statistics=statistics), class = "exclude")
}

init_exclude <- function(data, e_name, 
                          statistics = function(data) { list(count = nrow(data)) },
                          name = "Original data") {
  if (is.null(.GlobalEnv[[".Exclude"]]))
    .GlobalEnv[[".Exclude"]] <- list()   # This will contain all Exclude objects
  
  .GlobalEnv[[".Exclude"]][[".current_e_name"]] <- e_name  # Set the name of the current exclude object
  stats <- statistics(data)

  pretty <- function(x) format(x, big.mark=",", trim=TRUE)
  
  remain <- imap_chr(stats, function(value, key) glue("{key}={pretty(value)}")) %>% paste(collapse=" ")
  diff <- map_chr(names(stats), function(key) glue("{key}=0")) %>% paste(collapse=" ")
  msg <- glue("{name}: excluded {diff}, remaining {remain}")
  
  df <- as_tibble_row(stats) %>% mutate(name=name, .before=1)
  .GlobalEnv[[".Exclude"]][[e_name]] <- new_exclude(stats, df, msg, statistics)
  invisible(data)
}

exclude <- function(data, 
                     fmt="Old count was {old$count}, new count {new$count}, difference {old$count - new$count}",
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
  remain <- imap_chr(stats, function(value, key) glue("{key}={pretty(value)}")) %>% paste(collapse=" ")
  diff <- map_chr(names(stats), function(key) glue("{key}={pretty(old_stats[[key]]-stats[[key]])}")) %>% paste(collapse=" ")
  msg <- glue("{fmt}: excluded {diff}, remaining {remain}")
  #msg <- glue(fmt, .envir = .GlobalEnv[[".Exclude"]][[e_name]])  
  message(msg)
  
  df <- df %>% add_row(as_tibble_row(stats) %>% mutate(name=fmt))
  
  log <- append(log, msg)
  
  .GlobalEnv[[".Exclude"]][[e_name]]$old <- .GlobalEnv[[".Exclude"]][[e_name]]$new

  .GlobalEnv[[".Exclude"]][[e_name]]$.df  <- df
  .GlobalEnv[[".Exclude"]][[e_name]]$.log <- log
  
  data
}

get_exclude <- function(e_name=NULL) {
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Exclude"]]$.current_e_name
  .GlobalEnv[[".Exclude"]][[e_name]]
}

print.exclude <- function(object) {
  cat(object$.log, sep="\n")
}

dump_exclude <- function(filename, e_name=NULL) {
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Exclude"]]$.current_e_name
  writeLines(.GlobalEnv[[".Exclude"]][[e_name]]$.log, filename)
}

as_tibble.exclude <- function(object) {
  object$.df %>% 
    mutate(across(-name, function (x) lag(x) - x, .names="diff_{.col}"))  
}

get_tibble <- function(e_name=NULL) {
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Exclude"]]$.current_e_name
  .GlobalEnv[[".Exclude"]][[e_name]]$.df %>% 
    mutate(across(-name, function (x) lag(x) - x, .names="diff_{.col}"))
}

plot_flow <- function(df) {
  orig <- df %>% pluck("name", 1)   # Save the name of the original dataset
  remain_cols <- df %>% select(!(name | starts_with("diff_"))) %>% colnames()
  diff_cols <- df %>% select(starts_with("diff_")) %>% colnames()
  
  pretty <- function(x) format(x, big.mark=",", trim=TRUE)
  
  df <- df %>% mutate(across(all_of(diff_cols) | name, lead)) # Use thousand separators when printing a number
  
  # Remain nodes
  df <- df %>% 
    mutate(across(all_of(remain_cols), function(x) sprintf("%s=%s", cur_column(), pretty(x)))) %>%
    unite(remain, all_of(remain_cols), sep="\\l")
  # Diff nodes
  df <- df %>% 
    mutate(across(all_of(diff_cols), function(x) sprintf("%s=%s", str_remove(cur_column(), "^diff_"), pretty(x)))) %>%
    unite(diff, all_of(diff_cols), sep="\\l")
  
  #print(df)
  
  # Remain nodes
  df[[1, 'remain']] <- paste(orig, df[[1, 'remain']], sep="\\l")
  remain_nodes <- map_chr(1:nrow(df), function(i) glue("P{i} [label = \"{df[[i, 'remain']]}\"]")) %>% paste(collapse="\n")

  # Diff nodes
  diff_nodes <- map_chr(1:(nrow(df)-1), function(i) glue("M{i} [label = \"{df[[i, 'diff']]}\"]")) %>% paste(collapse="\n")
  
  # Vertical arrows
  vertical_arrows <- map_chr(1:(nrow(df)-1), function(i) glue("P{i} -> P{i+1}")) %>% paste(collapse="\n")

  # Horizontal arrows
  horizontal_arrows <- map_chr(1:(nrow(df)-1), function(i) glue("P{i} -> M{i}[label=\"{df[[i, 'name']]}\"]")) %>% paste(collapse="\n")
  
  # Force the layout of graph. The remain node and the corresponding diff node should be at the same height.
  layout <- map_chr(1:(nrow(df)-1), function(i) glue("{{ rank=same; P{i}; M{i}}}")) %>% paste(collapse="\n")
  
  flow <- glue('digraph graphname {{
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
  as_tibble(object) %>% plot_flow() %>% grViz()
}

