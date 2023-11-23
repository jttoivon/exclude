library(glue)

init_excluder <- function(data, e_name, statistics = function(data) { list(count = nrow(data)) }) {
  if (is.null(.GlobalEnv[[".Excluder"]]))
    .GlobalEnv[[".Excluder"]] <- list()
  
  .GlobalEnv[[".Excluder"]][[".current_e_name"]] <- e_name  # Set the name of the current excluder
  stats <- statistics(data)
  .GlobalEnv[[".Excluder"]][[e_name]] <- list(old=stats, .df=as_tibble_row(stats) %>% mutate(name="Original data", .before=1), .log=c(), .statistics=statistics)
  invisible(data)
}

excluder <- function(data, 
                     fmt="Old count was {old$count}, new count {new$count}, difference {old$count - new$count}",
                     e_name=NULL) {
  force(data)   # This is needed to prevent lazy evaluation, which breaks the side-effect
              # of setting a global variable.
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Excluder"]]$.current_e_name
  df <- .GlobalEnv[[".Excluder"]][[e_name]]$.df
  log <- .GlobalEnv[[".Excluder"]][[e_name]]$.log
  statistics <- .GlobalEnv[[".Excluder"]][[e_name]]$.statistics

  stats <- statistics(data)   
  .GlobalEnv[[".Excluder"]][[e_name]]$new <- stats
  old_stats <- .GlobalEnv[[".Excluder"]][[e_name]]$old
  
  #remain_names <- names(stats)
  remain <- imap_chr(stats, function(value, key) glue("{key}={value}")) %>% paste(collapse=" ")
  diff <- map_chr(names(stats), function(key) glue("{key}={old_stats[[key]]-stats[[key]]}")) %>% paste(collapse=" ")
  msg <- glue("{fmt}: excluded {diff}, remaining {remain}")
  #msg <- glue(fmt, .envir = .GlobalEnv[[".Excluder"]][[e_name]])  
  message(msg)
  
  df <- df %>% add_row(as_tibble_row(stats) %>% mutate(name=fmt))
  
  log <- append(log, msg)
  
  .GlobalEnv[[".Excluder"]][[e_name]]$old <- .GlobalEnv[[".Excluder"]][[e_name]]$new

  .GlobalEnv[[".Excluder"]][[e_name]]$.df  <- df
  .GlobalEnv[[".Excluder"]][[e_name]]$.log <- log
  
  data
}

dump_excluder <- function(filename, e_name=NULL) {
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Excluder"]]$.current_e_name
  writeLines(.GlobalEnv[[".Excluder"]][[e_name]]$.log, filename)
}

get_tibble <- function(e_name=NULL) {
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Excluder"]]$.current_e_name
  .GlobalEnv[[".Excluder"]][[e_name]]$.df %>% 
    mutate(across(-name, function (x) lag(x) - x, .names="diff_{.col}"))
}

plot_flow <- function(df) {
  orig <- df %>% pluck("name", 1)   # Save the name of the original dataset
  remain_cols <- df %>% select(!(name | starts_with("diff_"))) %>% colnames()
  diff_cols <- df %>% select(starts_with("diff_")) %>% colnames()
  
  df <- df %>% mutate(across(all_of(diff_cols) | name, lead))
  
  df <- df %>% 
    mutate(across(all_of(remain_cols), function(x) sprintf("%s=%i", cur_column(), x))) %>%
    unite(remain, all_of(remain_cols), sep="\n")
  df <- df %>% 
    mutate(across(all_of(diff_cols), function(x) sprintf("%s=%i", str_remove(cur_column(), "^diff_"), x))) %>%
    unite(diff, all_of(diff_cols), sep="\n")
  
  print(df)
  
  # Remain nodes
  df[[1, 'remain']] <- paste(orig, df[[1, 'remain']], sep="\n")
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
               {remain_nodes}
               {diff_nodes}
               {vertical_arrows}
               {horizontal_arrows}
               {layout}
  }}')
  flow
}
