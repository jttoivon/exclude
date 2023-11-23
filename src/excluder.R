library(glue)

init_excluder <- function(df, e_name, statistics = function(df) { list(count = nrow(df)) }) {
  if (is.null(.GlobalEnv[[".Excluder"]]))
    .GlobalEnv[[".Excluder"]] <- list()
  
  .GlobalEnv[[".Excluder"]][[".current_e_name"]] <- e_name  # Set the name of the current excluder
  .GlobalEnv[[".Excluder"]][[e_name]] <- list(old=statistics(df), .log=c(), .statistics=statistics)
  invisible(df)
}

excluder <- function(df, 
                     fmt="Old count was {old$count}, new count {new$count}, difference {old$count - new$count}",
                     e_name=NULL) {
  force(df)   # This is needed to prevent lazy evaluation, which breaks the side-effect
              # of setting a global variable.
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Excluder"]]$.current_e_name
  log <- .GlobalEnv[[".Excluder"]][[e_name]]$.log
  statistics <- .GlobalEnv[[".Excluder"]][[e_name]]$.statistics
  
  .GlobalEnv[[".Excluder"]][[e_name]]$new <- statistics(df)

  msg <- glue(fmt, .envir = .GlobalEnv[[".Excluder"]][[e_name]])  
  cat(msg, "\n")
  
  log <- append(log, msg)
  .GlobalEnv[[".Excluder"]][[e_name]]$old <- .GlobalEnv[[".Excluder"]][[e_name]]$new
  .GlobalEnv[[".Excluder"]][[e_name]]$.log <- log
  
  df
}

dump_excluder <- function(filename, e_name=NULL) {
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Excluder"]]$.current_e_name
  writeLines(.GlobalEnv[[".Excluder"]][[e_name]]$.log, filename)
}
