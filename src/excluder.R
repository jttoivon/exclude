library(glue)

init_excluder <- function(df, e_name) {
  if (is.null(.GlobalEnv[[".Excluder"]]))
    .GlobalEnv[[".Excluder"]] <- list()
  
  .GlobalEnv[[".Excluder"]][[".current_e_name"]] <- e_name  # Name of the current excluder
  .GlobalEnv[[".Excluder"]][[e_name]] <- list(count = nrow(df), log=c())
  invisible(df)
}

excluder <- function(df, 
                     fmt="Old count was {old_count}, new count {new_count}, difference {old_count - new_count}",
                     e_name=NULL) {
  force(df)   # This is needed to prevent lazy evaluation, which breaks the side-effect
              # of setting a global variable.
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Excluder"]]$.current_e_name
  old_count <- .GlobalEnv[[".Excluder"]][[e_name]]$count
  old_log <- .GlobalEnv[[".Excluder"]][[e_name]]$log
  new_count <- nrow(df)
  diff <- old_count - new_count
  cat(glue(fmt), "\n")
  new_log <- append(old_log, glue(fmt))
  .GlobalEnv[[".Excluder"]][[e_name]]$count <- nrow(df)
  .GlobalEnv[[".Excluder"]][[e_name]]$log <- new_log
  
  df
}

dump_excluder <- function(filename, e_name=NULL) {
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Excluder"]]$.current_e_name
  writeLines(.GlobalEnv[[".Excluder"]][[e_name]]$log, filename)
}
