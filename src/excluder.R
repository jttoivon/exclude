library(glue)

init_excluder <- function(df, e_name) {
  #excluder_count <<- nrow(df)
  if (is.null(.GlobalEnv[[".Excluder"]]))
    .GlobalEnv[[".Excluder"]] <- list()
  
  .GlobalEnv[[".Excluder"]][[".current_e_name"]] <- e_name  # Name of the current excluder
  .GlobalEnv[[".Excluder"]][[e_name]] <- list(count = nrow(df), log=c())
  #assign("excluder_count", nrow(df), envir = .GlobalEnv)
  #assign("excluder_log", c(), envir = .GlobalEnv)
  invisible(df)
}

excluder <- function(df, 
                     fmt="Old count was {old_count}, new count {new_count}, difference {old_count - new_count}",
                     e_name=NULL) {
  force(df)   # This is needed to prevent lazy evaluation, which breaks the side-effect
  # of setting a global variable.
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Excluder"]]$.current_e_name
  #old_count <- excluder_count
  #old_count <- get("excluder_count", envir = .GlobalEnv)
  old_count <- .GlobalEnv[[".Excluder"]][[e_name]]$count
  old_log <- .GlobalEnv[[".Excluder"]][[e_name]]$log
  new_count <- nrow(df)
  diff <- old_count - new_count
  #cat(sprintf("Old count was %s new count %s difference %s\n", old_count, new_count, old_count - new_count))
  #fmt = "Old count was {old_count} new count {new_count} difference {old_count - new_count}"
  cat(glue(fmt), "\n")
  new_log <- append(old_log, glue(fmt))
  #excluder_count <<- new_count
  #assign("excluder_count", nrow(df), envir = .GlobalEnv)
  #assign("excluder_log", new_log, envir = .GlobalEnv)
  .GlobalEnv[[".Excluder"]][[e_name]]$count <- nrow(df)
  .GlobalEnv[[".Excluder"]][[e_name]]$log <- new_log
  
  df
}

dump_excluder <- function(filename, e_name=NULL) {
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Excluder"]]$.current_e_name
  writeLines(.GlobalEnv[[".Excluder"]][[e_name]]$log, filename)
}
