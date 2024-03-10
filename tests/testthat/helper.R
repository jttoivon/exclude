# Helper function that makes sure the exclusion object created in a function exists only locally
local_exclude <- function(e_name=NULL, envir = parent.frame()) {
  if (is.null(e_name))
    e_name <- .GlobalEnv[[".Exclude"]]$.current_e_name
  old <- push(NULL, e_name)   # Store the original object (if it exists)
  old_e_name <- .GlobalEnv[[".Exclude"]]$.current_e_name
  withr::defer(push(old, e_name), envir = envir)   # Restore the original object at the end of the function
  withr::defer(.GlobalEnv[[".Exclude"]]$.current_e_name <- old_e_name, envir=envir)
}
