# Helper function that makes sure the exclusion object created in a function exists only locally
local_exclude <- function(e_name, envir = parent.frame()) {
  old <- push(NULL, "test")   # Store the original object (if it exists)
  withr::defer(push(old, "test"), envir = envir)   # Restore the original object at the end of the function
}
