# Initialize the package
.onLoad <- function(libname, pkgname) {
  # Create the exclude environment
  if (is.null(.GlobalEnv[[".Exclude"]]))
    .GlobalEnv[[".Exclude"]] <- list()   # This will contain all Exclude objects
  
  # Set the 'exclude.print_messages' option, if unset.
  op <- options()
  op.exclude <- list(
    exclude.print_messages = TRUE
  )
  toset <- !(names(op.exclude) %in% names(op))
  if (any(toset)) options(op.exclude[toset])
  
  invisible()
}