% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exclude.R
\name{plot.exclude}
\alias{plot.exclude}
\title{Plot the exclusion diagram}
\usage{
\method{plot}{exclude}(x, ...)
}
\arguments{
\item{x}{An exclude object}

\item{...}{Just to satisfy the requirement of the generic function.}
}
\value{
An object of class htmlwidget
}
\description{
Plot the exclusion diagram
}
\examples{
library(magrittr)
old <- exclude:::pop("default") # Only needed in the help page. Keeps environment clean.
mtcars \%>\% 
init_exclude() \%>\%
  dplyr::filter(gear != 3) \%>\%
  exclude()
e <- get_exclude()
plot(e)
invisible(exclude:::push(old, "default"))  # Only needed in the help page. Restore environment.
}
