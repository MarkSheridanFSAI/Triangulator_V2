\name{run_app}
\alias{run_app}
\title{Start the Triangulator app}
\usage{
run_app(launch.browser = TRUE, ...)
}
\arguments{
\item{launch.browser}{Open the app in your web browser (default \code{TRUE}).}

\item{...}{Other settings passed to \code{\link[shiny]{runApp}}, such as \code{port}.}
}
\value{
Nothing; the app runs until you close it (press Esc in the R console, or the red
Stop button in RStudio or Positron).
}
\description{
Opens Triangulator in your web browser. Upload a claims listing or bordereaux
(or try the built-in sample data) to get claims diagnostics, triangles and
first-look reserve estimates.
}
\examples{
if (interactive()) {
  run_app()
}
}
