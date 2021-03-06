% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elicit.R
\name{elicit}
\alias{elicit}
\alias{roulette}
\title{Elicit judgements and fit distributions interactively}
\usage{
elicit()
}
\value{
An object of class \code{elicitation}, which is returned once the 
Quit button has been clicked. See \code{\link{fitdist}} for details.
}
\description{
Opens up a web browser (using the shiny package), from which you can specify
judgements, fit distributions and plot the fitted density functions with
additional feedback. Probabilities can be specified directly, or the roulette 
elicitation method can be used.
}
\details{
Click on the "Help" tab for instructions. Click the "Quit" button to exit the app and return
the results from the \code{fitdist} command. Click "Download report" to generate a report
of all the fitted distributions.
}
\examples{

\dontrun{

elicit()

}
}
\author{
Jeremy Oakley <j.oakley@sheffield.ac.uk>
}
