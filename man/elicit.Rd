\name{elicit}
\alias{elicit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Elicit judgements and fit distributions interactively 
}
\description{
Opens up a web browser (using the shiny package), from which you can specify judgements, fit distributions and plot the fitted density functions with additional feedback. 
}
\usage{
elicit()
}
%- maybe also 'usage' for other objects documented here.


\details{Parameter limits determine which distributions can be fitted. Non-negative lower limits are needed for the gamma, lognormal and log-t distributions, and both limits must be finite for to fit a beta distribution. If a histogram is fitted without specifying finite limits, endpoints are chosen based on fitting a normal distribution.

As an example, if the elicited judgements are P(X<15)=0.25, P(X<20)=0.5) and P(X<40)=0.75, specify the parameter values as 15,20,40 and the cumulative probabilities as 0.25,0.5,0.75.

Press Esc in the R console window to exit the elicitation session.}  




\author{
Jeremy Oakley <j.oakley@sheffield.ac.uk>
}

\seealso{
\code{\link{feedback}}, \code{\link{fitdist}}, \code{\link{plinearpool}}, \code{\link{plotfit}}, \code{\link{qlinearpool}}, \code{\link{roulette}} 
}

\examples{
\dontrun{

elicit()

}
}
