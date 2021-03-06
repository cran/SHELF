% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linearPoolDensity.R
\name{linearPoolDensity}
\alias{linearPoolDensity}
\title{Obtain points on the density function of a linear pool}
\usage{
linearPoolDensity(fit, xl = -Inf, xu = Inf, d = "best", lpw = 1, nx = 200)
}
\arguments{
\item{fit}{An object of class \code{elicitation}.}

\item{xl}{The lower limit in the sequence of parameter values. The default is the 0.001 quantile
of the fitted distribution (or the 0.001 quantile of a fitted normal
distribution, if a histogram fit is chosen).}

\item{xu}{The upper limit in the sequence of parameter values. The default is the 0.999 quantile
of the fitted distribution (or the 0.999 quantile of a fitted normal
distribution, if a histogram fit is chosen).}

\item{d}{The distribution fitted to each expert's probabilities. Options are
\code{"normal"}, \code{"t"}, \code{"gamma"}, \code{"lognormal"},
\code{"logt"},\code{"beta"}, \code{"hist"} (for a histogram fit), and
\code{"best"} (for best fitting)}

\item{lpw}{A vector of weights to be used in linear pool, if unequal
weighting is desired.}

\item{nx}{The number of points in the sequence from \code{xl} to \code{xu}.}
}
\value{
A list, with elements
\item{x}{a sequence of values for the uncertain parameter}
\item{fx}{the density function of the linear pool, evaluated at each element in \code{x}.}
}
\description{
Takes an object of class \code{elicitation}, evaluates a (weighted) linear pool, 
and returns points on the density function at a sequence of values of the elicited 
parameter
}
\examples{

\dontrun{
# Two experts
# Expert 1 states P(X<30)=0.25, P(X<40)=0.5, P(X<50)=0.75
# Expert 2 states P(X<20)=0.25, P(X<25)=0.5, P(X<35)=0.75
# Both experts state 0<X<100. 

v <- matrix(c(30, 40, 50, 20, 25, 35), 3, 2)
p <- c(0.25, 0.5, 0.75)
myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
linearPoolDensity(myfit)
}
}
\author{
Jeremy Oakley <j.oakley@sheffield.ac.uk>
}
