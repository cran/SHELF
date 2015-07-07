\name{roulette}
\alias{roulette}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Elicit one set of probabilities using the roulette method.
}
\description{
Produces a graphics window with the roulette grid. The user clicks in the window to allocate 'chips' to 'bins'. The elicited probability inside each bin is the proportion of chips in each bin. 
}
\usage{
roulette(lower=0, upper=100, gridheight=10, nbins=10, round.end = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lower}{
The lower limit on the x-axis of the roulette grid.
}
  \item{upper}{
The upper limit on the x-axis of the roulette grid.
}
  \item{gridheight}{
The maximum number of chips that can be allocated to a single bin.
}
  \item{nbins}{
The number of equally sized bins drawn between \code{Lo} and \code{Up}.
}
  \item{round.end}{
If set to \code{T}, empty bins and the uppermost non-empty bin will be ignored. For example, with 20 chips in total, if the uppermost non-empty bin is [70,80] and contains 1 chip, setting \code{round.end = F} will result in an elicited probability P(X>80)=0, but setting \code{round.end = T} will remove this judgement, instead only having P(X>70)=0.05.  
}
}

\value{
  A list, with outputs \code{$v} corresponding to the upper limits of each bin, and \code{$p} corresponding to cumulative probabilities for each upper bin limit.
}

\author{
Jeremy Oakley <j.oakley@sheffield.ac.uk>
}

\seealso{
\code{\link{elicit}}, \code{\link{feedback}}, \code{\link{fitdist}}, \code{\link{plinearpool}}, \code{\link{plotfit}}, \code{\link{qlinearpool}} 
}

\examples{
\dontrun{
x<-roulette()
# Then allocate chips to bins

# To fit distributions and see the results
myfit <- fitdist(vals = x$v, probs = x$p)
plotfit(myfit)
}
}
