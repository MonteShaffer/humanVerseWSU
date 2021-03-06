% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-stats.R
\name{doMode}
\alias{doMode}
\title{doMode}
\usage{
doMode(x)
}
\arguments{
\item{x}{numeric vector}
}
\value{
numeric vector that contains _all_ values that are modal (could be bimodal)
}
\description{
Returns \code{mode} of a numeric vector x
}
\details{
\code{mode} is the most frequent value(s) in a set of data
}
\examples{
doMode( c(1:9) );
doMode( c(1, 1:9, 9) );
doMode( c(1, 1:9, 9, 9) );

}
\seealso{
Other Stats: 
\code{\link{calculateZscores}()},
\code{\link{doModeOpposite}()},
\code{\link{doSampleVariance}()},
\code{\link{doStatsSummary}()},
\code{\link{findOutliersUsingIQR}()},
\code{\link{findOutliersUsingZscores}()}
}
\concept{Stats}
