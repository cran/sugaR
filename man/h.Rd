\name{h}
\Rdversion{1.1}
\alias{h}
\title{Convert string with measurement data to row in matrix}
\description{
This function helps with the entry of data for the interpretation of blood glucose levels.
}
\usage{
h(X, ncol=2,debug=FALSE)
}
\arguments{
\item{X}{String with comma-separated values. The first is presume to be a time, the others floats.}
\item{ncol}{Number of expected values.}
\item{debug}{Assistance to developers for adding new functionality. Please ignore.}
}
\seealso{
\code{\link{u}}.
}
\examples{
h("8:47 139,11:12 75,16:18 112,19:46 71,21:48 75",ncol=2)
}
\author{Steffen Moeller \email{steffen_moeller@gmx.de}}

