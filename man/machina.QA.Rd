\name{mach.QA}
\alias{mach.QA}
\title{
    mach.QA
}
\description{
    This performs the bulk of the QA operations.
}
\usage{
    mach.QA(
    ticker = NULL,
    day = NULL
	)
}
\arguments{
	\item{ticker}{
	    Required, the ticker you want to QA, either ibm or spy
	}
    \item{day}{
        Required, the date to perform QA on. Choose a trading day between 1/1/2014 through 4/30/2014 in YYYYMMDD format.
	}
}
\details{
    This performs the bulk of the QA operations.
}
\value{
    This performs the bulk of the QA operations.
}
\author{
    Tim Norton
}
\examples{
   # mach.QA(ticker = "ibm", day = "20140221")
}
\keyword{mach}