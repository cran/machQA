\name{mach.Go}
\alias{mach.Go}
\title{
    mach.Go
}
\description{
    This connects to the Machina web service, and opens strategy.
}
\usage{
    mach.Go(    
    username = NULL,
    password = NULL,
    strategy = NULL
	)
}
\arguments{
	\item{username}{
	    Required, your Machina username
		}
	\item{password}{
	    Required, your Machina password
	}
    \item{strategy}{
        Not required, this will default to default
	}
}
\details{
    This connects to the Machina web service, and opens strategy.
}
\value{
    This connects to the Machina web service, and opens strategy.
}
\author{
    Tim Norton
}
\examples{
   # mach.Go(username = "userName", password = "passWord", strategy = "strategyName")
}
\keyword{mach}