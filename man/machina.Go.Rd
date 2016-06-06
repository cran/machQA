\name{mach.Go}
\alias{mach.Go}
\title{
    mach.Go
}
\description{
    This connects to the Machina web service, and opens model.
}
\usage{
    mach.Go(    
    username = NULL,
    password = NULL,
    model = NULL
	)
}
\arguments{
	\item{username}{
	    Required, your Machina username
		}
	\item{password}{
	    Required, your Machina password
	}
    \item{model}{
        Not required, this will default to default
	}
}
\details{
    This connects to the Machina web service, and opens model.
}
\value{
    This connects to the Machina web service, and opens model.
}
\author{
    Tim Norton
}
\examples{
   # mach.Go(username = "userName", password = "passWord", model = "modelName")
}
\keyword{mach}