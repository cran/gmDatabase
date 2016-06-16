\name{tick}
\alias{tick}
\title{
Create a unique name
}
\description{
\code{tick} creates a unique name out of \code{x}
}
\usage{
tick(s="T")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{The first part of the later name.}
}
\details{
\code{tick} concatenates a consecutive number to \code{s}.
}
\value{a string}
\author{K. Gerald van den Boogaart, S. Matos Camacho}

\examples{
a <- "S"
tick(a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
