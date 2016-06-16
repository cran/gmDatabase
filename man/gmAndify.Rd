\name{gmAndify}
\alias{gmAndify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Creating a conjunction}
\description{
\code{gmAndify} creates a call, which is a conjunction of list elements given in its arguments for further use in ON or WHERE clauses of SQL statements
}
\usage{
gmAndify(l,and="&")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{list of elements, which shall be conjuncted}
  \item{and}{determines the kind of conjunction being used. Default case is the logical AND.}
}
\details{
Instead of a conjunction you can create a disjunction using \code{and="|"}. This function is in \code{\link{gmReadInternal}}
}
\value{A string of class \code{call} for further use.}

\author{K. Gerald van den Boogaart, S. Matos Camacho
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gmRead}}
}
\examples{
gmAndify(c("a","b"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
