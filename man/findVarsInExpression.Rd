\name{findVarsInExpression}
\alias{findVarsInExpression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\code{findVarsInExpression}
}
\description{
parses expressions and returns the variables
}
\usage{
findVarsInExpression(EXPR)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{EXPR}{a call object representing an R expression}
}
\details{
\code{findVarsInExpression} takes a call and returns the names. It is used in \code{gmReadInternal} for parsing an R expression of an SQL query for variables.
}
\value{
A string consisting of the variables.
}
\author{K. Gerald van den Boogaart, S. Matos Camacho
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{replaceVarsInExpression}}, \code{\link{gmReadInternal}}
}
\examples{
a <- quote( A==B^C+A^2+C )
findVarsInExpression(a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
