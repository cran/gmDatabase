\name{replaceVarsInExpression}
\alias{replaceVarsInExpression}
\title{Replace Variables in Expressions}
\description{
  Replaces the variables in expressions by something else and returns
  the expression.
}
\usage{
replaceVarsInExpression(EXPR,vars,warn=FALSE)
}
\arguments{
  \item{EXPR}{a call object representning an R expression}
  \item{vars}{a names list providing the replacements for the
    variables. The names represent the variable names to be replaced.}
  \item{warn}{If TRUE warns in case of undefined variables. NA produces
    an error.}
}
\value{
  An expression similar to EXPR, with the variables replaced.
}
\examples{
a <- quote( A==B^C+A^2+C )
replaceVarsInExpression(a,list(A=quote(2*r^2),C=as.name("R"),B=pi))
a <- quote( A+B+C )
replaceVarsInExpression(a,list(A=quote(C^2),B=quote(A^2),C=quote(B^2)))
}
\author{K. Gerald van den Boogaart}
\keyword{utilities}
