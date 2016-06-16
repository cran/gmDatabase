\docType{class}
\name{gmExpr-class}
\alias{gmExpr-class}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Class gmExpr}
\description{\code{gmExpr} can be generated using the function \link{gmExpr}. It has two slots. Such an object can be used as argument on \code{gmRead}}

\section{Slots}{
  \describe{
  \item{EXPR}{stores the expression itself.}
  \item{envir}{defines the environment used for evaluation of \code{expr}.}
  }
}

\author{
K. Gerald van den Boogaart, S. Matos Camacho
}

\examples{
myExpr <- gmExpr(root)

myExpr
}

\seealso{
\code{\link{gmRead}}, \code{\link{gmExpr}}
}

\keyword{ internal }% __ONLY ONE__ keyword per line
