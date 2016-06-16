\name{extractDollarExpressions}
\alias{extractDollarExpressions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extracting a dollar expression}
\description{Extracts the expression containing a dollar symbol.
}
\usage{
extractDollarExpressions(EXPR)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{EXPR}{an R expression}
%%     ~~Describe \code{x} here~~
}

\details{\code{extractDollarExpressions} extracts the expression containing a dollar symbol. Furthermore it provides an alias for it. This function is meant for internal use in \code{gmReadInternal}, when dollar expressions occur in bracket terms.
}
\value{Returns a structure, containing the whole expression using the alias instead of the original dollar expression in \code{expr} and the dollar expression itself in \code{dollarExpr}.  
}

\author{K. Gerald van den Boogaart, S. Matos Camacho}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gmReadInternal}}
}
\examples{
extractDollarExpressions(quote(root$project == 2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
