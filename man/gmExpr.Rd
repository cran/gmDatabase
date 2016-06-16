\name{gmExpr}
\alias{gmExpr}
\alias{root}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creating gmExpressions
}
\description{
\code{gmExpr} creates a gmExpression
}
\usage{
gmExpr(expr,...,envir=parent.frame(),EXPR=substitute(expr))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{an R-like representation of an SQL query.}
  \item{envir}{defines the environment used for evaluation of \code{expr}.}
  \item{EXPR}{The quoted version of such an expression.}
  \item{...}{further arguments for later use in other methods.}
}
\details{
\code{gmExpr} creates an object of class \code{gmExpr}, which can be used as argument in \code{gmRead} instead of a string. Furthermore it can be extended like any other expression used in \code{gmRead}.
For use in the geometallurgical database you need to begin every expression with \code{root}, since data storage starts from this common root object.

The composition of an expression is done in the following way: for accessing a certain object in a class, use the \code{$}-operator, to access its elements use square brackets \code{[]}. By specifying \code{columnName=operation} the user can control the content of a column in the the output. Named columns are always shown in the result. If they need to be dropped set them to operation \code{drop}. If you want to group objects by a named column, set its operation to \code{group}. This grouping can only be done for elements at least one step down in the hierarchy.
}
\value{
An object of class \code{gmExpr}, containing the expression in \code{$EXPR} and the \code{envir} as attribute.
}

\author{K. Gerald van den Boogaart, S. Matos Camacho
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gmRead}}
}
\examples{
expr <- gmExpr(root$project)
\dontrun{
gmConnectServer(MySQL(), ...)
gmRead(expr$series)

gmRead(expr$series[name=gmTitle, name=drop])
## naming the element gmTitle as name and dropping it in the result

gmRead(expr[pID=id]$series[name=gmTitle, pID=group])
## grouping the series by their corresponding project in the result
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
\keyword{utilities}
