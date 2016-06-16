\name{gmGet}
\alias{gmGet}
\alias{gmGetVar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get information about an object.
}
\description{
\code{gmGet} retrieves the information on specified objects stored in a geometallurgical database.
}
\usage{gmGet(expr,envir=parent.frame(),EXPR=substitute(expr),db=getOption("gmDB"))
gmGetVar(expr,what,EXPR=substitute(expr),envir=parent.frame(),db=getOption("gmDB"),
          unique=FALSE)
}
\arguments{
  \item{expr}{R expression of an SQL query. Expressions created with \code{\link{gmExpr}} can be used as well.}
  \item{what}{The members/variables you want the information from.}
  \item{envir}{defines the environment used for evaluation of \code{expr}.}
  \item{EXPR}{The quoted version of such an expression.}
  \item{db}{The database connection for the query.}
  \item{unique}{logical: If TRUE the execution is stopped in the case of multiple variables of the same requested variable name.}
}
\details{Every object corresponding to the query is referred by its id. \code{gmGet} delivers a list consisting of a data frame \code{info} and another list \code{sets}. In \code{info} the values of all atomic members for every found object are stated. \code{sets} consists of data frames, called after the composite member variables, stating the object id and the id of the member object. Since these members are sets, it is possible that the data frames consist of several member objects for one referred object.

If you are interested in information on a certain variable but not the content of variables of this kind, use \code{gmGetVar} instead.}
\value{list for \code{gmGet}, data.frame for \code{gmGetVar}}
\author{K. Gerald van den Boogaart, S. Matos Camacho}
\seealso{\code{\link{gmRead}}, \code{\link{gmExpr}}}
\examples{
\dontrun{gmGet(root)
gmGetVar(root, "gmUser")}
  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{databases}
