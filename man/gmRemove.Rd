\name{gmRemove}
\alias{gmRemove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Delete objects in an geometallurgical database
}
\description{
Deletes the specified object in an geometallurgical database.
}
\usage{
gmRemove(expr, which=NULL, var=NULL, EXPR=substitute(expr), WHICH=substitute(which),
  envir=parent.frame(), db=getOption("gmDB"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{R expression of an SQL query. Expressions created with \code{\link{gmExpr}} and an ID in the database can be used as well.}
  \item{which}{Specifies for a set variable, which entry shall be deleted.}
  \item{var}{Defines the attributes, that shall be deleted.}
  \item{EXPR}{The quoted version of such an expression.}
  \item{WHICH}{The quoted version of \code{which}.}
  \item{envir}{defines the environment used for evaluation of \code{expr}.}
  \item{db}{The connection to the database.}
}
\details{
\code{gmRemove} deletes the specified object in a geometallurgical database. This may include the removal of all its members, too. 

If you only want to remove a certain attribute for an object, than use the argument \code{var} for specifying. In the case of this attribute being an object itself but not a value, you may have more than one entry to choose for deleting. Therefor choose the right one by stating an id or an \link{gmExpr} in the \code{which} argument.
}
\value{
No value.
}

\author{K. Gerald van den Boogaart, S. Matos Camacho}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gmRead}}, \code{\link{gmExpr}}
}
\examples{
\dontrun{
gmRemove(4545)
gmRemove(root$project[gmTitle=="Project X"])
gmRemove(root$gmUserGroup, root$gmUser[gmUserName=="userXY"], var="gmUser")
gmRemove(root$project[gmTitle=="Project X"], var="gmTitle"))
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{databases}
