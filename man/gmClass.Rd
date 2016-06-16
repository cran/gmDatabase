\name{gmClass}
\alias{gmClass}
\alias{gmClassMembers}
%\alias{gmMembers}
\title{Returning information from geometallurgicala database
%%  ~~function to do ... ~~
}
\description{
Get information on given object or variable from the database.
}
\usage{
gmClass(expr, var, envir=parent.frame(), EXPR=substitute(expr),db=getOption("gmDB"))
gmClassMembers(expr, var, envir=parent.frame(), EXPR=substitute(expr),
                db=getOption("gmDB"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{R expression of an SQL query. Expressions created with \code{\link{gmExpr}} can be used as well.}
  \item{var}{a number or variable name specifying a variable.}
  \item{envir}{defines the environment used for evaluation of \code{expr}}
  \item{EXPR}{The quoted version of such an expression.}
  \item{db}{defines the database, on which the query will be run.}
}
\details{
If you want information on a special variable in the database, specify \code{var}. You may use its variable id or the variable name. If \code{var} is missing information on the object(s) specified by \code{expr} is retrieved. 

\code{gmClass} gives information on the variable itself or the variable the object is. \code{gmClassMembers} shows information about all possible members of a variable or the variable the object is.
}
\value{
\code{gmClass} returns a data frame containing the \code{id}, \code{gmVarID}, \code{gmVarTypeID}, \code{gmVarName}, and \code{gmVarDescription} of the desired variable or object. If the object is not distinct, then information for every object retrieved. 
\code{gmClassMembers} returns a data frame containing the \code{gmVarID} and \code{gmVarName} of this all of the possible given members of variable respectively object. \code{required} is a boolean stating if the member is mandatory or not, \code{definer} states the variable, where this member is defined. If it is not inheritated, then the variable itself is given.}
\author{K. Gerald van den Boogaart, S. Matos Camacho}
\seealso{\code{\link{gmGet}}}
\examples{
\dontrun{
.gmDB <- dbConnect(...)
root <- "root"
gmClass(root)
## returns information on the root in the database

gmClass(root$project)
## returns information on every project in root in the database

gmClass(var="project")
## returns information on the variable/class project

gmClass(var=2)
## returns information on the variable with variable id 2
}}
\keyword{databases}
