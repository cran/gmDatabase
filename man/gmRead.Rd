\name{gmRead}
\alias{gmRead}
\alias{gmReadInternal}
\title{Return the result of an SQl query given as R-like representation}

\description{
\code{gmRead} translates the R representation \code{expr} of an SQL query into SQL and evaluates it in the database \code{db}.
}
\usage{
gmRead(expr,envir=parent.frame(),EXPR=substitute(expr),limit=-1,db=getOption("gmDB"))
gmReadInternal(EXPR,sel=NULL,db,gmNameVarID=getGmNameVarID(db))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{R expression of an SQL query. Expressions created with \code{\link{gmExpr}} can be used as well.}
  \item{envir}{defines the environment used for evaluation of \code{expr}.}
  \item{EXPR}{The quoted version of such an expression.}
  \item{sel}{The selection statement in R representation.}
  \item{limit}{defines the number of lines returned in the query.}
  \item{db}{defines the database, on which the query will be run.}
  \item{gmNameVarID}{The gmVarID in the gmDatabase of gmName for later referencing of an object by its name in the gmDatabase.}
}
\details{
\code{gmRead} is the principal function of the gmDatabase package. It provides the main functionality: Receiving an R expression \code{expr} of an SQL query, it returns a dataframe, containing the requested data. On the other hand, \code{gmReadInternal}, indicated by its notation, is meant for in-package use only. During the execution of \code{gmRead} it creates a call containing the R representation, which is used later in \code{gmSQL} to create and run a proper SQL query against the database. 
}
\value{
For \code{gmRead} a dataframe is returned. It contains the requested data.
For \code{gmReadInternal} a call is returned, consisting of an expression to be interpreted as
    the R representation of SQL by \code{gmSQL}. Furthermore a list of the asked variables in 
    \code{EXPR} and the columns, where to look in the database tables are returned, too.
}

\author{K. Gerald van den Boogaart, S. Matos Camacho}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gmExpr}}, \code{\link{gmSQL}}
}
\examples{
\dontrun{
.gmDB <- dbConnect(...)
root <- "root"
erg <- gmRead(root)
## returns the ID of root in the database

gmRead(root$project[gmTitle=group])
## groups
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{databases}
