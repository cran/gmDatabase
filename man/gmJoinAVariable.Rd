\name{gmJoinAVariable}
\alias{gmJoinAVariable}
\alias{gmJoinADollarExpr}
\title{Creating the join R representation of a SQL JOIN}
\description{Queries the variable types of the variables, which shall be joined, and creates the R presentation of an SQL JOIN.}
\usage{
gmJoinAVariable(sel,var,nameUse=var,bind=TRUE,db=getOption("gmDB"),selID=attr(sel,"id"))
gmJoinADollarExpr(sel,expr,nameUse,envir,bind=TRUE,db=getOption("gmDB"),
                    gmNameVarID=getGmNameVarID(db))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sel}{The selection statement in R representation.}
  \item{var}{R expression of an SQL query.}
  \item{nameUse}{the names used for the variables}
  \item{envir}{defines the environment used for evaluation of \code{expr}}
  \item{bind}{should the variable be bound}
  \item{db}{The database connection to the material analysis database.}
  \item{selID}{The ID attribute of the selection.}
  \item{expr}{An R expression.}
  \item{gmNameVarID}{The gmVarID of gmName from the database.}
}
\details{
Internals functions for constructing a SQL statement.
}
\value{
For \code{gmJoinADollarExpression} the new selection expression, where the dollar expression is added.
}

\author{K. Gerald van den Boogaart, S. Matos Camacho}


%% ~Make other sections like Warning with \section{Warning }{....} ~


%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
