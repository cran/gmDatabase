\name{repairGmGrandChilds}
\alias{repairGmGrandChilds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Updating second order inheritance.
}
\description{
Updates the table \code{gmGrandChilds} in a geometallurgical database.
}
\usage{
repairGmGrandChilds(db=getOption("gmDB"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{defines the database, on which the query will be run.}
}
\details{
The table \code{gmGrandChilds} is used for a fast and effectiv access to the hierarchical structure of a geometallurgical database. Here inheritance of second order is recorded. This information is necessary for retrieving all class members for a given object in \code{gmClassMembers}.
}
\value{string if any changes made in \code{gmGrandChilds}.}
\author{K. Gerald van den Boogaart, S. Matos Camacho}
\seealso{\code{\link{gmClass}}, \code{\link{gmClassMembers}}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{databases}
\keyword{utilities}
