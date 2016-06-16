\name{gmJoinTheID}
\alias{gmJoinTheID}
\title{Add the ID to the selection}
\description{
\code{gmJoinTheID} joins the ID attribute to the selection   
}
\usage{
gmJoinTheID(sel,nameUse,db=getOption("gmDB"),front=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sel}{The selection statement in R representation.}
  \item{nameUse}{The name for the selection statement.}
  \item{db}{The database connection to the material analysis database.}
  \item{front}{A boolean determining if this ID shall become the first column in the later data frame. Default is FALSE.}
}
\details{The variable \code{nameUse} will be used as name of the selection later referring. This function is intended for internal use in \code{gmReadInternal} only.}
\value{The changed selection call.}

\author{K. Gerald van den Boogaart, S. Matos Camacho}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gmReadInternal}}
}
\keyword{utilities}
