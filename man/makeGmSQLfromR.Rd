\name{makeGmSQLfromR}
\alias{makeGmSQLfromR}
\title{Parse an R expression of SQl statements
}
\description{\code{makeGmSQLfromR} parses an R expression}
\usage{
makeGmSQLfromR(EXPR,vars,warn=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{EXPR}{R expression of an SQL query.}
  \item{vars}{A list of variables}
  \item{warn}{A logical defining the handling of a possible occuring warning.}
}
\details{\code{makeGmSQLfromR} is an internal function, which is used for constructing WHERE clauses in SELECT statements or ON clauses in JOIN statements out of an R expression.
}
\value{Returns the names for the WHERE or ON clause.
}

\author{K. Gerald van den Boogaart, S. Matos Camacho}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{gmRead}}
  }
\keyword{databases}
