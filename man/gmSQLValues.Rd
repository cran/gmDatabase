\name{gmSQLValues}
\alias{gmSQLValues}
\title{Format vector in parenthesis and SQL quote it}
\description{Formats values for use in SQL statements}
\usage{
  gmSQLValues(v,quote=TRUE,db=getOption("gmDB"))
}
\arguments{
  \item{v}{The values to be converted, could be dataframe, numeric
    vector or something convertable to a character vector.}
  \item{quote}{Logocical value defining if the given values shall be quoted.}
  \item{db}{The database connection for which the transformation shall
    be done. Currently only mysql is supported.}
}
\value{
  \item{\code{gmSQLValues}}{
  A single string containing an SQL representation of the vector for use
  with the \code{IN} operator.}
  %\item{\code{gmEscapeStrings}}{
  %  The Strings escaped for use in SQL
  %}
}
\details{
The functions currently use \code{\link{gmEscapeStrings}} and are
therefore only reliable with RMySQL-Connections.
}
\seealso{
  \code{\link{gmEscapeStrings}}
}
\examples{
\dontrun{
.gmDB <- dbConnect(...)
complexString <- "He said: \"I\'m going to the circus\"\n"
cat(complexString)
gmEscapeStrings(complexString)
cat(gmEscapeStrings(complexString),"\n")
cat(gmSQLValues(complexString),"\n")
myfactor <- factor(c("a","a","b"))
cat(gmSQLValues(myfactor),"\n")
num <- 1:3
cat(gmSQLValues(num),"\n")

X <- data.frame(string=rep(complexString,3),fac=myfactor,x=num)
cat(gmSQLValues(X),"\n")
}

}
\author{K. Gerald van den Boogaart}
\keyword{database}
