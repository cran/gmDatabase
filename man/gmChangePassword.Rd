\name{gmChangePassword}
\alias{gmChangePassword}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for changing the password of a user in the geometallurgical database. 
}
\description{
A gui for changing the password of user in the database.
}
\usage{
gmChangePassword(db=getOption("gmDB"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{A database connection object as returned by \code{gmConnectServer}.}
}
\details{
\code{gmChangePassword} allows the user to change the password for accessing data in the database. It uses \code{\link{shiny}} to provide aa gui, that hides the letters of an entered password. 
}
\author{
S. Matos Camacho
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{shiny}}
}
\examples{
\dontrun{
gmChangePassword()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ database }% __ONLY ONE__ keyword per line
