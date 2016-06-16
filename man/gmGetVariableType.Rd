\name{gmGetVariableType}
\alias{gmGetVariableType}
\title{Getting information on variables in a
  geometallurgy (gm)Database}
\description{
  Reads the variable name metainformation from the gmDatabase.
}
\usage{
gmGetVariableType(sel,name,selID=attr(sel,"id"),db=getOption("gmDB"))
}
\arguments{
  \item{sel}{A call representing the intended SQL
  expression as R expression.}
  \item{name}{A character vector giving the names of the variables to be
    read.}
  \item{selID}{The ID attribute of the selection.}
  \item{db}{The database connection to the geometallurgical database.}
}
\value{
  A dataframe with column name, table, decription and var, giving the
  name of the variable, the database table storing the information, the
  description (definition) of the variable, and the column of the
  database containing this data. 
}
\details{
  The geometallurgical database allows to store recursive dataframes of
  arbitrary content, provided that the same variable always contains the
  same type of information. This command allows to get the information
  on defined variables including their information. 
}

%\examples{
%}
\author{K. Gerald van den Boogaart}
\keyword{utilities}
