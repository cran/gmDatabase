\name{gmAdd}
\alias{gmAdd}
\alias{gmSet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adding and updating objects in a geometallurgical database.
}
\description{
\code{gmAdd} adds a new object to a geometallurgical database. Its attributes can be set and updated with \code{gmSet}
}
\usage{
gmAdd(where, what, data, rg=getOption("defaultRightsGroup"), EXPR=substitute(where), 
  envir=parent.frame(), db=getOption("gmDB"), force=FALSE)
gmSet(where, data, varID=NULL, childs=NULL, members=NULL,EXPR=substitute(where), 
  envir=parent.frame(), db=getOption("gmDB"), update=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{where}{an expression specifying where in the hierarchical structure of the database the new object(s) should be added.}
  \item{what}{specifies the variable type of the new object(s). Apart from an R expression of an SQL query or a \code{gmExpr}, a ID from the database
  is possible, too.}
  \item{data}{list or data frame giving attributes for the new object(s).}
  \item{rg}{states the id of the rights groups this object will belongs to.}
  \item{varID}{the variable ID of the objects to be updated}
  \item{childs}{a data frame containing the class member of the object to be updated}
  \item{members}{a vector containing the names of the class members of \code{what}, which will be updated}
  \item{EXPR}{the quoted version of \code{where}.}
  \item{envir}{defines the environment used for evaluation of \code{where}.}
  \item{db}{defines the database, on which the query will be run.}
  \item{force}{logical: if \code{FALSE}, the case of missing required class members results in an abortion, otherwise only warnings are printed.}
  \item{update}{logical: if \code{TRUE}, set objects will be updated instead of adding new entries.}
}
\details{
\code{gmAdd} adds one or more new objects of the specified variable name to a geometallurgical database. The number of new objects is determined by
\code{data}. This is a list or data frame stating values for the class members of each new object. If \code{force=TRUE} missing required class 
members are ignored and a warning is given. If \code{force=FALSE} the insert is aborded in the case of missing required members with a corresponding
error message. If no rights group is given the object will not be linked to the default rights group. If that is not explicitly set by 
\code{\link{gmDefaultRightsGroup}} the new object will not be linked to any rights group. Therefore it will be accessible by everyone. 

For the actual insertion of the values of the members, the function \code{gmSet} is used. If a member given in \code{data} is already set for this 
object, then the database is being updated. Otherwise this attribute is added. Attributes which are not allowed for this object result in an abortion
and an error message.

If \code{gmSet} is used outside \code{gmAdd} the argmuments \code{varID}, \code{childs} and \code{members} may be missing and therefore set to
\code{NULL}. Then they are queried prior to the insertion. This setting is used to determine, if \code{gmSet} is called by \code{gmAdd}. If 
\code{data} contains a set object, setting the argument \code{update} to \code{TRUE} prohibits updating this member. Instead an additional entry
is added.

If \code{gmSet} is called by \code{gmAdd} and a creationTime is missing in \code{data}, \code{gmSet} adds the actual time instead.
Since that for variables of the type set more than one entry is permitted, you have the possibility of updating the entry or adding a new reference
by setting \code{update=FALSE}. If there is already more than one entry, updating is not permitted. 
}
\value{
\code{gmAdd} returns the gmID of the first added object.
}
\author{K. Gerald van den Boogaart, S. Matos Camacho}
\seealso{
\code{\link{gmRead}}, \code{\link{gmExpr}}
}
\examples{
\dontrun{
  gmAdd(root,"project",list(gmTitle="sunny thursday", gmCreator=3))
  gmSet(root$project[gmTitle=="sunny thursday"], list(gmName="something"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ databases }
