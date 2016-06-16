\name{gmCreateClass}
\alias{gmCreateClass}
\alias{gmAddMembers}
\alias{gmCreateRightsGroup}
\alias{gmCreateUserGroup}
\alias{gmSetRights}
\alias{gmAddUserToGroup}
\alias{gmSetRightsGroup}
\alias{gmCreateUser}
\alias{gmDefaultRightsGroup}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Providing new classes and adding new users, rights groups, usergroups, etc.}

\description{For working with a (geometallurgical) database you need at least some users, rights groups to manage their access rights, or 
classes/variables, you want to store the data.}

\usage{
gmCreateClass(name, type, description, parent="gmObject", 
  envir=parent.frame(), db=getOption("gmDB"))
gmAddMembers(class, members, required, envir=parent.frame(), db=getOption("gmDB"))
gmCreateRightsGroup(rightsGroup, envir=parent.frame(), db=getOption("gmDB"))
gmCreateUserGroup(userGroup, envir=parent.frame(), db=getOption("gmDB"))
gmSetRights(userGroup, rightsGroup, write=TRUE, read=TRUE, 
  envir=parent.frame(), db=getOption("gmDB"))
gmAddUserToGroup(user, userGroup, envir=parent.frame(), db=getOption("gmDB")) 
gmSetRightsGroup(object, rightsGroup, envir=parent.frame(), db=getOption("gmDB"))
gmCreateUser(user, password, userGroups, envir=parent.frame(), db=getOption("gmDB"))
gmDefaultRightsGroup(rightsGroup, db=getOption("gmDB"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{The name of the new class, which shall be created.}
  \item{type}{The type of the new class.}
  \item{description}{A short description of the calss and what it is for.}
  \item{parent}{The super class of the new one.}
  \item{class}{The name of the class to which the members shall be added.}
  \item{members}{The names of the new class members.}
  \item{required}{A logical. If \code{TRUE} this member has to be initialised, when an instance of the object is added to the database.}
  \item{rightsGroup}{The name or, if it refers an existing one, ID of the rights group.}
  \item{userGroup}{The name or, if it refers an existing one, ID of the usergroup}
  \item{user}{The username oder ID of the user, who shall be added.}
  \item{write}{A logical. If \code{TRUE} the stated usergroup is granted writing rights.}
  \item{read}{A logical. If \code{TRUE} the stated usergroup is granted reading rights.}
  \item{object}{An ID or \code{gmExpr} of the object, for that the rights group shall be determined.}
  \item{password}{The password of the new user.}
  \item{userGroups}{The usergroups the new user shall be part of.}
  \item{envir}{defines the environment used for evaluation of the other arguments.}
  \item{db}{A database connection object as returned by \code{gmConnectServer}.}
}
\details{
\code{gmCreateClass} allows the suer to add a new variable/class to the database. If \code{set} is given a type, the new class will be inferred from \code{parent}, which is \code{gmObject} by default.

\code{gmAddMembers} allows to add classes/variable to other classes. The option \code{required} determines, if this member must be initialised during construction of an instance of \code{class}.

\code{gmCreateRightsGroup} and \code{gmCreateUserGroup} create new rights or user groups. A rights group contains the sets \code{readers} and \code{writers}, consisting of usergroups, which determine, who is allowed to access or change an object. The rights group of an object can be set with \code{gmSetRights} after the initialisation of the object.

\code{gmCreateUser} is for creation of a new user. You may add this new user to the appropriate usergroups using \code{gmAddUserToGroup}. 

During object initialisation using \link{gmAdd} the arguments \code{rg} can give the needed rights group. If is not given, a default value is used. This can be set by \code{gmDefaultRightsGroup}. 
}
\value{\code{gmCreateRightsGroup} and \code{gmCreateUserGroup} return the gmID of this object.
}

\author{S. Matos Camacho}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{gmConnectServer}
}
\examples{
\dontrun{
gmConnectServer(MySQL(), host="myServer.mydomain.edu", user="mysqlUser",
  passwd="mysqlPassword", dbname="myDB")

rg = gmCreateRightsGroup("Protected")
ug = gmCreateUserGroup("Admins")
gmSetRights("Admins","Protected",read=TRUE,write=TRUE)
gmAddUserToGroup("admin","Admins")

gmSetRightsGroup(rg,"Protected")
gmSetRightsGroup(ug,"Protected")

rg = gmCreateRightsGroup("general")
ug = gmCreateUserGroup("users")
gmSetRightsGroup(c(rg,ug),"Protected")

gmAddUserToGroup("admin","users")
gmSetRights("users", "general", read=TRUE, write=TRUE)

gmCreateUser("myuser","mypassword",c("users"))
gmCreateUser(userGroups=c("users"))
gmDisconnectServer()

gmConnectDB(server="myServer.mydomain.edu",user="mysqlUser",passwd="mysqlPassword")
gmDefaultRightsGroup("general")

gmListVariables()

gmCreateClass("sample","set","a material sample",parent="gmObject")
gmAddMembers("root","sample", required=FALSE)
gmClassMembers(var="sample")
gmAdd(root,"sample",data.frame(gmName="Sample1"))

gmDisconnectServer()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ database }
\keyword{ utilities }% __ONLY ONE__ keyword per line
