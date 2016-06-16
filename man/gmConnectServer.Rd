\name{gmConnectServer}
\alias{gmCreateDB}
\alias{gmDisconnectServer}
\alias{gmListVariables}
\alias{gmListVariableTypes}
\alias{gmDBSummary}
\alias{gmEscapeStrings}
\alias{gmConnectServer}
\alias{gmRequest}
\alias{gmRequest,character,list,ANY-method}
\alias{gmRequest,character,list,MySQLConnection-method}
\title{Geometallurgy Database connection}
\description{
  A geometallurgy database is a database on a relational database
  server. It can handle very general and recursive information for
  the transparent access to whole projects from a statistical analysis
  software.  
}
\usage{
gmConnectServer(..., server="localhost", dbuser, asDefault=TRUE)
gmDisconnectServer(db=getOption("gmDB"))
gmCreateDB(..., dbName="gmDatabase", admin=NULL, adminPwd=NULL)
gmListVariables(pattern="\%",db=getOption("gmDB"))
gmListVariableTypes(db=getOption("gmDB"))
gmDBSummary(db=getOption("gmDB"))
gmEscapeStrings(s,db=getOption("gmDB"))
gmRequest(fun, parameters, db)
}
\arguments{
  \item{...}{Arguments to \code{\link{dbConnect}} from \link{RMySQL} to contact the
    database server.}
  \item{asDefault}{Logical indicating whether being used as default connection.}
  \item{db}{A database connection object as returned by \code{gmConnectServer}.}
  \item{server}{The address of the server, where the R server.}
  \item{dbuser}{A list giving the \code{login} and \code{password} of the user working in the geometallurgical database.}
  \item{dbName}{String stating the name for the database to-be.}
  \item{admin}{The login for the administrator/the first database user.}
  \item{adminPwd}{The password for the administrator/the database user.}
  \item{pattern}{An SQL-regular expression (for LIKE) to specify the
    variable names to be listed.} 
  \item{s}{A character vector of strings to be escaped.}
  \item{fun}{A function name.}
  \item{parameters}{A list of parameters for the function \code{fun}}
}
\value{
  \item{gmConnectServer}{Returns a server connection to be used in subsequent calls. It is assigned to the hidden variable \code{.gmDB}.
    If \code{server} ist set to localhost, it will be a database connection instead.}
  \item{gmDisconnectServer}{Returns the same as \code{gmDisconnectDB}. Additionally \code{.gmDB} is set to zero.}
  \item{gmCreateDB}{No return value.}
  \item{gmListVariables}{Returns a dataframe describing the variables
    defined in the geometallurgy database.}
  \item{gmListVariableTypes}{Returns a dataframe describing the variable
    types defined in the geometallurgy database.}
  \item{gmDBSummary}{Returns a dataframe giving variable name,
    variable type and number of objects 
    for each variable.}
}
\details{
  The geometallurgy database provides an abstraction layer to a
  relational database storage of many eventually differently structured
  datasets, which might contain datasets recursively. Only in this very
  complex structure it is possible to respresent the data needed for
  typical statistical tasks of geometallurgy.

  The data storage starts from a common root object "root", which
  contains a variable set variable "project". 
  
  The Database connection is handled by \code{gmConnectServer} and \code{gmDisconnectServer}. In order to assure a proper rights management, the 
  user does not directly connect to the database. We implemented a two level security system. The first level represents the connection to the 
  database server. Therefore a general user on the server is needed, who is provided with the required rights to access and manipulate the database 
  itself. The corresponding credentials are compiled in \code{...} and will be forwarded to \link{dbConnect} from the package \link{RMySQL}.
  Data access happens on the second level, where the \code{dbuser} is involved. His credentials are checked against the information stored in the 
  database. Everytime some information is queried from the database, the result is given according to the rights this user owns on this particular
  data. If no user is explicitly given during the function call, i.e. \code{dbuser} is missing, \code{gmConnectServer} will start a dialog to access
  the (db)user's credentials. Same is true, if information on the general database server user is omitted.
  
  In order to create a compatible database, you may \code{gmCreateDB}. A connection to the given MySQL-server is established and a new database of
  the given \code{dbName} is created. Furthermore first variables to realise the user/rights management are provided. Additonally a first 
  user/admin is added to the database with the given username and password, so that this user is able to login to the database afterwards. If no 
  MySQL login, MySQL password or MySQL host is given, a shiny app requests these information. Same happens if no login or password is given for the 
  first user/admin.
  
  This package can be used in a client - server approach. Then \code{gmConnectServer} will not return a database connection, but a socket
  connection. It will be used to send the commands to an R server. The actual translation from R into SQL is then done on the server. Again only 
  the accessible data according to the user's rights will be returned. In order to activate the client-server mode, use the argument \code{server} 
  to state the server you want to access. By default \code{server} is set to \code{localhost}.
  
  \code{gmRequest} is an internal function. It should not be called directly. Depending on the class of database connection used, it decides if the
  \code{fun} is evaluated locally or handed over to the server.
}
\seealso{\code{\link[RMySQL]{MySQLConnection-class}}
  
}
\examples{
\dontrun{
gmCreateDB(MySQL(), user="mysqlUser", password="mysqlPassword", host="mysqlServer", 
  dbName="myDB", admin="admin",adminPwd="myAdminPassword")
gmConnectServer(MySQL(), group="test", server="localhost")
gmListVariables(pattern=".*",db=.gmDB)
gmListVariableTypes(db=.gmDB)
gmDBSummary(db=.gmDB)
gmDisconnectServer()

}
}
\author{K. Gerald van den Boogaart, S. Matos Camacho}
\keyword{database}
