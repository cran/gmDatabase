\name{gmSQL}
\alias{gmSQLTable}
\alias{gmSQL}
\alias{gmSQL2SQL}
\alias{SQLenv}
\alias{SQL2SQLenv}
\title{Provide an R representation of SQL}
\description{
  gmSQL provides an R representation of SQL, which can be used to
  construct a complex hierachy of joins and select statements.
}
\usage{
gmSQL(.,expr=substitute(.),env=SQLenv)
gmSQL2SQL(expr,env=SQL2SQLenv)
gmSQLTable(table,as=tick(table))
}
\arguments{
  \item{.}{For \code{gmSQL} an unquote expression to be interpreted as
    the R representation of SQL described under details.}
  \item{expr}{The quoted version of such an expression.}
  \item{env}{The environment holding the variables used in the expression.}
  \item{table}{a character string giving the name of the SQL table
    denoted.}
  \item{as}{the alias of the table in the SQL expression}
}
\value{
  For gmSQL and gmSQLTable, a call representing the intended SQL
  expression as R expression. For gmSQL2SQL a character string holding
  the corresponding SQL expression.
}
\details{
  These commands allow to construct a representation of a small subset
  of SQL statements by R language objects. The following (derived) table
  value statements are supported
  \describe{
    \item{join(x,y,on=NULL)}{Represent the \code{x JOIN y ON on}.}
    \item{leftjoin(x,y,on=NULL)}{Represent the \code{x LEFT OUTER JOIN y ON on}.}
    \item{select(what=NULL,from=NULL,where=NULL,as=tick())}{
      Represents \code{(SELECT w1=v1,... FROM from WHERE where) AS as}, where
      what is a named list of the form \code{list(w1=v1,...)}. If any of
      the terms is NULL it is logically omitted.
    }
    \item{table(table,as)}{Represents \code{table AS as} in a FROM
      clause.}
    \item{table$name}{Represents \code{tableAlias.name} anywhere in an
      SQL expression e.g. in \code{what} and \code{where} clauses of a \code{SELECT}.}
    \item{Call(fun,...)}{Represents \code{fun(...)} in SQL expressions.}
    \item{x==y}{Represents \code{x=y} in SQL expressions.}
    \item{x!=y}{Represents \code{x!=y} in SQL expressions.}
    \item{x<y}{Represents \code{x<y} in SQL expressions.}
    \item{x>y}{Represents \code{x>y} in SQL expressions.}
    \item{x<=y}{Represents \code{x<=y} in SQL expressions.}
    \item{x>=y}{Represents \code{x>=y} in SQL expressions.}
    \item{between(x,y,z)}{Represents \code{x BETWEEN y AND z} in SQL expressions.}
    \item{x+y}{Represents \code{x+y} in SQL expressions.}
    \item{x-y}{Represents \code{x-y} in SQL expressions.}
    \item{x*y}{Represents \code{x*y} in SQL expressions.}
    \item{x/y}{Represents \code{x/y} in SQL expressions.}
    \item{in(x,y)}{Represents \code{x IN y} in SQL expressions.}
    \item{"x \%in\% y"}{Substitute for in(x,y).}
    \item{&}{Represents \code{x AND y} in SQL expressions.}
    \item{|}{Represents \code{x OR y} in SQL expressions.}
    \item{!}{Represents \code{NOT x} in SQL expressions.}
    \item{ifelse(x,y,z)}{Represents \code{IF x THEN y ELSE z} in SQL
      expressions.}
    \item{.(x)}{Evaluates its argument in env, i.e. it is used to quote
      calculation, which should be executed in R rather than SQL.}
    \item{x \%<<\% y}{Represents \code{x << y} in SQL expressions (Left shift).}
    \item{x \%>>\% y}{Represents \code{x >> y} in SQL expressions (Right
    shift).}
    \item{xor(x,y)}{Represents \code{x XOR y} in SQL expressions.}
    \item{x\%&\%y}{Represents \code{x & y} in SQL expressions (bitwise and).}
    \item{x\%|\%y}{Represents \code{x | y} in SQL expressions (bitwise
  or).}
    \item{x\%<==>\%y}{Represents \code{x<=>y} in SQL expressions (null
      safe equality).}
    \item{x\%\%y}{Represents \code{x \% y} in SQL expressions
      (remainder).}
    \item{x&&y}{Represents \code{x && y} in SQL expressions
      (logical AND).}
    \item{x||y}{Represents \code{x || y} in SQL expressions
      (logical OR).}
    \item{c(...)}{Represents \code{(...)} in SQL expressions.}
    \item{sum(x)}{Represents \code{SUM(x)} in SQL expressions (sum of values).}
    \item{avg(x)}{Represents  \code{AVG(x)} in SQl expressions (average of values).}
    \item{min(...)}{Represents \code{MIN(...)} in SQL expressions (minimum of values).}
    \item{max(...)}{Represents \code{MAX(...)} in SQL expressions (maximum of values).}
    \item{count(x)}{Represents \code{COUNT(x)} in SQL expressions.}
  }

  Special environments \code{SQLenv} and \code{SQL2SQLenv} are used in order to prevent code injection.
}
\references{
  \url{http://dev.mysql.com/doc/refman/5.7/en}
}
\seealso{
  \code{\link[DBI]{dbSendQuery}}
}
\examples{
tabA <- gmSQLTable("A")
tabB <- gmSQLTable("B")
ennv <- new.env(parent=SQLenv)
assign("tabA", tabA, ennv)
assign("tabB", tabB, ennv)
AB <- gmSQL(join(tabA,tabB,on=tabA$id==tabB$refID), env=ennv)
AB
gmSQL2SQL(tabA)
gmSQL2SQL(tabB)
gmSQL2SQL(AB)
legalvalues <- 1:3
assign("AB", AB, ennv)
assign("legalvalues", legalvalues, ennv)
sAB <- gmSQL(select(what=list(x=1,y=tabA$y*tabB$y),
                    from=AB,
                    where=Call("log",tabB$othervalue)<=17 &&
                          IN(tabA$inte,c(legalvalues)),
                    as=NULL), env=ennv)

cat(gmSQL2SQL(sAB))

}
\author{K. Gerald van den Boogaart, S. Matos Camacho}
\keyword{databases}
