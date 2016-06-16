\name{forKeyVal}
\alias{forKeyVal}
\title{forKeyVal -- Looping named lists}
\description{
  forKeyVal loops a named list or vector, with one variable bound to the
  key and another to the value. 
}
\usage{
forKeyVal(key,val,LIST,block,envir=parent.frame())
}
\arguments{
  \item{key}{the variable to be bound by the name of the element of the LIST}
  \item{val}{the variable to be bound by the value of the element of the LIST}
  \item{LIST}{a list}
  \item{block}{the block to be exectued, with this variables bound}
  \item{envir}{the environment in which the variables are bound and the
    block is executed}
}
\value{
  the value of the last execution of the block
}
\details{
  It works much like a \code{for(val in LIST) block} with the
  difference. It however additionally binds key to the value name of the
  list element, but does not recognize continue or break
  statements. This might change at some point in future.
  \cr
  Empty or unbound names result in a binding of key to \code{NULL}.
}

\seealso{
  \code{\link{for}}, \code{\link{lapply}}
}
\examples{
forKeyVal(name,x,c(a=1,b=5,c=6),{
  cat(name,"=>",x,"\n")
})

forKeyVal(name,x,list(a=4,b=1:7,c=c(a="Aber",b="nicht")), {
  cat(name,"\n",sep="")
  if(is.null(names(x)))
    names(x) <- 1:length(x)
  forKeyVal(iname,x,x,{
    cat(name,".",iname,"=>",x,"\n",sep="")
  })
})
}
\author{K. Gerald van den Boogaart}
\keyword{utilities}
