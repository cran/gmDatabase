### definition of the root object
root <- "root"
nofunc <- function(...) stop("Wrong function called.")

### gets the gmVarID of gmName from database
getGmNameVarID <- function(db=getOption("gmDB")) dbGetQuery(db, "SELECT gmVarID FROM gmVar WHERE gmVarName = 'gmName'")$gmVarID

### forKeyVal iterates a named list or vector
### with assigning the key and the val variable
forKeyVal <- function(key,val,LIST,block,envir=parent.frame()) {
  if( length(LIST) > 0 ) {
    block <- substitute(block)
    key   <- as.character(substitute(key))
    val   <- as.character(substitute(val))
    okey  <- mget(key,envir,ifnotfound=list(NULL),inherits=FALSE)
    oval  <- mget(val,envir,ifnotfound=list(NULL),inherits=FALSE)
    nam   <- names(LIST)
    for(i in 1:length(LIST)) {
      assign(key,if( identical(nam[i],"") ) NULL else nam[i],envir)
      assign(val,LIST[[i]],envir)
      eval(block,envir)
    }
  }
}

###
gmEscapeStrings <- function(s,db=getOption("gmDB")) {
  #mysqlEscapeStrings(db,as.character(s))
  dbEscapeStrings(db, as.character(s))
}

### A gmObject identifies an object by gmID in a gmDatabase
gmObjects <- function(x,db=getOption("gmDB")) {
  structure(as.integer(x),class="gmObjects")
}

### Converts a sequence of values into an SQL (,,,) expression
gmSQLValues <- function(v,quote=TRUE,db=getOption("gmDB")) {
  if( is.data.frame(v) ) {
    n <- nrow(v)
    if (ncol(v) > 1)
      paste(apply(matrix(unlist(lapply(v,as.character)),nrow=n),1,gmSQLValues),collapse=",\n")
    else paste(apply(matrix(unlist(lapply(v,as.character)),ncol=n),1,gmSQLValues),collapse=",\n")
  } else {
    if( is.numeric(v) ) {
      #paste("(",paste(as.integer(v),collapse=",",sep=""),")",sep="")

      paste(as.integer(v),collapse=",",sep="")
    } else if(quote)
    {
      paste("(",paste("'",gmEscapeStrings(v, db=db),"'",collapse=",",sep=""),")",sep="")
    }
    else
      paste("(",paste(v,collapse=",",sep=""),")",sep="")
  }
}

### The tickEnv is here to create unique names for tables.

tickEnv <- new.env();with(tickEnv,tick<-0)
tick <- function(s="T")  paste(s,with(tickEnv,tick<-tick+1),sep="")

###
aliasof <- function(x,name) { #intern
  #print(list(x=x,name=name))
  if(is.name(x)) {
    return(as.character(x))
  } else if(is.call(x)) {
    return(as.character(x$as))
  } else {
    return(as.character(x))
  }
}

###
gmSQLTable <- function(table,as=tick(table)) call("table",table=table,as=as)

### The SQLenv contains functions used to formulate SQL-Requests by
### R-expressions. These expressions are converted to SQL by gmSQL2SQL()
SQLenv <- new.env()
with(SQLenv, {
  # prepares a table of the database to be joint with an as clause
  # The create variable
  table <- function(table,as=tick()) call("table",table=table,as=as)
  "$" <- function(table,name) call("$",as.name(aliasof(table,substitute(name))),substitute(name))
  join <- function(x,y,on=NULL) call("join",x,y,on=on)
  leftjoin <- function(x,y,on=NULL) call("leftjoin",x,y,on=on)
  select <- function(what=NULL,from=NULL,where=NULL,as=tick(),groupby=NULL,unique=FALSE) call("select",what=what,from=from,where=where,as=as,unique=unique,groupby=groupby)
  "=="<-function(x,y) call("==",x,y)
  "<="<-function(x,y) call("<=",x,y)
  ">="<-function(x,y) call(">=",x,y)
  "&"<-function(x,y) call("&",x,y)
  "|"<-function(x,y) call("|",x,y)
  "IN" <- function(x,y) call("%in%",x,y)
  "%in%" <- function(x,y) call("%in%",x,y)
  "-" <- function(x,y) if( missing(y) ) call("-",x) else call("-",x,y)
  "!" <- function(x) call("!",x)
  "ifelse" <- function(x,y,z) call("ifelse",x,y,z)
  # Client?? remove?
  #"." <- function(x) {
  #  x <- substitute(x);
  #  eval(x,get("env",parent.frame()))
  #}
  sum <- function(x) call("sum",x)
  count <- function(x) call("count",x)
  min <- function(...) call("min",...)
  max <- function(...) call("max",...)
  between <- function(...) call ("between", ...)
  const <- function(x) call("const",x)
  values <- function(x) call("values",x)
  #  "c"<-function(...) as.call(list(as.name("c"),...))
  `Call`<-function(fun,...) as.call(list(as.name("Call"),fun,...))
  for(XXX in objects(SQLenv) ) eval(bquote(environment(.(as.name(XXX))) <- environment(gmSQLValues)),envir=SQLenv)
  #call <- call
  #"if"<-get("if")
  #as.name<-as.name
  #aliasof<-aliasof
  #substitute<-substitute
  #tick<-tick
  #"{"<-.Primitive("{")
  "("<-get("(")
  c<-c
  gmEscapeStrings<-gmEscapeStrings
})
parent.env(SQLenv)<-emptyenv()


# who checks for limition of set?
SQL2SQLenv <- new.env()
with(SQL2SQLenv, {
  table <- function(table,as) paste(as.character(table),as,sep=" AS ")
  "$" <- function(table,name) paste(as.character(substitute(table)),as.character(substitute(name)),sep=".")
  join <- function(x,y,on=NULL) paste("(",x,"JOIN",y,"ON",on,")",sep=" ")
  leftjoin <- function(x,y,on=NULL) paste("(",x,"LEFT OUTER JOIN",y,"ON",on,")",sep=" ")
  select <- function(what=NULL,from=NULL,where=NULL,as=tick(),unique=FALSE,groupby=NULL) {
    if( is.null(what) )
      what <- "*"
    nams <- names(what)
    what<-paste(sapply(1:length(what),
                       function(i) {
                         en2 <- new.env(parent=SQL2SQLenv)
                         assign("UUU",what[[i]],en2)
                         n <- nams[i]
                         if(is.null(nams[i]))
                           paste(gmSQL2SQL(expr=what[[i]],env=en2))
                         else
                           paste(gmSQL2SQL(expr=what[[i]],env=en2),"as",nams[i],sep=" ")
                       }),collapse=",")
    if( !is.null(where) ) {
      where <- paste("\nWHERE",where)
    }
    lv<-length(groupby)>0
    if( lv ) {
      groupby= paste("\nGROUP BY ",paste(sapply(groupby,gmSQL2SQL),collapse=",")," ")
    } else
      groupby= ""
    inner <- paste("SELECT",ifelse(unique," DISTINCT ",""),what,"\nFROM",from,where,groupby,"\n")
    if( is.null(as) ) {
      return(inner)
    }
    paste("(",inner,") AS ",as,sep="")
  }
  "=="<-function(x,y) paste("(",x,"=",y,")")
  "<="<-function(x,y) paste("(",x,"<=",y,")")
  ">="<-function(x,y) paste("(",x,">=",y,")")
  "between"<-function(x,y,z) paste("(",x, " BETWEEN ",y, " AND ",z,")")
  "&"<-function(x,y)  paste("(",x," AND ",y,")")
  "|"<-function(x,y)  paste("(",x," OR ",y,")")
  "%in%" <- function(x,y) {
    if( length(y)==1 && substr(y,1,8)=="(select ")
      paste("(",x," IN ",y,")")
    else
      ########paste("(",x," IN ",gmSQLValues(y,db=db),")")
      paste("(",x," IN ",gmSQLValues(y, quote=FALSE),")")
  }
  "-" <- function(x,y) if(missing(y)) {paste("(-",y,")")} else paste("(",x,"-",y,")")
  #"c" <- function(...) {gmSQLValues(base::c(...))}
  sum <- function(x) paste("SUM(",x,")")
  avg <- function(x) paste("AVG(",x,")")
  count <- function(x) paste("COUNT(",x,")")
  min <- function(...) paste("MIN(",paste(...,collapse=","),")")
  max <- function(...) paste("MAX(",paste(...,collapse=","),")")
  "const" <- function(x) {
    if(is.logical(x)) {
      if( identical(x,TRUE) ) {
        "1"
      } else if(identical(x,FALSE)) {
        "0"
      } else paste(" NULL ")
    }  else if(is.numeric(x))
      as.character(x)
    else
      paste("'",gmEscapeStrings(as.character(x)),"'",sep="")
  }
  "values"<-gmSQLValues
  `Call` <- function(fun,...) {paste(" ",fun,"(",...,")",sep="")}
  for(XXX in objects(SQL2SQLenv) ) eval(bquote(environment(.(as.name(XXX))) <- environment(gmSQLValues)),envir=SQL2SQLenv)
  # identical<-identical
  # as.character<-as.character
  # is.logical<-is.logical
  # is.numeric<-is.numeric
  # missing<-missing
  # length<-length
  # substr<-substr
  # gmSQLValues<-gmSQLValues
  # "if" <- get("if")
  # '&&'<-'&&'
  # paste <- paste
  # ifelse<-ifelse
  # is.null<-is.null
  #tick<-tick
  #gmAndify<-gmAndify
  # "{"<-.Primitive("{")
  # innerSelect <- innerSelect
  # substitute<-substitute
  # sapply<-sapply
  # length<-length
  # names<-names
  # "<-"<-get("<-")
  # "function"<-get("function")
  # ":"<-get(":")
  # "["<-get("[")
  # "[["<-get("[[")
  # gmSQL2SQL<-gmSQL2SQL
  # "!"<-get("!")
  # ">"<-get(">")
  # as.logical<-as.logical
  "("<-get("(")
  gmEscapeStrings<-gmEscapeStrings
})
parent.env(SQL2SQLenv)<-emptyenv()

###
local({
  binops <- list("%<<%"="<<",
                 "%>>%"=">>",
                 "+"="+",
                 "*"="*",
                 "/"="/",
                 "xor"="^",
                 "%|%"="|",
                 "%&%"="&",
                 "!="="!=",
                 "<"="<",
                 "<="="<=",
                 "%<=>%"="<=>",
                 ">"=">",
                 ">="=">=",
                 "=="="=",
                 "&&"="&&",
                 "||"="||",
                 "%%"="%"
  )
  forKeyVal(binop,sql,binops,
{
  assign(binop,eval(bquote(function(x,y) call(.(binop),x,y))),SQLenv)
  assign(binop,eval(bquote(function(x,y) paste("(",x," ",.(sql)," ",y,")",sep=""))),SQL2SQLenv)
}
  )


})


###
gmSQL <- function(.,expr=substitute(.),env=SQLenv) { #env=parent.frame()
  e1 <- new.env(parent=env)
  assign("list",list,e1)
  #cat("gmSQL in: ",paste(deparse(expr),collapse="\n"),"\n")
  #for(x in objects(envir=SQLenv))
  #  assign(x,get(x,SQLenv),e1)
  #assign("env",env,e1)
  #e <- new.env(parent=e1)
  erg<-eval(expr,e1)
  #cat("gmSQL out: ",paste(deparse(erg),collapse="\n"),"\n")
  erg
}

###
gmSQL2SQL <- function(expr,env=SQL2SQLenv) {
  e1 <- new.env(parent=env)
  #cat("gmSQL2SQ in: ",paste(deparse(expr),collapse="\n"),"\n")
  #for(x in objects(envir=SQL2SQLenv))
  #  assign(x,get(x,SQL2SQLenv),e1)
  #e <- new.env(parent=e1)
  erg<-eval(expr,e1)
  #cat("gmSQL2SQL out:",paste(erg,collapse="\n"),"\n")
  erg
}


# Returns a dataframe of variable descriptions for the variables names
# given. The colums are name, table, var and description
gmGetVariableType <- function(sel,name,selID=attr(sel,"id"),db=getOption("gmDB")) {
  gmRef <- gmSQLTable("gmRef")
  sql <- paste("SELECT DISTINCT gmVarName as name,
               gmVar.gmVarID   as gmVarID,
               gmVarTypeID AS type,
               gmTable     AS gmTable,
               \"\"          AS gmVar,
               gmVarDescription as description
               FROM (gmVar NATURAL JOIN gmVarType)  WHERE gmVar.gmVarName IN ",
               gmSQLValues(name, db=db)," GROUP BY gmVar.gmVarID",sep=" ")
  erg <- dbGetQuery(db,sql)

  found <- name %in% erg$name
  if( ! all(found))
    stop("gmGetVariableType: Variables not found: ",paste(name[!found],collapse=","))
  ttt <- table(erg$name)
  if( any(ttt>1) )
    warning("gmGetVariableType: Variable(s) ",format(ttt[ttt>1])," not unique")
  erg<-erg[match(name,erg$name),]
  erg$gmVar <- c(gmNumeric="x",gmString="x",gmText="x",gmRef="gmRefID",
                 gmBlob="x",gmInteger="x",gmBoolean="x",gmDate="x")[erg$gmTable]

  erg
}


gmJoinADollarExpr <- function(sel,expr,nameUse,envir,bind=TRUE,db=getOption("gmDB"),gmNameVarID=getGmNameVarID(db)) {
  var <- as.character(expr[[3]])
  lhs <- expr[[2]]
  myID <- tick("id")
  attr(sel,"vars")[[myID]]<-attr(sel,"id")
  #print(attributes(sel))
  sel <- gmReadInternal(lhs,sel=sel,db=db,gmNameVarID=gmNameVarID)
  #print(attributes(sel))
  sel <- gmJoinAVariable(sel,var=var,nameUse=nameUse,bind=bind,db=db)
  #print(attributes(sel))
  attr(sel,"id")<-attr(sel,"vars")[[myID]]
  #cat("gmJoinADollarExpr: id=",attr(sel,"id"),"\n")
  sel
}

gmJoinAVariable <- function(sel,var,nameUse=var,bind=TRUE,db=getOption("gmDB"),selID=attr(sel,"id")) {
  if (FALSE)
  {
    leftjoin <- nofunc
    const <- nofunc
  }
  varType <- gmGetVariableType(sel,var,db=db,selID=selID)
  va <- gmSQLTable(varType$gmTable)
  varID=varType$gmVarID
  ennv<-new.env(parent=SQLenv)
  assign("sel",sel,ennv)
  assign("va",va,ennv)
  assign("varID",varID,ennv)
  assign("selID",selID,ennv)
  varb <- gmSQL(leftjoin(
    sel,
    va,
    on= va$gmVarID==const(varID) & selID == va$gmID
  ),env=ennv
  )
  if( bind ) {
    x <- list(x=call("$",va$as,varType$gmVar))
    names(x) <- nameUse
  } else x<-NULL
  structure(varb,id=attr(sel,"id"),vars=c(attr(sel,"vars"),x))
}

gmJoinTheID <- function(sel,nameUse,db=getOption("gmDB"),front=FALSE) {
  l <- list(x=attr(sel,"id"))
  names(l)<-nameUse
  if( isTRUE(front) ) {
    structure(sel,id=attr(sel,"id"),vars=c(l,attr(sel,"vars")))
  } else {
    structure(sel,id=attr(sel,"id"),vars=c(attr(sel,"vars"),l))
  }
}


makeGmSQLfromR <-function(EXPR,vars,warn=FALSE) {
  if (FALSE)
  {
    error <- nofunc
  }
  if( is.name(EXPR) ) {
    nam <- as.character(EXPR)
    if( isTRUE(nam %in% names(vars)) )
      return(vars[[nam]])
    else {
      if( isTRUE(warn) )
        warning("makeGmSQLfromR: Variable",nam," not defined in this context")
      else if( !identical(warn,FALSE) )
        stop("makeGmSQLfromR: Variable",nam," not defined in this context")
      return(EXPR)
    }
  }
  if( is.call(EXPR) ) {
    if( identical(EXPR,as.name("$"))) {
      return(EXPR)
    }
    if( identical(EXPR,as.name("."))) {
      return(EXPR[[2]])
    }
    if( identical(EXPR[[1]],as.name("$")) ) {
      error("makeGmSQLfromR: $expression not implemented")
    } else if( length(EXPR)>1 )
      for(i in 2:length(EXPR))
        EXPR[[i]]<-makeGmSQLfromR(EXPR[[i]],vars,warn)
    return(EXPR)
  }
  if( is.numeric(EXPR) || is.character(EXPR) || is.logical(EXPR) )
    return(call("const",EXPR))
  #if( is.numeric(EXPR) || is.logical(EXPR) )
  #  return(call("const",EXPR))
  #else return(EXPR)
  warning("makeGmSQLfromR: Unkown type ",class(EXPR)," in expression translation")
  return(EXPR)
}

replaceVarsInExpression <- function(EXPR,vars,warn=FALSE) {
  if( is.name(EXPR) ) {
    nam <- as.character(EXPR)
    if( isTRUE(nam %in% names(vars)) )
      return(vars[[nam]])
    else {
      if( isTRUE(warn) )
        warning("replaceVarsInExpression: Variable",nam," not defined in this context")
      else if( !identical(warn,FALSE) )
        stop("replaceVarsInExpression: Variable",nam," not defined in this context")
      return(EXPR)
    }
  }
  if( is.call(EXPR) ) {
    if( identical(EXPR[[1]],as.name("$"))) {
      return(EXPR)
    }
    if( identical(EXPR[[1]],as.name("."))) {
      return(EXPR)
    }
    if( length(EXPR)>1 )
      for(i in 2:length(EXPR))
        EXPR[[i]]<-replaceVarsInExpression(EXPR[[i]],vars,warn)
    return(EXPR)
  }
  return(EXPR)
}

extractDollarExpressions <- function(EXPR) {
  dollarExpr <- list()
  extractDollarExpressionsIntern <- function(EXPR) {
    if( is.name(EXPR) )
      return(EXPR)
    if( is.call(EXPR) ) {
      if( identical(EXPR[[1]],as.name("."))) {
        return(NULL)
      }
      if( identical(EXPR[[1]],as.name("$"))) {
        l <- list(x=EXPR)
        nam <- tick("EX")
        names(l)<-nam
        dollarExpr <<- c(dollarExpr,l)
        return(as.name(nam))
      }
      return( as.call(lapply(EXPR,extractDollarExpressionsIntern) ) )
    }
    EXPR
  }
  expr=extractDollarExpressionsIntern(EXPR)
  return(list(expr=expr,dollarExpr=dollarExpr))
}



findVarsInExpression <-function(EXPR) {
  if( is.name(EXPR) )
    return(as.character(EXPR))
  if( is.call(EXPR) ) {
    if( identical(EXPR[[1]],as.name("$"))) {
      return(NULL)
    }
    if( identical(EXPR[[1]],as.name("."))) {
      return(NULL)
    }
    return( unlist(lapply(EXPR[-1],findVarsInExpression)) )
  }
  return(NULL)
}

gmAndify <-function(l,and="&") {
  if( length(l)==0)
    return(NULL)
  if(length(l) > 1 )
    return(call(and,l[[1]],gmAndify(l[-1],and)))
  else l[[1]]
}



gmID2Selection <- function(EXPR,gmNameVarID) {
  if(FALSE) {
    select <- nofunc
    values <- nofunc
    join <- nofunc
    const <- nofunc
    my<-nofunc
    gmName <- nofunc
  }
  ennv <- new.env(parent=SQLenv)
  assign("my",gmSQLTable("gmObject"),ennv)
  assign("EXPR",EXPR,ennv)

  if( is.numeric(EXPR) ) {  # List of ids
    prim <- gmSQL(select(what=list(gmID=my$gmID),
                         from=my,
                         where=my$gmID %in% values(EXPR),
                         as="prim"
    ),env=ennv)
    assign("prim",prim,ennv)
    structure(prim,id=gmSQL(prim$gmID,env=ennv),vars=list())
  } else if(is.character(EXPR)) { # list of named objects
    assign("gmName",gmSQLTable("gmString"),ennv)
    assign("gmNameVarID",gmNameVarID,ennv)
    prim <- gmSQL(join(my,gmName,
                       on=gmName$gmVarID==const(gmNameVarID) & my$gmID==gmName$gmID & (gmName$x %in% EXPR)
    ),env=ennv)
    assign("prim",prim,ennv)
    structure(prim,id=gmSQL(my$gmID,env=ennv),vars=list())
  }
}

gmExpr <- setClass("gmExpr",
                   representation(EXPR = "ANY",
                                  envir = "environment"
                                  )
)

setMethod("initialize", "gmExpr",
          function(.Object, EXPR, envir) {
            .Object@EXPR = EXPR
            .Object@envir = envir
            return(.Object)
          })

gmExpr <- function(expr,...,envir=parent.frame(),EXPR=substitute(expr)){
  unquote <- function(e) {
    if (length(e) <= 1L)
      e
    else if (e[[1L]] == as.name("."))
      eval(e[[2L]], envir)
    else if (is.pairlist(e))
      as.pairlist(lapply(e, unquote))
    else as.call(lapply(e, unquote))
  }
  EXPR <- unquote(EXPR)
  #structure(list(EXPR=EXPR),envir=envir,class="gmExpr")
  new(Class="gmExpr", EXPR=EXPR, envir=envir)
}

###
gmRead <- function(expr,envir=parent.frame(),EXPR=substitute(expr),limit=-1,db=getOption("gmDB")) {
  unquote <- function(e) {
    if (length(e) <= 1L)
      e
    else if (e[[1L]] == as.name("."))
      eval(e[[2L]], envir)
    else if (is.pairlist(e))
      as.pairlist(lapply(e, unquote))
    else as.call(lapply(e, unquote))
  }
  EXPR <- unquote(EXPR)
  res <- gmRequest("gmiInitRead", parameters=list(EXPR=EXPR, envir=envir, limit=limit), db=db)

  return(res)
}

gmiInitRead <- function(EXPR, envir, limit, db) {
  if(FALSE) {
    userName <- nofunc
    . <- nofunc
    gmID <- nofunc
    id <- nofunc
  }

  res <- gmiFullRead(EXPR=EXPR, envir=envir, limit=limit, db=db)
  #res <- res[order(res$gmID),]
  if (nrow(res) == 0)
    return(res)

  # rights management
  userID <- gmiFullRead(gmExpr(root$user[userName==.(getOption("dbUser"))]))$gmID
  rgID <- gmRequest("gmiRightsGroup", parameters=list(gmID=res$gmID), db=db)

  ### NA correction for no given rights group
  rgID$rg[is.na(rgID$rg)] <- -1

  tmp <- names(table(rgID$rg))
  if (length(tmp) == 1 && tmp == -1)
    return(res)
  ruser <- gmiFullRead(gmExpr(root$rightsGroup[id=gmID, id %in% .(tmp), id=drop, user=.$readers$userGroup$user]))

  ### deleting all empty entries
  ruser <- ruser[!is.na(ruser$user),]

  ruser1 <- ruser[ruser$user==userID,]
  idx1 <- is.element(rgID$rg, ruser1$gmID)
  #idx1 <- rgID$rg==ruser1$gmID

  ### if no rights group is given, anyone can access
  idx2 <- rgID$rg==-1
  idx <- idx1 | idx2
  #idx[is.na(idx)] <- TRUE

  #return(res[res$gmID==rgID[idx,]$gmID,, drop=FALSE])
  return(res[res$gmID %in% rgID[idx,]$gmID,, drop=FALSE])
}

gmiRightsGroup <- function(gmID, db) {
  rgID <- dbGetQuery(db,paste("SELECT gmRightsGroup AS rg, gmID FROM gmObject WHERE gmID IN (",gmSQLValues(gmID),");", sep=""))
  return(rgID)
}

gmiFullRead <- function(EXPR, envir=parent.frame(), limit=-1, db=getOption("gmDB")) {
  EXPR <- gmiProcessExpr(EXPR,envir=envir)
  gmRequest("gmiRead", parameters=list(EXPR=EXPR,limit=limit), db=db)
}


gmiRead <- function(EXPR, limit, db, user=getOption("dbUser"))
{
  sel   <- gmReadInternal(EXPR=EXPR,db=db,gmNameVarID=getGmNameVarID(db))
  #cat("sel=\n")
  #print(sel)
  sel   <- gmJoinTheID(sel,"gmID",db=db,front=TRUE)
  ennv <- new.env(parent=SQLenv)
  assign("sel",sel,ennv)
  assign("bquote",bquote,ennv)
  assign("attr",attr,ennv)
  xxx   <- gmSQL(bquote(select(what=.(attr(sel,"vars")),
                               from=.(sel),
                               as=NULL)),env=ennv)
  #cat("xxx=\n")
  #print(xxx)
  #cat("sql=\n")
  en2 <-new.env(parent=SQL2SQLenv)
  assign("xxx",xxx,en2)
  sql <- gmSQL2SQL(xxx,env=en2)
  if (limit>0)
    sql <- paste(sql, "LIMIT", limit)
  #cat(sql,"\n")
  dbGetQuery(db,sql)
  # Rueckgabe von Server
  #rights <- gmShowRights(.(res), user)
  #res[rights$read,]
}



gmiProcessExpr <- function(EXPR,envir=parent.frame(),type=0) {
  if(type==1) {
    return(EXPR)
  }
  if( inherits(EXPR,"gmExpr")) {
    return(Recall(EXPR@EXPR,envir=EXPR@envir,type=type))
  }
  if( is.name(EXPR)) {
    if(identical(EXPR,as.name("root")))
      return(EXPR)
    if(identical(EXPR,as.name(".")))
      return(EXPR)
    EXPR=eval(EXPR,envir)
  }
  if(is.numeric(EXPR)||is.character(EXPR)) return(EXPR)
  if(!is.call(EXPR))
    stop("Only expressions are supported in gmiProcessExpr")
  fun <- EXPR[[1]]
  if( !is.name(fun) )
    stop("Only expressions are supported in gmiProcessExpr")
  switch(as.character(fun),
        "gmExpr"={gmiProcessExpr(eval(EXPR,envir),envir,type=type)},
         "$"={call("$",gmiProcessExpr(EXPR[[2]],envir),as.name(EXPR[[3]]))},
         "["={
           as.call(c(list(EXPR[[1]],
                   gmiProcessExpr(EXPR[[2]],envir)),
                   lapply(EXPR[-(1:2)],function(x) {
                     gmiProcessExpr(x,envir,type=1)
                   })
                   ))
         },
         "join"={as.call(list(EXPR[[1]],gmiProcessExpr(EXPR[[2]],envir,0),gmiProcessExpr(EXPR[[3]],envir,0),gmiProcessExpr(EXPR[[4]],envir,1)))},
         "."={gmiProcessExpr(eval(EXPR[[2]],envir),envir=envir)}
         ,stop(as.character(fun)," not implemented
                          in gmRead")

         )

}

gmReadInternal <- function(EXPR,sel=NULL,db,
                           gmNameVarID=getGmNameVarID(db)) {
  if (FALSE)
  {
    select <- nofunc
    values <- nofunc
    join <- nofunc
    const <- nofunc
    key <- nofunc
    nam <- nofunc
    expr <- nofunc
    my <- nofunc
    xxxVarID<-nofunc
    envir <- nofunc
    gmName <- nofunc
  }
#  if( inherits(EXPR,"gmExpr")) {
#    noCall = EXPR@noCall
#    EXPR = EXPR@EXPR
#    envir=attr(EXPR,"envir")
#  }
  if( is.name(EXPR) ) {
    if( identical(EXPR,as.name(".")) ) {
      return(sel)
    }                                      # an R variable
    if(identical(EXPR,as.name("root"))) {
      EXPR<-"root"
    } else stop("no such object: ",as.character(EXPR))
    #EXPR <- eval(EXPR,envir)
  }
#  if (EXPR[[1]] == '<undef>')
#    EXPR <- eval(noCall, envir)
  ennv <- new.env(parent=SQLenv)
  assign("my",gmSQLTable("gmObject"),ennv)
  assign("gmName",gmSQLTable("gmString"),ennv)
  assign("EXPR",EXPR,ennv)
  if( is.numeric(EXPR) ) {  # List of ids
    prim <- gmSQL(select(what=list(gmID=my$gmID),
                         from=my,
                         where=my$gmID %in% values(EXPR),
                         as="prim"
    )
    ,env=ennv)
    assign("prim",prim,ennv)
    structure(prim,id=gmSQL(prim$gmID,env=ennv),vars=list())
  } else if(is.character(EXPR)) { # list of named objects
    assign("gmNameVarID",gmNameVarID,ennv)
    prim <- gmSQL(join(my,gmName,
                       on=gmName$gmVarID==const(gmNameVarID) & my$gmID==gmName$gmID & (gmName$x %in% const(gmEscapeStrings(EXPR)))
    ),env=ennv)
    assign("prim",prim,ennv)
    structure(prim,id=gmSQL(my$gmID,env=ennv),vars=list())
  } else if( is.call(EXPR) ) {
    fun <- EXPR[[1]]
    if( is.name(fun) ) {
      switch(as.character(fun),
             "$"={
               # expects recursive expression on left handside
               # expects variable name on right hand side
               lhs <- EXPR[[2]]
               rhs <- as.character(EXPR[[3]])
               ## expects a (derived) table
               sel <- gmReadInternal(EXPR=lhs,sel=sel,db=db,gmNameVarID=gmNameVarID)
               assign("sel",sel,ennv)
               vars <- attr(sel,"vars")
               assign("id",attr(sel,"id"),ennv)
               var <- gmGetVariableType(sel,rhs,db=db)
               assign("xxxVarID",var$gmVarID,ennv)
               #if( is.na(id) ) stop("gmReadInternal: Id undefined")
               assign("my",gmSQLTable("gmRef"),ennv)
               gmS <- gmSQL(join(sel,my,on= id==my$gmID & my$gmVarID==const(xxxVarID)),env=ennv)
               structure(gmS,id=gmSQL(my$gmRefID,env=ennv),vars=vars)
             },
             "["={
               lhs <- EXPR[[2]]
               rhs = EXPR[-(1:2)]
               sel <- gmReadInternal(EXPR=lhs,sel=sel,db=db,gmNameVarID=gmNameVarID)
               assign("sel",sel,ennv)
               vars <- attr(sel,"vars")
               id   <- attr(sel,"id")
               cond <- list()
               group <- list()
               # obj[x,"y",t=x+y,z==4,t==z*z] => select gmID as id, x,y, (x+y) as t from obj where z=4 AND t=z*z;
               #
               forKeyVal(key,val,rhs,{
                 #print(vars)
                 if( is.null(key) ) {
                   # bind a variable
                   if( is.name(val) ) # root[x] => select x ...
                     val <- as.character(val)
                   if( is.character(val) ) { # root["x"], # root[c("x","z")]
                     # => select x,z ...
                     attr(sel,"vars")<-vars
                     attr(sel,"id")<-id
                     for(x in val) sel<-gmJoinAVariable(sel,x,x,db=db,bind=TRUE)
                     vars <- attr(sel,"vars")
                     id   <- attr(sel,"id")
                   } else { # root[x==3] => select gmID as id from ... where x=3
                     # give a condition
                     attr(sel,"vars")<-vars
                     dollar <- extractDollarExpressions(val)
                     forKeyVal(nam,expr,dollar$dollarExpr,{
                       sel<-gmJoinADollarExpr(sel,expr,nam,
                                              envir=envir,db=db,bind=TRUE)
                     })
                     val <- dollar$expr
                     myvars <- findVarsInExpression(val)
                     for(x in myvars) {
                       if( is.call(x) ) {
                         stop("recursive variables not yet implemented")
                       } else if( !(x %in% names(attr(sel,"vars")) ) )
                         sel<-gmJoinAVariable(sel,x,x,db=db,bind=TRUE)
                     }
                     where <- makeGmSQLfromR(val,attr(sel,"vars"))
                     cond <- c(cond,list(where))
                   }
                 } else {
                   if( is.na(key) ) stop("gmRead: NA name")
                   if( identical(key,"") ) stop("gmRead: Empty name")
                   ## define a derived variable
                   if( is.name(val) && identical(as.character(val),"gmID") ) {
                     ## root[key=gmID] => select gmID as gmID ...
                     l <- list(x=id)
                     names(l)<-key
                     vars <- c(vars,l)
                   } else if( is.name(val) && identical(as.character(val),"drop") ) {
                     vars[as.character(key)]<-NULL
                   } else if( is.name(val) && identical(as.character(val),"group") ) {
                     x <- as.character(key)
                     if( !(x %in% names(attr(sel,"vars")) ) )
                       sel<-gmJoinAVariable(sel,x,x,db=db,bind=TRUE)
                     group<-c(group,attr(sel,"vars")[x])
                   } else {
                     ## root[key=EXPR] => select (EXPR) as key ...
                     ## root[a=x] => select x as a ...
                     ## root[a=x+y*y*ifelse(x==y,cxx,cxy)]
                     attr(sel,"vars")<-vars
                     dollar <- extractDollarExpressions(val)
                     forKeyVal(nam,expr,dollar$dollarExpr, {
                       sel<-gmJoinADollarExpr(sel,expr,nam,
                                              envir=envir,db=db,bind=TRUE)
                     })
                     val <- dollar$expr
                     myvars <- findVarsInExpression(val)
                     for(x in myvars) {
                       if( is.call(x) ) {
                         stop("recursive variables not yet implemented")
                       } else if( !(x %in% names(attr(sel,"vars")) ) )
                         sel<-gmJoinAVariable(sel,x,x,db=db,bind=TRUE)
                     }
                     ex <- list(x=makeGmSQLfromR(val,attr(sel,"vars"),warn=TRUE))
                     names(ex)<-key
                     vars <- c(vars,ex)
                   }
                 }
               }
               )
               what <- c(vars,list(gmID=id))
               assign("what",what,ennv)
               assign("sel",sel,ennv)
               assign("cond",cond,ennv)
               assign("gmSQL",gmSQL,ennv)
               assign("gmAndify",gmAndify,ennv)
               assign("group",group,ennv)
               assign("ennv",ennv)
               sel <- gmSQL(select(what=what,
                                   from=sel,
                                   where=gmAndify(cond),groupby=group),env=ennv)
               vars <- lapply(structure(names(vars),names=names(vars)),function(x) call("$",sel$as,as.name(x)))
               assign("sel",sel,ennv)
               id <- gmSQL(sel$gmID,env=ennv)
               sel <- structure(sel,
                                vars=vars,
                                id=id
               )
               sel
             },
             join={
               stopifnot( length(EXPR) %in% 3:4 )
               x <- gmReadInternal(EXPR[[2]],db=db,gmNameVarID=gmNameVarID)
               y <- gmReadInternal(EXPR[[3]],db=db,gmNameVarID=gmNameVarID)
               nx <- names(attr(x,"vars"))
               ny <- names(attr(y,"vars"))
               nb <- nx[nx %in% ny]
               on = list()
               allVars <- c(attr(x,"vars"),attr(y,"vars")[! (ny %in% nb)])
               if( length(EXPR)==4 ){
                 on = makeGmSQLfromR(EXPR[[4]],allVars)
               }
               if( length(nb)>0 )
                 on <- c(on,lapply(nb,function(n) call("==",attr(x,"vars")[[n]],attr(y,"vars")[[n]])))
               assign("gmAndify",gmAndify,ennv)
               assign("on",on,ennv)
               assign("x",x,ennv)
               assign("y",y,ennv)
               on <- gmSQL(gmAndify(on,"&"),env=ennv)
               assign("on",on,ennv)
               structure(gmSQL(join(x,y,on=on),env=ennv),id=attr(x,"id"),vars=allVars)
             },
             default=stop(as.character(fun)," not implemented
                          in gmRead")
      )
    } else {
      stop("Only expressions are supported in gmInternalRead")
    }
  }
}

gmClass <- function(expr,var,envir=parent.frame(),EXPR=substitute(expr),db=getOption("gmDB")) {
  if (!missing(var))
    gmRequest("gmiClassVar", parameters=list(var=var), db=db)
  else {#gmRequest("gmiClass", parameters=list(EXPR=EXPR, envir=envir), db=db)
    id <- gmRead(envir=envir, EXPR=EXPR, db=db)
    if (dim(id)[1] < 1)
      stop("No object found. Maybe you don't have sufficient access rights.")
    gmRequest("gmiClass", parameters=list(id=data.frame(id$gmID)), db=db)
  }
}

### gmClass for expressions
gmiClass <- function(id, db) {
  sql <- paste("SELECT DISTINCT gmRefID as gmID,gmVarID, gmVarTypeID,gmVarName,gmVarDescription FROM gmRef
                   NATURAL JOIN gmVar WHERE gmRefID IN ",gmSQLValues(id,db=db), ";", sep="")
  dbGetQuery(db,sql)
}

### gmClass for variables
setGeneric("gmiClassVar", function(var, db) {standardGeneric("gmiClassVar")})

setMethod("gmiClassVar", signature = c(var="character", db="MySQLConnection"),
          definition = function(var, db) {
            sql <- paste("SELECT gmVarID, gmVarTypeID,gmVarName,gmVarDescription FROM gmVar
                   WHERE gmVarName IN (",gmSQLValues(var,db=db),")")
            dbGetQuery(db,sql)
          })

setMethod("gmiClassVar", signature = c(var="numeric", db="MySQLConnection"),
          definition = function(var, db) {
            sql <- paste("SELECT gmVarID, gmVarTypeID,gmVarName,gmVarDescription FROM gmVar
                   WHERE gmVarID IN (",gmSQLValues(var,db=db), ")")
            dbGetQuery(db,sql)
          })


gmClassMembers <- function(expr,var,envir=parent.frame(),EXPR=substitute(expr),db=getOption("gmDB")) {
  if ( !missing(var) )
    gmRequest("gmiClassMembersVar", parameters=list(var=var), db=db)
  else {
    cls <- gmClass(envir=envir, EXPR=EXPR, db=db)
    if (length(cls) == 0)
      return(NULL)
    varn <- cls$gmVarID
    gmRequest("gmiClassMembers", parameters=list(varn=varn), db=db)
  }
}

### gmiClassMembers for expressions
gmiClassMembers <- function (varn, db) {
  sql <- paste("SELECT DISTINCT V2.gmVarID as gmVarID,V2.gmVarName as gmVarName,E.required AS required, VX.gmVarName AS definer, V2.gmVarTypeID AS gmVarTypeID FROM ((gmVar V1 JOIN  (gmGrandChilds C JOIN gmVar VX ON C.gmVarID=VX.gmVarID) ON V1.gmVarID=C.grandchild) JOIN gmElements E ON C.gmVarID=E.gmVarID) JOIN gmVar V2 ON E.member=V2.gmVarID WHERE V1.gmVarID IN (",
               gmSQLValues(varn,db=db),")")
  dbGetQuery(db,sql)
}


### gmiClassMembers for variables
setGeneric("gmiClassMembersVar", function(var, db) {standardGeneric("gmiClassMembersVar")})

setMethod("gmiClassMembersVar", signature = c(var="character", db="MySQLConnection"),
          definition = function(var, db) {
            sql <- paste("SELECT DISTINCT V2.gmVarID as gmVarID,V2.gmVarName as gmVarName,E.required AS required, VX.gmVarName AS definer, V2.gmVarTypeID AS gmVarTypeID FROM ((gmVar V1 JOIN  (gmGrandChilds C JOIN gmVar VX ON C.gmVarID=VX.gmVarID) ON V1.gmVarID=C.grandchild) JOIN gmElements E ON C.gmVarID=E.gmVarID) JOIN gmVar V2 ON E.member=V2.gmVarID WHERE V1.gmVarName IN (",
                         gmSQLValues(var,db=db), ")")
            dbGetQuery(db, sql)
          })

setMethod("gmiClassMembersVar", signature = c(var="numeric", db="MySQLConnection"),
          definition = function(var, db) {
            var <- var
            sql <- paste("SELECT DISTINCT V2.gmVarID as gmVarID,V2.gmVarName as gmVarName,E.required AS required, VX.gmVarName AS definer, V2.gmVarTypeID AS gmVarTypeID FROM ((gmVar V1 JOIN  (gmGrandChilds C JOIN gmVar VX ON C.gmVarID=VX.gmVarID) ON V1.gmVarID=C.grandchild) JOIN gmElements E ON C.gmVarID=E.gmVarID) JOIN gmVar V2 ON E.member=V2.gmVarID WHERE V1.gmVarID IN (",
                         gmSQLValues(var,db=db), ")")
            dbGetQuery(db, sql)
          })

repairGmGrandChilds <- function(db=getOption("gmDB")) {
  gmRequest("repairGmiGrandChilds", parameters=list(), db=db)
}

repairGmiGrandChilds <- function(db) {
  # Server
  k = 0;
  res=dbSendQuery(db,"INSERT INTO gmGrandChilds (gmVarID,grandchild,generation) SELECT V.gmVarID,V.gmVarID,0 FROM gmGrandChilds C RIGHT OUTER JOIN gmVar V ON C.gmVarID=V.gmVarID AND C.grandchild=V.gmVarID AND generation=0 WHERE C.grandchild IS NULL");
  k=k+dbGetRowsAffected(res);
  dbClearResult(res);
  res=dbSendQuery(db,"INSERT INTO gmGrandChilds (gmVarID,grandchild,generation) SELECT V.gmVarID,V.child,1 FROM gmGrandChilds C RIGHT OUTER JOIN gmInherits V ON C.gmVarID=V.gmVarID AND C.grandchild=V.child AND generation=1 WHERE C.grandchild IS NULL");
  k=k+dbGetRowsAffected(res);
  dbClearResult(res);
  dbGetQuery(db,"UPDATE gmGrandChilds C LEFT OUTER JOIN (gmInherits V JOIN gmGrandChilds G ON V.child=G.gmVarID) ON C.gmVarID=V.gmVarID AND C.grandchild=G.grandchild AND C.generation=G.generation+1 SET C.generation=NULL WHERE G.gmVarID IS NULL AND C.generation>0");
  res=dbSendQuery(db,"DELETE FROM gmGrandChilds WHERE generation IS NULL");
  k=k+dbGetRowsAffected(res);
  dbClearResult(res);
  n=0;
  while( TRUE ) {
    res=dbSendQuery(db,"INSERT IGNORE INTO gmGrandChilds (gmVarID,grandchild,generation) SELECT V.gmVarID,G.grandchild,G.generation+1 FROM gmInherits V JOIN gmGrandChilds G ON V.child=G.gmVarID");
    ra=dbGetRowsAffected(res);
    dbClearResult(res);
    k=k+ra
    if( ra == 0 )
      break;
    nnew = dbGetQuery(db,"SELECT COUNT(*) AS n FROM gmGrandChilds")$n;
    if( nnew== n )
      break;
    n=nnew;
  }
  if( k > 0 )
    cat("repairGmGrandChild",k, " changes affected\n");
}

dbNow <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

gmGet <- function(expr,envir=parent.frame(),EXPR=substitute(expr),db=getOption("gmDB")) {
  if(FALSE) {
    . <- nofunc
  }


  unquote <- function(e) {
    if (length(e) <= 1L)
      e
    else if (e[[1L]] == as.name("."))
      eval(e[[2L]], envir)
    else if (is.pairlist(e))
      as.pairlist(lapply(e, unquote))
    else as.call(lapply(e, unquote))
  }
  EXPR <- unquote(EXPR)
  cm <- gmClassMembers(envir=envir, EXPR=EXPR, db=db)
  ex <- gmExpr(EXPR=EXPR)
  exInfo <- ex
  cmsl <- cm[cm$gmVarType != "set",]
  exInfo@EXPR <- as.call(c(list(as.name("["), ex@EXPR), as.list(cmsl$gmVarName)))
  assign("exInfo", exInfo, envir)
  info <- gmRead(.(exInfo), envir=envir, db=db)
  cms <- cm[cm$gmVarType == "set",]
  if (FALSE)
    delayedAssign("i", stop("gmGet: internal error"))
  sets <- foreach(i=1:nrow(cms)) %do%
  {
    exSet <- as.call(c(list(as.name("["), ex@EXPR), as.list(cms$gmVarName[i])))
    gmRead(exSet, db=db)
  }
  names(sets) <- cms$gmVarName

  return(list(info=info, sets=sets))

  #gmRequest("gmiGet", parameters=list(EXPR=EXPR, envir=envir, ...), db=db)
}


gmGetVar <- function(expr,what,EXPR=substitute(expr),envir=parent.frame(),db=getOption("gmDB"),unique=FALSE) {
  cm <- gmClassMembers(EXPR=EXPR,envir=envir,db=db)
  cm1 <- cm[cm$gmVarName==what,,drop=FALSE]
  if( unique && nrow(cm1) !=1 ) {
    if( nrow(cm1) == 0 ) {
      stop("Variable ",what," not found in ",paste(cm$gmVarName,collapse=","));
    } else {
      stop("Variable ",what," is not unique. Candidates: ",paste(cm1$gmVarID,cm1$definer,sep=":",collapse=","));
    }
  }
  cm1
}


gmiInsertWithID <- function(sql, db=getOption("gmDB")) {
  # server internal
  res = dbSendQuery(db,sql);
  dbClearResult(res)
  res = dbSendQuery(db,"SELECT LAST_INSERT_ID()")
  x = fetch(res)
  dbClearResult(res)
  x$LAST_INSERT_ID
}

gmAdd <- function(where, what, data, rg=getOption("defaultRightsGroup"), EXPR=substitute(where),
                  envir=parent.frame(), db=getOption("gmDB"), force=FALSE) {
  unquote <- function(e) {
    if (length(e) <= 1L)
      e
    else if (e[[1L]] == as.name("."))
      eval(e[[2L]], envir)
    else if (is.pairlist(e))
      as.pairlist(lapply(e, unquote))
    else as.call(lapply(e, unquote))
  }
  EXPR <- unquote(EXPR)

  data <- unquote(substitute(data))[-1]
  myid <- gmRequest("gmiAdd", parameters=list(EXPR=EXPR, what=what, data=data, rg=rg, force=force, envir=envir), db=db)
  return(myid)
}


gmiAdd <- function(EXPR, what, data, rg, force, envir, db) {
  if(FALSE) {
    userName <- nofunc
    . <- nofunc
    gmID <- nofunc
    id <- nofunc
  }

  exprID <- gmiFullRead(EXPR=EXPR, envir=envir, db=db)
  if (dim(exprID)[1] != 1 ) {
    stop("gmAdd: ",dim(exprID)[1]," objects found where to place the new one. Please specify.");
  }

  # rights managemnt: is the user allowed to add a new object?
  #userID <- gmiFullRead(gmExpr(root$user[userName==.(getOption("dbUser"))]))$gmID
  #rgID <- dbGetQuery(db,paste("SELECT gmRightsGroup AS rg, gmID FROM gmObject WHERE gmID IN (",gmSQLValues(exprID$gmID),");", sep=""))
  #tmp <- names(table(rgID$rg))
  #wuser <- gmiFullRead(gmExpr(root$rightsGroup[id=gmID, id %in% .(tmp), id=drop, user=.$writers$userGroup$user]))
  #wuser1 <- wuser[wuser$user==userID,]
  #idx <- rgID$rg==wuser1$gmID
  #idx[is.na(idx)] <- TRUE

  userID <- gmiFullRead(gmExpr(root$user[userName==.(getOption("dbUser"))]))$gmID
  rgID <- gmRequest("gmiRightsGroup", parameters=list(gmID=exprID$gmID), db=db)

  ### NA correction for no given rights group
  rgID$rg[is.na(rgID$rg)] <- -1

  tmp <- names(table(rgID$rg))
  if (!(length(tmp) == 1 && tmp == -1)) {
    wuser <- gmiFullRead(gmExpr(root$rightsGroup[id=gmID, id %in% .(tmp), id=drop, user=.$writers$userGroup$user]))

    ### deleting all empty entries
    wuser <- wuser[!is.na(wuser$user),]

    wuser1 <- wuser[wuser$user==userID,]
    idx1 <- is.element(rgID$rg, wuser1$gmID)

    ### if no rights group is given, anyone can access
    idx2 <- rgID$rg==-1
    idx <- idx1 | idx2

    exprID <- exprID[exprID$gmID %in% rgID[idx,]$gmID,, drop=FALSE]
  }

  if (dim(exprID)[1]  == 0)
    stop("You do not have permission to add objects here.")

  varID <- gmClass(var=gmGetVar(.(exprID$gmID),what=what,db=db,unique=TRUE)$gmVarID,db=db);
  stopifnot(varID$gmVarTypeID=="set")

  childs <- gmClassMembers(var=varID$gmVarID, db=db)
  if( !is.data.frame(data) ) {
    data <- as.data.frame(as.list(data))
  }

  #are there all required objects? (apart from gmCreationTime - we can do it on our own)
  required <- childs$gmVarName[childs$require==1 & childs$gmVarName!="gmCreationTime"]
  if (!all(required %in% names(data))) {
    if(isTRUE(force))
      warning("gmAdd: ",paste(required,collapse=", "), " are required but missing.")
    else stop("gmAdd: ",paste(required,collapse=", "), " are required but missing. Use force=TRUE to force addition.")
  }

  members <- names(data) %in% childs$gmVarName
  if( !all(members))
    stop("gmAdd: ",paste(names(data)[!members],collapse=", ")," are not in variable list (",paste(childs$gmVarName,collapse=","),")")

  n <- nrow(data)
  #if (rg == -1)
  #  stop("gmAdd: no rights goup given. Object cannot be created without a given rights group!")
  sql <- paste("INSERT INTO gmObject VALUES ",paste(rep("()",n),collapse=","))
  myid=gmiInsertWithID(db=db, sql)

  if ( rg != -1 )
    dbSendQuery(db, paste("UPDATE gmObject SET gmRightsGroup=",gmSQLValues(rg)," WHERE gmID=",myid,";",sep=""))

  refVals <- data.frame(gmID=exprID$gmID,gmVarID=varID$gmVarID, gmRefID=seq(myid,myid+n-1))
  sql <-paste("INSERT INTO gmRef (gmID,gmVarID,gmRefID) VALUES ", gmSQLValues(refVals, db=db), ";", sep="")
  dbSendQuery(db, sql)

  gmSet(where=.(myid), data=data, varID=varID, childs=childs, members=members, db=db)
  return(myid)
}

gmSet <- function(where, data, varID=NULL, childs=NULL, members=NULL, EXPR=substitute(where), envir=parent.frame(),
                  db=getOption("gmDB"), update=TRUE) {
  unquote <- function(e) {
    if (length(e) <= 1L)
      e
    else if (e[[1L]] == as.name("."))
      eval(e[[2L]], envir)
    else if (is.pairlist(e))
      as.pairlist(lapply(e, unquote))
    else as.call(lapply(e, unquote))
  }

  if (!is.data.frame(data))
    data <- unquote(substitute(data))[-1]
  EXPR <- unquote(EXPR)

  #parameters=list(EXPR=EXPR, data=data, envir=envir, update=update, ...)
  #if (!missing(varID))
  #{
  #  varID <- eval(varID, envir)
  #  parameters=c(parameters, varID=varID)
  #}
  #if (!missing(members))
  #{
  #  members <- eval(members, envir)
  #  parameters=c(parameters, members=members)
  #}
  #if (!missing(childs))
  #{
  #  childs <- eval(childs, envir)
  #  parameters=c(parameters, childs=childs)
  #}
  gmRequest("gmiSet", parameters=list(EXPR=EXPR, data=data, varID=varID, childs=childs, members=members, envir=envir, update=update), db=db)
}

gmiSet <- function(EXPR, data, varID, childs, members, envir, update, db) {
#  if (is.numeric(EXPR))
#    ct <- TRUE
#  else ct <- FALSE
  if(FALSE) {
    userName <- nofunc
    . <- nofunc
    gmID <- nofunc
    id <- nofunc
  }

  if (is.null(varID))
    ct <- FALSE
  else ct <- TRUE

  #if varID is not missing, then it should come from gmAdd and therefore where is an id
  if (is.null(varID))
  {
    #exprID <- gmClass(EXPR=EXPR,db=db)
    exprID <- gmiFullRead(EXPR=EXPR, envir=envir, db=db)
    if (dim(exprID)[1] != 1 ) {
      stop("gmAdd: ",dim(exprID)[1]," objects found where to place the new one. Please specify.");
    }
    #varID <- gmClass(var=gmGetVar(.(exprID$gmID),var,db=db,unique=TRUE)$gmVarID,db=db);
    stopifnot(exprID$gmVarTypeID=="set")
    varID <- exprID$gmVarID
    myID <- exprID$gmID
    #ct <- FALSE
    if( !is.data.frame(data) ) {
      data <- as.data.frame(as.list(data))
    }
  } else myID <- EXPR

  #userID <- gmiFullRead(gmExpr(root$user[userName==.(getOption("dbUser"))]))$gmID
  #rgID <- dbGetQuery(db,paste("SELECT gmRightsGroup AS rg, gmID FROM gmObject WHERE gmID IN (",gmSQLValues(myID),");", sep=""))
  #tmp <- names(table(rgID$rg))
  #wuser <- gmiFullRead(gmExpr(root$rightsGroup[id=gmID, id %in% .(tmp), id=drop, user=.$writers$userGroup$user]))
  #wuser1 <- wuser[wuser$user==userID,]
  #idx <- rgID$rg==wuser1$gmID
  #idx[is.na(idx)] <- TRUE
  userID <- gmiFullRead(gmExpr(root$user[userName==.(getOption("dbUser"))]))$gmID
  rgID <- gmRequest("gmiRightsGroup", parameters=list(gmID=myID), db=db)

  ### NA correction for no given rights group
  rgID$rg[is.na(rgID$rg)] <- -1

  tmp <- names(table(rgID$rg))
  if (!(length(tmp) == 1 && tmp == -1)) {
    wuser <- gmiFullRead(gmExpr(root$rightsGroup[id=gmID, id %in% .(tmp), id=drop, user=.$writers$userGroup$user]))

    ### deleting all empty entries
    wuser <- wuser[!is.na(wuser$user),]

    wuser1 <- wuser[wuser$user==userID,]
    idx1 <- is.element(rgID$rg, wuser1$gmID)

    ### if no rights group is given, anyone can access
    idx2 <- rgID$rg==-1
    idx <- idx1 | idx2

    myID <- myID[myID %in% rgID[idx,]$gmID, drop=FALSE]
  }

  if (length(myID)  == 0)
    stop("You do not have permission to add objects here.")

  if (is.null(childs))
    childs <- gmClassMembers(.(myID), db=db)
  if (is.null(members))
    members <- names(data) %in% childs$gmVarName
  if( !all(members))
    stop("gmAdd: ",paste(names(data)[!members],collapse=", "),
         " is/are not in variable list (",paste(childs$gmVarName,collapse=","),")")

  n <- nrow(data)

  #Adding the data stuff
  sql=paste("SELECT V.gmVarName, V.gmVarTypeID, V.gmVarID, T.gmTable FROM gmVar V ",
            "NATURAL JOIN gmVarType T WHERE gmVarName IN ",gmSQLValues(names(data),db=db), sep="")
  tables <- dbGetQuery(db,sql)

  for (i in 1:n) {
    for (j in 1:length(members)) {
      if (tables$gmVarTypeID[j]!="set")
      {
        #Is the value already set? Then update it. Otherwise insert it.
        sql <- paste("SELECT x FROM ", tables$gmTable[j], "WHERE gmID=", myID+i-1, " AND gmVarID=", tables$gmVarID[j], ";")
        res <- fetch(dbSendQuery(db, sql))
        if (nrow(res) == 0)
          sql <- paste("INSERT INTO ", tables$gmTable[j], " (gmID,gmVarID,x) VALUES (",
                       myID+i-1, ",", tables$gmVarID[j], ",\"", gmEscapeStrings(db=db, as.character(data[tables$gmVarName[j]][i,])), "\");", sep="")
        else sql <- paste("UPDATE ", tables$gmTable[j], " SET x=\"", gmEscapeStrings(db=db, as.character(data[tables$gmVarName[j]][i,])),
                          "\" WHERE gmID=", myID+i-1, " AND gmVarID=", tables$gmVarID[j], ";", sep="")
      } else {
        #abchecken ob das referenzierte Objekt wirklich in der Datenbank mit der Funktion vorhanden ist?
        #Objekt wieder l?schen, wenn Dinge fehlen oder nur ne Warning ausgeben?
        sql <- paste("SELECT * FROM gmRef WHERE gmID=", myID+i-1, " AND gmVarID=", tables$gmVarID[j], ";", sep="")
        res <- fetch(dbSendQuery(db, sql))

        if (nrow(res) == 0)
          sql <- paste("INSERT INTO gmRef (gmID,gmVarID,gmRefID) VALUES (",
                       myID+i-1, ",", tables$gmVarID[j], ",", data[tables$gmVarName[j]][i,], ");", sep="")
        else
        {
          if (update)
          {
            if (nrow(res) != 1)
              stop("Update object ambiguous.")
            else
              sql <- paste("UPDATE gmRef SET gmRefID=", data[tables$gmVarName[j]][i], " WHERE gmID=", myID+i-1,
                           " AND gmVarID=", tables$gmVarID[j], ";", sep="")
          }
          else
            sql <- paste("INSERT INTO gmRef (gmID, gmVarID, gmRefID) VALUES (", myID+i-1, ",", tables$gmVarID[j], ",",
                         data[tables$gmVarName[j]][i,],");", sep="")
        }
      }
      dbSendQuery(db, sql)
    }

    #gmCreationTime set automatically?
    if (!any("gmCreationTime"==names(data)) && isTRUE(ct))
    {
      sql <- "SELECT V.gmVarName, V.gmVarTypeID, V.gmVarID, T.gmTable FROM gmVar V
      NATURAL JOIN gmVarType T WHERE gmVarName='gmCreationTime';"
      varID <- fetch(dbSendQuery(db,sql))
      sql <- paste("SELECT DATE_FORMAT(gmCreateTime, '%Y-%m-%d %H:%i:%s') AS gmCreateTime FROM gmObject WHERE gmID=", myID+i-1,";", sep="")
      ctime <- fetch(dbSendQuery(db, sql))
      sql <- paste("INSERT INTO ", varID$gmTable, " (gmID,gmVarID,x) VALUES (",
                   myID+i-1, ",", varID$gmVarID, ",\"", ctime$gmCreateTime, "\");", sep="")
      dbSendQuery(db, sql)

    }

    if (!any("gmCreator"==names(data)) && isTRUE(ct)) {
      sql <- "SELECT V.gmVarName, V.gmVarTypeID, V.gmVarID, T.gmTable FROM gmVar V
      NATURAL JOIN gmVarType T WHERE gmVarName='gmCreator';"
      varID <- fetch(dbSendQuery(db, sql))
      sql <- paste("INSERT INTO gmRef (gmID, gmVarID, gmRefID) VALUES (", myID+i-1, ",", varID$gmVarID, ",", userID, ");", sep="")
      dbSendQuery(db, sql)
    }
  }
}




###
gmRemove <- function(expr, which=NULL, var=NULL, EXPR=substitute(expr), WHICH=substitute(which),
                     envir=parent.frame(),db=getOption("gmDB")) {
  unquote <- function(e) {
    if (length(e) <= 1L)
      e
    else if (e[[1L]] == as.name("."))
      eval(e[[2L]], envir)
    else if (is.pairlist(e))
      as.pairlist(lapply(e, unquote))
    else as.call(lapply(e, unquote))
  }
  EXPR <- unquote(EXPR)
  if (!is.null(which))
    WHICH <- unquote(WHICH)
  gmRequest(fun="gmiRemove", parameters=list(EXPR=EXPR, WHICH=WHICH, var=var, envir=envir), db=db)
}


gmiRemove <- function(EXPR, WHICH, var, envir, db) {
  if (FALSE)
  {
    . <- nofunc
    userName <- nofunc
    id <- nofunc
    gmID <- nofunc
  }
  if (!is.numeric(EXPR))
  {
    varID <- gmClass(EXPR=EXPR,db=db)
    if (dim(varID)[1] != 1 ) {
      stop("gmRemove: ",dim(varID)[1]," objects found to remove. Please specify.");
    }
  }
  else #exprID <- data.frame(gmID=expr)
    varID <- gmClass(EXPR=EXPR,db=db);

  # rights managemnt: is the user allowed to write the object?
  # userID <- gmiFullRead(gmExpr(root$user[userName==.(getOption("dbUser"))]))$gmID
  # rgID <- dbGetQuery(db,paste("SELECT gmRightsGroup AS rg, gmID FROM gmObject WHERE gmID IN (",gmSQLValues(varID$gmID),");", sep=""))
  # tmp <- names(table(rgID$rg))
  # wuser <- gmiFullRead(gmExpr(root$rightsGroup[id=gmID, id %in% .(tmp), id=drop, user=.$writers$userGroup$user]))
  # wuser1 <- wuser[wuser$user==userID,]
  # idx <- rgID$rg==wuser1$gmID
  # idx[is.na(idx)] <- TRUE
  # varID <- varID[varID$gmID==rgID[idx,]$gmID,, drop=FALSE]
  userID <- gmiFullRead(gmExpr(root$user[userName==.(getOption("dbUser"))]))$gmID
  rgID <- gmRequest("gmiRightsGroup", parameters=list(gmID=varID$gmID), db=db)

  ### NA correction for no given rights group
  rgID$rg[is.na(rgID$rg)] <- -1

  tmp <- names(table(rgID$rg))
  if (!(length(tmp) == 1 && tmp == -1)) {
    wuser <- gmiFullRead(gmExpr(root$rightsGroup[id=gmID, id %in% .(tmp), id=drop, user=.$writers$userGroup$user]))

    ### deleting all empty entries
    wuser <- wuser[!is.na(wuser$user),]

    wuser1 <- wuser[wuser$user==userID,]
    idx1 <- is.element(rgID$rg, wuser1$gmID)

    ### if no rights group is given, anyone can access
    idx2 <- rgID$rg==-1
    idx <- idx1 | idx2

    varID <- varID[varID$gmID %in% rgID[idx,]$gmID,, drop=FALSE]
  }

  if (dim(varID)[1]  == 0)
    stop("You do not have permission to remove any of these objects.")

  if (is.null(var))
  {
    if (varID$gmVarTypeID == "set")
    {
      sql <- paste("DELETE FROM gmObject WHERE gmID=", varID$gmID, ";", sep="")
      res <- dbSendQuery(db, sql)
    }
  }
  else
  {
    #myID <- exprID$gmID
    childs <- gmClassMembers(.(varID$gmID), db=db)

    if (!(var %in% childs$gmVarName))
      stop("There is no member called ", var, " for ", varID$gmVarName, ".")

    childsSel <- childs[childs$gmVarName == var,]
    if (childsSel$gmVarTypeID != "set")
    {
      sql <- paste("SELECT gmTable FROM gmVarType WHERE gmVarTypeID='", childsSel$gmVarTypeID, "';", sep="")
      res <- fetch(dbSendQuery(db, sql))
      sql <- paste("DELETE FROM ", res$gmTable, " WHERE gmID=", varID$gmID, " AND gmVarID=", childsSel$gmVarID, ";", sep="")
      res <- dbSendQuery(db, sql)
    }
    else
    {
      gmRefID <- gmRead(.(WHICH))
      if (dim(gmRefID)[1] == 0)
        stop("No object found for removal.")
      if (dim(gmRefID)[1] > 1)
        stop("Object for removal ambiguous. Please specify.")
      sql <- paste("DELETE FROM gmRef WHERE gmVarID=", childsSel$gmVarID, " AND gmRefID=", gmRefID$gmID,
                   " AND gmID=", varID$gmID, ";", sep="")
      res <- dbSendQuery(db, sql)
      # warning, if nothing was removed because of an missing entry?
    }
  }
}


###
gmCreateRightsGroup <- function(rightsGroup, envir=parent.frame(), db=getOption("gmDB")) {
  gmRequest("gmiCreateRightsGroup", parameters=list(rightsGroup=rightsGroup, envir=envir), db=db)
}

gmiCreateRightsGroup <- function(rightsGroup, envir, db) {
  if(FALSE) {
    userName <- nofunc
    . <- nofunc
  }

  userID <- gmiFullRead(gmExpr(root$user[userName==.(getOption("dbUser"))]))$gmID
  options(warn=-1)
  rgID <- gmAdd(root, "rightsGroup", list(gmName=.(rightsGroup)), force=TRUE)
  readID <- gmAdd(rgID, "readers", data=list(gmCreator=.(userID)), force=TRUE)
  writeID <- gmAdd(rgID, "writers", data=list(gmCreator=.(userID)), force=TRUE)
  options(warn=0)
  return(rgID)

}

###
gmCreateUserGroup <- function(userGroup, envir=parent.frame(), db=getOption("gmDB")) {
  gmRequest("gmiCreateUserGroup", parameters=list(userGroup=userGroup, envir=envir), db=db)
}

gmiCreateUserGroup <- function(userGroup, envir, db) {
  if(FALSE) {
    userName <- nofunc
    . <- nofunc
  }

  userID <- gmiFullRead(gmExpr(root$user[userName==.(getOption("dbUser"))]))$gmID
  options(warn=-1)
  ugID <- gmAdd(root, "userGroup", list(gmName=.(userGroup)), force=TRUE)
  return(ugID)
}


###
gmSetRights <- function(userGroup, rightsGroup, write=TRUE, read=TRUE, envir=parent.frame(), db=getOption("gmDB")) {
  gmRequest("gmiSetRights", parameters=list(userGroup=userGroup, rightsGroup=rightsGroup, write=write, read=read, envir=envir), db=db)
}

gmiSetRights <- function(userGroup, rightsGroup, write, read, envir, db) {
  if(FALSE) {
    gmName <- nofunc
    . <- nofunc
  }

  if (is.character(userGroup))
    ugID <- gmRead(root$userGroup[gmName==.(userGroup)])$gmID
  else ugID <- userGroup
  if(!is.numeric(ugID) || length(ugID) == 0) stop("userGroup must be an ID or name of an existing usergroup")

  if (is.character(rightsGroup))
    rgID <- gmRead(root$rightsGroup[gmName==.(rightsGroup)])$gmID
  else rgID <- rightsGroup
  if (!is.numeric(rgID) || length(rgID) == 0) stop("rightsGroup must be an ID or name of an existing rights group")

  if (write)
    gmSet(rgID$writers, data.frame(userGroup=ugID))
  if(read)
    gmSet(rgID$readers, data.frame(userGroup=ugID))
}

###
gmAddUserToGroup <- function(user, userGroup, envir=parent.frame(), db=getOption("gmDB")) {
  gmRequest("gmiAddUserToGroup", parameters=list(user=user, userGroup=userGroup, envir=envir), db=db)
}

gmiAddUserToGroup <- function(user, userGroup, envir, db) {
  if(FALSE)
  {
    userName <- nofunc
    userGroup <- nofunc
    . <- nofunc
    gmName <- nofunc
  }

  if (is.character(user))
    userID <- gmRead(root$user[userName==.(user)])$gmID
  else userID <- user
  if (!is.numeric(userID)) stop("user must be an ID or valid username")

  if (is.character(userGroup))
    ugID <- gmRead(root$userGroup[gmName==.(userGroup)])$gmID
  else ugID <- userGroup
  if (!is.numeric(ugID)) stop("userGroup must be an ID or name of an existing usergroup")

  gmSet(ugID, data=data.frame(user=userID))
}


###
gmSetRightsGroup <- function(object, rightsGroup, envir=parent.frame(), db=getOption("gmDB")) {
  gmRequest("gmiSetRightsGroup", parameters=list(object=object, rightsGroup=rightsGroup, envir=envir), db=db)
}

gmiSetRightsGroup <- function(object, rightsGroup, envir, db) {
  if(FALSE)
  {
    . <- nofunc
    gmName <- nofunc
  }
  if (is.character(rightsGroup))
    rgID <- gmRead(root$rightsGroup[gmName==.(rightsGroup)])$gmID
  else rgID <- rightsGroup
  if (!is.numeric(rgID)) stop("rightsGroup must be an ID or a name of an existing rights group")
  if (length(rgID) == 0) stop("Rights group not found. Maybe you do not have sufficient access rights.")

  object <- as.vector(object)
  for (i in 1:length(object)) {
    objID <- gmRead(.(object[i]))
    if (length(objID) == 0) stop("No object found. Maybe you do not have the rights to change the rights group.")

    sql <- paste("UPDATE gmObject SET gmRightsGroup=", rgID, " WHERE gmID=", objID, ";", sep="")
    res <- dbSendQuery(db,sql)
    dbClearResult(res)
  }

}


###
gmCreateUser <- function(user, password, userGroups, envir=parent.frame(), db=getOption("gmDB")) {
  if (missing(user) | missing(password)) {
    dbuser <- runApp(list(ui = fluidPage(
      column(3,
             wellPanel(renderText("Enter new user credentials:"),
                       textInput(inputId = "login", label = "Login", width=150),
                       passwordInput(inputId = "pwd", label = "Password", width=200),
                       actionButton(inputId = "OK", label ="OK")))
    ),
    server = function(input, output)
    {
      observeEvent(input$OK, { dbuser <- (list(login=input$login, password=input$pwd))
      stopApp(dbuser)})
    }))

    user <- dbuser$login
    password <- dbuser$password
  }

  password <- digest(password, "sha256", serialize=FALSE)

  gmRequest("gmiCreateUser", parameters=list(user=user, password=password, userGroups=userGroups, envir=envir), db=db)
}

gmiCreateUser <- function(user, password, userGroups, envir, db) {
  if (FALSE)
  {
    . <- nofunc
    gmName <- nofunc
  }

  userID <- gmAdd(root, "user", data=data.frame(userName=.(user), password=.(password)))

  userGroups <- as.vector(userGroups)
  for (i in 1:length(userGroups))
  {
    ugID <- gmRead(root$userGroup[gmName==.(userGroups[i])])$gmID
    gmSet(ugID, data.frame(user=userID), update = FALSE)
  }
}


###
### set the default rightsGroup
gmDefaultRightsGroup <- function(rightsGroup, db=getOption("gmDB")) {
  if (FALSE)
  {
    gmName <- nofunc
    . <- nofunc
  }
  if (is.character(rightsGroup))
    rgID <- gmiFullRead(gmExpr(root$rightsGroup[gmName==.(rightsGroup)]))$gmID
  else rgID <- rightsGroup
  if (!is.numeric(rgID)) stop("rightsGroup must be an ID or name of an existing rights group")

  if (length(rgID) == 0) stop("Cannot set the default rights group. Maybe you don not have sufficient access rights.")

  options(defaultRightsGroup=rgID)
}


### decide where to execute the functions: gmRequest
setGeneric("gmRequest", function(fun, parameters, db) {standardGeneric("gmRequest")
})

setMethod("gmRequest", signature = c(fun="character", parameters="list", db="MySQLConnection"),
          definition = function(fun, parameters, db) {
            if( ! fun %in% c("gmiInitRead", "gmiRead","gmiClass", "gmiClassMembers", "repairGmiGrandChilds",
                             "gmiAdd", "gmiSet", "gmiRemove","gmiClassVar", "gmiClassMembersVar", "gmiRightsGroup",
                             "gmiCreateRightsGroup", "gmiCreateUserGroup", "gmiSetRights", "gmiAddUserToGroup",
                             "gmiSetRightsGroup", "gmiCreateUser", "gmiCreateClass", "gmiAddMembers")) {
              stop("Cannot process request ...")
            }

            do.call(fun, c(parameters, list(db=db)),quote=TRUE)
          }
)

setMethod("gmRequest", signature = c(fun="character", parameters="list", db="ANY"),
          definition = function(fun, parameters, db) {
            # if ( class(db)[1] != "sockconn")
            if ( !is(db, "sockconn"))
              stop("Error: No socket or MySQL connection provided.")
            if( ! fun %in% c("gmiInitRead","gmiRead","gmiClass", "gmiClassMembers", "repairGmiGrandChilds",
                             "gmiAdd", "gmiSet", "gmiRemove", "gmiClassVar", "gmiClassMembersVar", "gmiRightsGroup",
                             "gmiCreateRightsGroup", "gmiCreateUserGroup", "gmiSetRights", "gmiAddUserToGroup",
                             "gmiSetRightsGroup", "gmiCreateUser", "gmiCreateClass", "gmiAddMembers")) {
              stop("Can not process request ...")
            }
            #serialize(list(type="gmRequest", fun=fun, parameters=parameters),db)
            saveRDS(list(type="gmRequest", fun=fun, parameters=parameters),db)
            #print("send")
            readRDS(db)
          }
)
