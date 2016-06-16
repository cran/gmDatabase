options(gmDB=NULL)
options(dbUser=NULL)
options(defaultRightsGroup=-1)

gmConnectDB <- function(...,asDefault=TRUE) {
  # Open the connection   
  obj <- dbConnect(...)
  if( ! dbExistsTable(obj,"gmObject") ) {
      stop("Geometallurgy Database structure is not initialized on this database. Use gmCreateDB")
    } 
  stopifnot(dbExistsTable(obj,"gmObject"))
  stopifnot(dbExistsTable(obj,"gmVarType"))
  stopifnot(dbExistsTable(obj,"gmVar"))
  #stopifnot(dbExistsTable("gmName"))
  if(asDefault)
    options(gmDB=obj)
  obj
}

gmConnectServer <- function(..., server="localhost", dbuser, asDefault=TRUE) {
  if (FALSE) {
  userName <- nofunc
  password <- nofunc
  . <- nofunc
  }
  
  args <- list(...)
  namArgs <- names(args)
  
  if ((!is.element("user", namArgs) || !is.element("host", namArgs) || !is.element("password", namArgs) || !is.element("dbname", namArgs)) 
      && !is.element("group", namArgs) ) {
    if (is.element("user", namArgs))
      mysqlUser <- args[["user"]]
    else mysqlUser <- "mysqlUser"
    
    if (is.element("host", namArgs))
      mysqlHost <- args[["host"]]
    else mysqlHost <- "mysqlServer"
    
    if (is.element("dbname", namArgs))
      dbname <- args[["dbname"]]
    else dbname <- "dbname"
    
    sqluser <- runApp(list(ui = fluidPage(
      titlePanel("MySQL login information"),
      column(3,
             wellPanel(textInput(inputId = "login", label = "MySQL login", mysqlUser, width=150),
                       passwordInput(inputId = "pwd", label = "MySQL password", width=200),
                       textInput(inputId = "host" , label = "MySQL server", mysqlHost, width=200),
                       textInput(inputId = "dbname", label ="Database", dbname, width=150),
                       actionButton(inputId = "OK", label ="OK")))
    ), 
    server = function(input, output) 
    {
      observeEvent(input$OK, { dbuser <- (list(login=input$login, password=input$pwd, host=input$host, dbname=input$dbname))
      stopApp(dbuser)})
    }))
    user <- sqluser$login
    pwd <- sqluser$password 
    host <- sqluser$host
    dbname <- sqluser$dbname
  }
  else 
    if (!is.element("group", namArgs)) {
    user = args$user
    pwd <- args$password
    host <- args$host
    dbname <- args$dbname
  }
  
  
  
  if (missing(dbuser)) 
    dbuser <- runApp(list(ui = fluidPage(
      titlePanel("Login information"),
      column(3,
             wellPanel(textInput(inputId = "login", label = "Login", width=150),
                       passwordInput(inputId = "pwd", label = "Password", width=200),
                       actionButton(inputId = "OK", label ="OK")))
    ), 
    server = function(input, output) 
    {
      observeEvent(input$OK, { dbuser <- (list(login=input$login, password=input$pwd))
      stopApp(dbuser)})
    }))
  if (server != "localhost")
  {
    obj <- socketConnection(server, port=149000, timeout=400, blocking=TRUE, open="a+b")
    if (is.element("group", namArgs))
      saveRDS(list(type="dblogin", drv=args[[1]], group=args$group, dbuser=dbuser), obj)
    else saveRDS(list(type="dblogin", drv=args[[1]], user=user, password=pwd, host=host, dbname=dbname, dbuser=dbuser), obj)
    tmp <- readRDS(obj)
    if (tmp != "Connection established.")
    {
      close(obj)
      stop("Access denied. Try again.")
    }
      
    else print("Connection established.")
  }
  else {
    if (is.element("group",namArgs))
      obj <- gmConnectDB(...)
    else obj <- gmConnectDB(drv=args[[1]], user=user, password=pwd, host=host, dbname=dbname, asDefault=asDefault)
    res1 <- gmiFullRead(gmExpr(root$user[userName==.(dbuser$login), password]))
    if (length(res1) == 0) {
      dbDisconnect(obj)
      warning("User not found. Try again.")
      return(NULL)
    }
    else {
      if (digest(dbuser$password, "sha256", serialize=FALSE) == res1$password) {
        print("Login successful.")
      }
      else {
        dbDisconnect(obj)
        warning("Access denied. Try again.")
        return(NULL)
      }
    }   
  }
    
  
  options(dbUser=dbuser$login)
  options(defaultRightsGroup=-1)
  
  if(asDefault)
    options(gmDB=obj)
  invisible(obj)
}

gmDisconnectServer <- function(db=getOption("gmDB")) {
  if ( identical(db,"gmDB") )
    options(gmDB=NULL)
  options(dbUser=NULL)
  if (class(db)[1] == "MySQLConnection")
    dbDisconnect(db)
  else {
    #serialize(list(type="term"), db)
    saveRDS(list(type="term"), db)
    #tmp <- unserialize(db)
    tmp <- readRDS(db)
    if (tmp) { 
      close(db)
      print("Connection closed.")
    }
    else print("Problem occured during disconnecting.")
  }
}

## gmCreateDB
gmCreateDB <- function(..., dbName="gmDatabase", admin=NULL, adminPwd=NULL) {
  #stop("Execute the file gmDatabase.sql to your server")
  
  ## utility function for db construction: assembles sql statements from the given characters
  concat <- function(obj)
  {
    l <- length(obj)
    start <- 1
    tmp <- character()
    res <- character()
    for (i in 1:l)
    {
      if (obj[i] == "")
      {
        for (j in start:i)
          tmp <- paste(tmp, obj[j], sep=" ")
        start <- i+1
        res <- c(res, paste(tmp, ";", sep=""))
        tmp <- character()
      }
      if (substring(obj[i],1,1) == "-")
        start <- i+1
    }
    res
  }
  
  args <- list(...)
  namArgs <- names(args)
  
  #if ((missing(user) || missing(host) || missing(password)) && missing(group)) {
  if ((!is.element("user", namArgs) || !is.element("host", namArgs) || !is.element("password", namArgs)) && !is.element("group", namArgs) ) {
    if (is.element("user", namArgs))
      mysqlUser <- args[["user"]]
    else mysqlUser <- "mysqlUser"
    
    if (is.element("host", namArgs))
      mysqlHost <- args[["host"]]
    else mysqlHost <- "mysqlServer"
    
    dbuser <- runApp(list(ui = fluidPage(
      titlePanel("MySQL login information"),
      column(3,
             wellPanel(textInput(inputId = "login", label = "MySQL login", mysqlUser, width=150),
                       passwordInput(inputId = "pwd", label = "MySQL password", width=200),
                       textInput(inputId = "host" , label = "MySQL server", mysqlHost, width=200),
                       actionButton(inputId = "OK", label ="OK")))
    ), 
    server = function(input, output) 
    {
      observeEvent(input$OK, { dbuser <- (list(login=input$login, password=input$pwd, host=input$host))
      stopApp(dbuser)})
    }))
    user <- dbuser$login
    pwd <- dbuser$password 
    host <- dbuser$host
    
    db <- dbConnect(..., user=user, password=pwd, host=host)
  }
  else 
    if (is.element("group", namArgs))
      db <- dbConnect(...)
    else db <- dbConnect(...)
  
  if (is.null(admin) | is.null(adminPwd))
  {
    dbuser <- runApp(list(ui = fluidPage(
      titlePanel("Admin login information"),
      column(3,
             wellPanel(textInput(inputId = "login", label = "Admin login", width=150),
                       passwordInput(inputId = "pwd", label = "Admin password", width=200),
                       actionButton(inputId = "OK", label ="OK")))
    ), 
    server = function(input, output) 
    {
      observeEvent(input$OK, { dbuser <- (list(login=input$login, password=input$pwd))
      stopApp(dbuser)})
    }))
    admin <- dbuser$login
    adminPwd <- dbuser$password 
  }
  
  packagePath <- find.package("gmDatabase", lib.loc=NULL, quiet=TRUE)
  tmp <- scan(file=paste(packagePath,"/gmDatabase.sql", sep=""), what=character(), sep=";", blank.lines.skip=TRUE, quiet=TRUE)
  tmp <- concat(tmp)
  
  if (dbName!="gmDatabase")
  {
    tmp[4] <- paste("CREATE SCHEMA IF NOT EXISTS `", dbName, "` DEFAULT CHARACTER SET latin1 ;", sep="")
    tmp[5] <- paste("USE `", dbName,"`;", sep="")
  }
  
  for (i in 1:length(tmp))
  {
    rs <- dbSendQuery(db, tmp[i])
    dbClearResult(rs)
  }
  
  sql <- paste("USE ", dbName, ";", sep="")
  dbSendQuery(db, sql)
  
  tmp <- scan(file=paste(packagePath,"/startDB.sql", sep=""), what=character(), sep=";", blank.lines.skip=TRUE, quiet=TRUE, quote=NULL)
  tmp <- concat(tmp)
  
  for (i in 1:length(tmp))
  {
    rs <- dbSendQuery(db, tmp[i])
    dbClearResult(rs)
  }
  
  sql <- paste("INSERT INTO gmObject (gmID) VALUES (2);")
  rs <- dbSendQuery(db, sql)
  dbClearResult(rs)
  
  sql <- paste("INSERT INTO gmRef (gmID,gmVarID,gmRefID) VALUES (1,2,2);")
  rs <- dbSendQuery(db, sql)
  dbClearResult(rs)

  sql <- paste("INSERT INTO gmString (gmID,gmVarID,x) VALUES (2,4,'", admin, "'), 
               (2,5,'", digest(adminPwd, "sha256", serialize=FALSE),"');", sep="")
  rs <- dbSendQuery(db, sql)
  dbClearResult(rs)
  
  repairGmGrandChilds(db)
  
  dbDisconnect(db)
}

gmListVariables <- function(pattern="%",db=getOption("gmDB")) {
  sql <- paste("SELECT * FROM gmVar WHERE gmVarName LIKE \"",gmEscapeStrings(pattern),"\"",sep="")
  dbGetQuery(db,sql)
}

gmListVariableTypes <- function(db=getOption("gmDB")) {
  dbGetQuery(db,"SELECT * FROM gmVarType")
}

gmDBSummary <- function(db=getOption("gmDB")) {
  dbGetQuery(db,"(SELECT gmVarName as VarName, gmVarTypeID as VarTypeID, COUNT(gmVarName) as N FROM
   (SELECT * FROM gmRef NATURAL JOIN gmVar GROUP BY gmVarID,gmRefID) AS refs GROUP BY gmVarID,gmVarName ORDER BY gmVarName)
  UNION (SELECT gmVarName as VarName, gmVarTypeID as VarTypeID, COUNT(gmVarName) as N FROM gmString NATURAL JOIN gmVar GROUP BY gmVarName ORDER BY gmVarName)
  UNION (SELECT gmVarName as VarName, gmVarTypeID as VarTypeID, COUNT(gmVarName) as N FROM gmInteger NATURAL JOIN gmVar GROUP BY gmVarName ORDER BY gmVarName)
  UNION (SELECT gmVarName as VarName, gmVarTypeID as VarTypeID, COUNT(gmVarName) as N FROM gmBlob NATURAL JOIN gmVar GROUP BY gmVarName ORDER BY gmVarName) 
  UNION (SELECT gmVarName as VarName, gmVarTypeID as VarTypeID, COUNT(gmVarName) as N FROM gmText NATURAL JOIN gmVar GROUP BY gmVarName ORDER BY gmVarName)
  UNION (SELECT gmVarName as VarName, gmVarTypeID as VarTypeID, COUNT(gmVarName) as N FROM gmNumeric NATURAL JOIN gmVar GROUP BY gmVarName ORDER BY gmVarName)")
}

gmCreateVariable <- function(name, type, description, inferredBy=NULL, db=getOption("gmDB")) {
  typeNames <- gmListVariableTypes()
  sapply(name, function(x) if (is.element(x,typeNames$gmVarName)) stop(x, " is already a variable in the database. Please use a different name."))
  type <- as.character(sapply(type,function(x) match.arg(x,typeNames$gmVarTypeID)))
  name <- as.character(name)
  stopifnot(length(name)==length(type))
  
  if (!is.null(inferredBy)) {
    inferredBy <- as.character(sapply(inferredBy, function(x) match.arg(x,typeNames$gmVarName)))
    stopifnot(length(name)==length(inferredBy))
  }
  
  if (is.element("set",type) && is.null(inferredBy))
    stop("Variables of type set have to by inferred from at least gmObject")

  
  sql <- paste("INSERT INTO gmVar (gmVarName,gmVarTypeID,gmVarDescription) VALUES",
               gmSQLValues(data.frame(name,type,description)))
  dbGetQuery(db,sql)
  
  sql <- paste("SELECT * FROM gmVar WHERE gmVarName IN ", gmSQLValues(c(name, inferredBy)), ";")
  ids <- dbGetQuery(db,sql)
  for (i in 1:length(name))
    if (!is.null(inferredBy[i]))
        dbSendQuery(db, paste("INSERT INTO gmInherits (gmVarID, child, remark) VALUES ", 
                          gmSQLValues(c(ids[ids$gmVarName==inferredBy[i],]$gmVarID, 
                                        ids[ids$gmVarName==name[i],]$gmVarID, paste(name[i],"extends",inferredBy[i]))), ";", sep=""))
}

gmCreateClass <- function(name, type, description, parent="gmObject", envir=parent.frame(), db=getOption("gmDB")) {
  gmRequest("gmiCreateClass", parameters=list(name=name, type=type, description=description, parent=parent, envir=envir), db=db)
}

gmiCreateClass <- function(name, type, description, parent, envir, db) {
  if (length(name) > 1) stop("I am sorry. You can only create one class at once.")
  typeNames <- gmListVariableTypes()
  if (is.element(name,typeNames$gmVarName)) 
    stop(name, " is already a class or variable in the database. Please use a different name.")
  type <- as.character(match.arg(type,typeNames$gmVarTypeID))
  name <- as.character(name)
  stopifnot(length(name)==length(type))
  
  sql <- paste("INSERT INTO gmVar (gmVarName,gmVarTypeID,gmVarDescription) VALUES",
               gmSQLValues(data.frame(name,type,description)))
  dbGetQuery(db,sql)
  
  sql <- paste("SELECT * FROM gmVar WHERE gmVarName IN ", gmSQLValues(c(name, parent)), ";")
  ids <- dbGetQuery(db,sql)
  if (type == "set")
    dbSendQuery(db, paste("INSERT INTO gmInherits (gmVarID, child, remark) VALUES ", 
                          gmSQLValues(c(ids[ids$gmVarName==parent,]$gmVarID, 
                                        ids[ids$gmVarName==name,]$gmVarID, paste(name,"extends",parent))), ";", sep=""))
  invisible(repairGmGrandChilds())
}


###
gmAddMembers <- function(class, members, required, envir=parent.frame(), db=getOption("gmDB")) {
  gmRequest("gmiAddMembers", parameters=list(class=class, members=members, required=required, envir=envir), db=db)
}

gmiAddMembers <- function(class, members, required, envir, db) {
  varNames <- gmListVariables()
  stopifnot(length(members) == length(required))
  member1 <- as.character(sapply(members, function(x) match.arg(x, varNames$gmVarName)))
  stopifnot(length(members) == length(member1))
  
  required = sapply(required, function(x) if (x) x=1 else x=0)
  sql <- paste ("INSERT INTO gmElements (gmVarID, member, required, remark) VALUES ", 
               gmSQLValues(data.frame(rep(varNames[varNames$gmVarName==class,]$gmVarID,length(members)), 
                                       varNames[apply(sapply(members, function(x) varNames$gmVarName==x),1,any),]$gmVarID, 
                                       required, 
                                       sapply(members, function(x) paste(class,".",x,sep="")))))
  dbSendQuery(db, sql)
  repairGmGrandChilds()
}


gmChangePassword <- function (db=getOption("gmDB")) {
  shinyApp(ui = ui, server = server)
}

ui <- fluidPage(
  column(4,
         wellPanel(
           textInput(inputId = "login", label = "Login", width=150),
           passwordInput(inputId = "oldPwd", label = "old password", width=200),
           passwordInput(inputId = "newPwd", label = "new password", width=200),
           passwordInput(inputId = "renewPwd", label = "retype new password", width=200),
           actionButton(inputId = "OK", label ="OK"),
           textOutput(outputId = "out"), width=250
         )
  )
  
)

server <- function(input, output) {
  if(FALSE) {
    gmName <- nofunc
    . <- nofunc
    userName <- nofunc
  }
  envir = parent.frame()
  unquote <- function(e) {
    if (length(e) <= 1L) 
      e
    else if (e[[1L]] == as.name(".")) 
      eval(e[[2L]], envir)
    else if (is.pairlist(e)) 
      as.pairlist(lapply(e, unquote))
    else as.call(lapply(e, unquote))
  }

  observeEvent(input$OK, {
    output$out <- renderText("")
    if (input$newPwd == "" | input$renewPwd == "")
      output$out <- renderText("Please fill in all fields!")
    else {
      if (input$newPwd != input$renewPwd)
        output$out <- renderText("The new passwords do not match.")
      else {
        error <- try(tmp <- gmGet(root$user[userName==.(input$login)]), TRUE)
        if (class(error) != "try-error")
        {
          if (digest(input$oldPwd, "sha256", serialize=FALSE) == tmp$info$password) {
            pwd <- digest(input$newPwd, "sha256", serialize = FALSE)
            data <- data.frame(password=unquote(substitute(.(pwd))))
            gmSet(root$user[userName==.(input$login)], data=data)
            output$out <- renderText("Password changed.")
          }
          else output$out <- renderText("Wrong password.")
        } else output$out <- renderText("User not found.")
        #output$out <- renderText("Password changed.")
      }
    }
    })
}

