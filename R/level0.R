dbDataSet <- function(...) {
  

}

dbCreateGMDatabase <- function(con) {
sql<-
"
CREATE TABLE IF NOT EXISTS gmObjects (
   gmID CHAR(24) PRIMARY KEY,
   gmCreateTime TIMESTAMP,
   gmObjectDescription TEXT
);
"
res<-dbSendQuery(con,sql)
}
