# install.packages("RJDBC")
# library(RJDBC)
# ordrv7 <- JDBC("oracle.jdbc.OracleDriver","/Library/Java/Extensions/ojdbc7.jar")
# 17102018 funnel test
install.packages("RMariaDB")
#n
#install.packages("RMySQL") they are phasing out RMySQL
#install.packages("devtools")
#install.packages("DBI")
#install.packages("odbc")

#test
# library(DBI)
# # Connect to my-db as defined in ~/.my.cnf
# con <- dbConnect(RMariaDB::MariaDB(), group = "my-db")
# 
# dbListTables(con)
# dbWriteTable(con, "mtcars", mtcars)
# dbListTables(con)
# 
# dbListFields(con, "mtcars")
# dbReadTable(con, "mtcars")
# 
# # You can fetch all results:
# res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
# dbFetch(res)
# dbClearResult(res)
# 
# # Or a chunk at a time
# res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
# while(!dbHasCompleted(res)){
#   chunk <- dbFetch(res, n = 5)
#   print(nrow(chunk))
# }
# # Clear the result
# dbClearResult(res)
# 
# # Disconnect from the database
# dbDisconnect(con)
# 
# 
# 
# 

library("RMariaDB");

mydb = dbConnect(MySQL(), user='root', password='abcd1234!', host='localhost')

dbDisconnect(mydb)


install.packages("RMySQL")
library(RMySQL)
mydb = dbConnect(MySQL(), user='root', password='abcd1234!', host='localhost')






install.packages("rjson")

###Funnel Plot###
install.packages("funnelR")
library(funnelR)

my_data  <- data.frame(id=c(1,2,3,4,5,6,7,8,9,10),
                       sex=c('M','F','M','F','F','M','F','M','F','M'), 
                       n=c(130,65,155,125,19,185,82,77,50,80), 
                       d=c(150,200,300,250,50,220,100,90,400,425)
)
knitr::kable(my_data)

my_limits   <- fundata(input=my_data, 
                        benchmark=0.50, 
                        alpha=0.95, 
                        alpha2=0.98, 
                        method='exact', 
                        step=1)

my_plot     <- funplot(input=my_data, 
                       fundata=my_limits)

my_plot

#manipulation of test data for POC
library(rjson)
#scatter
x$rate = x$n/x$d
keeps = c("rate","d")
x = x[keeps]
x2 <- toJSON(unname(split(x, 1:nrow(x))))
cat(x2)

#average
keeps=c("d","benchmark")
x=my_limits[keeps]
colnames(x)=c("x","y")
x2 = toJSON(unname(split(x, 1:nrow(x))))
cat(x2)

#average
keeps=c("d","benchmark")
x=my_limits[keeps]
colnames(x)=c("x","y")
x2 = toJSON(unname(split(x, 1:nrow(x))))
cat(x2)

#80 percentile ul
keeps=c("d","up")
x=my_limits[keeps]
colnames(x)=c("x","y")
x2 = toJSON(unname(split(x, 1:nrow(x))))
cat(x2)

#80 percentile ll
keeps=c("d","lo")
x=my_limits[keeps]
colnames(x)=c("x","y")
x2 = toJSON(unname(split(x, 1:nrow(x))))
cat(x2)

#95 percentile up
keeps=c("d","lo")
x=my_limits[keeps]
colnames(x)=c("x","y")
x2 = toJSON(unname(split(x, 1:nrow(x))))
cat(x2)

#95 percentile ll
keeps=c("d","lo")
x=my_limits[keeps]
colnames(x)=c("x","y")
x2 = toJSON(unname(split(x, 1:nrow(x))))
cat(x2)




