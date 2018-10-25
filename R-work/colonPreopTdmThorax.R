#remove workspace EXCEPT input from nodeJS r-script
rm(list = ls()[which(ls()!="input")])

library(needs)
needs(DBI)
needs(RMySQL)
needs(jsonlite)

# close and open connection to mysql database
lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
mydb = dbConnect(MySQL(), user='root', password='abcd1234!', host='localhost')

dbExecute(mydb, "use mssDB")

##??remove the below
startDate = '"2018-01-01"'
endDate = '"2019-01-01"'
formType = '"E"'
userLevel = 3
userId = 2

# startDate=paste('"',input[[1]],'"',sep="")
# endDate=paste('"',input[[2]],'"',sep="")
# formType = paste('"',input[[3]],'"',sep="")
# userLevel = input[[4]]
# userId = input[[5]]

sqlQuery=paste("select * from formulaire_item fi
               join formulaire f on f.id = fi.id_formulaire
               join patient p on p.id = f.id_patient
               join organe o on o.id = f.id_organe
               join item i on fi.id_item = i.id
               join medecin m on fi.valeur_item =m.id
               and i.intitule= 'Opérateur1'
               join service s on s.id = m.id_service
               join utilisateur u on u.doctorCode = m.doctorCode
               where f.date_creation BETWEEN",startDate,"AND",endDate,
               "AND","o.code=",formType,
               sep=" ")
#  join medecin m on fi.valeur_item =m.id  and i.intitule= 'Opérateur1' join service s on s.id = m.id_service ?? need to add this back in and and s.id='3'
df=dbGetQuery(mydb,sqlQuery)
total=length(unique(df$id_patient))

df2=df[which(df$id_item=='34'&df$valeur_item=='1'),]
adherent=length(unique(df2$id_patient))

df3=df[which(df$id_item=='34'&df$valeur_item=='0'),]
nonadherent=length(unique(df3$id_patient))

df3=df[which(df$id_item=='34'&df$valeur_item=='0'),]
nonadherent=length(unique(df3$id_patient))

df4=df[which(df$id_item=='34'&df$valeur_item=='999'),]
missing=length(unique(df4$id_patient))

result = data.frame(adherent, nonadherent, total, missing)

#result = toJSON(result,auto_unbox = FASL)
#temp =colnames(result)
#result = reshape(result, direction="long",varying=temp,timevar="type",times=temp,v.names="count")
#keeps = c("type","count")
#result=result[keeps]
# 
#names(result) = NULL
#result=as.list(result)

# 
# result = toJSON(result)
# # result=gsub("\\[","",result)
# # result=gsub("\\]","",result)
# 
# resultString = toString(result)
# #
# 
# print(resultString)
# 
# # resultString = toString(result)
# resultString=gsub("\\[","",resultString)
# resultString=gsub("\\]","",resultString)
# # resultString=gsub('\\"\\{','\{',resultString)
# # resultString=gsub('\"', '"', resultString, fixed = FALSE)
# # resultString=gsub("\\","",resultString,fixed=T)
# # resultString=gsub('\\\"','test',resultString)
# # resultString=gsub('test','"',resultString,fixed=TRUE)
# cat(resultString,"\n")
# # test=gsub('\"', '\"', resultString, fixed = FALSE)
# 
