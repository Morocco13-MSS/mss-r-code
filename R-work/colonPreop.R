library(needs)
needs(DBI)
needs(RMySQL)
needs(jsonlite)

# close and open connection to mysql database
lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
mydb = dbConnect(MySQL(), user='root', password='abcd1234!', host='localhost')

dbExecute(mydb, "use mssDB")

# c=as.data.frame(dbReadTable(mydb,"commentaire"))
# print(c$id[1])
# cr=as.data.frame(dbReadTable(mydb,"commentaire_reponse"))
# f=as.data.frame(dbReadTable(mydb,"formulaire"))
# fh=as.data.frame(dbReadTable(mydb,"formulaire_historique"))
# i=as.data.frame(dbReadTable(mydb,"item"))
# m=as.data.frame(dbReadTable(mydb,"medecin"))
# pj=as.data.frame(dbReadTable(mydb,"pj_item"))
# r=as.data.frame(dbReadTable(mydb,"role"))
# s=as.data.frame(dbReadTable(mydb,"service"))
# u=as.data.frame(dbReadTable(mydb,"utilisateur"))

# 
result=data.frame()
result$count[0]=1

result$count[0,1]=dbGetQuery(mydb,"select count(distinct p.id) as count from formulaire_item fi  join formulaire f on f.id = fi.id_formulaire join patient p on p.id = f.id_patient join organe o on o.id = f.id_organe join item i on fi.id_item = i.id join medecin m on fi.valeur_item =m.id and i.intitule= 'Opérateur1' join service s on s.id = m.id_service where i.code = 'q99_item' and fi.valeur_item = 1 and f.date_creation BETWEEN 2018-01-01 AND 2017-01-01 and o.code = 'E' and s.id='3'")[1,1]
toJSON(test)
print("hello")
# 
# 
# jsonlite::toJSON(test)
#dbGetQuery(mydb,"select count(distinct p.id) as count from patient p")


