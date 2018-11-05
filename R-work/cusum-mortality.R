#remove workspace EXCEPT input from nodeJS r-script
rm(list = ls()[which(ls()!="input")])

library(needs)
needs(RMySQL)
needs(jsonlite)
needs(dplyr)
needs(funnelR)
needs(ggplot2)

##??remove the  (testing purposes)
# startDate = '"2018-01-01"'
# endDate = '"2019-01-01"'
# formType = '"E"'
# #userLevel is either 0 for surgeon level, 1 for unit level, or 2 for all
# userLevel = 2
# #taken as the utilisateur.id of the specific user logging in
# userId = 8
# plotType = "scatterPlot"

#set the upper and lower control limits (ucl and lcl)
ucl=3
lcl=0

#input from NodeJS (remove commenting when done testing)

startDate=paste('"',input[[1]],'"',sep="")
endDate=paste('"',input[[2]],'"',sep="")
formType = paste('"',input[[3]],'"',sep="")
userLevel = input[[4]]
userId = input[[5]]
plotType = input[[6]]

#close all connections. only 16 can be open at one time
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

#connect to the database
mydb = dbConnect(MySQL(), user='root', password='abcd1234!', host='localhost')
dbSendQuery(mydb,'set character set "utf8"')
dbExecute(mydb, "use mssDB")
sapply(df,class)

# c=as.data.frame(dbReadTable(mydb,"commentaire"))
# cr=as.data.frame(dbReadTable(mydb,"commentaire_reponse"))
# f=as.data.frame(dbReadTable(mydb,"formulaire"))
# fh=as.data.frame(dbReadTable(mydb,"formulaire_historique"))
# i=as.data.frame(dbReadTable(mydb,"item"))
# m=as.data.frame(dbReadTable(mydb,"medecin"))
# pj=as.data.frame(dbReadTable(mydb,"pj_item"))
# r=as.data.frame(dbReadTable(mydb,"role"))
# s=as.data.frame(dbReadTable(mydb,"service"))
# u=as.data.frame(dbReadTable(mydb,"utilisateur"))

###Extract Data###

#inputs from NodeJS will fill in the where conditions below for date range, unit, organ, curative, completed forms
#don't filter by doctor/unit since they want different views and each point is a doctor or unit, however I still need the joins
#i don't think we should filter by date either initially in the beginning
sqlQuery=paste("select * from patient p 
               join formulaire f on p.id = f.id_patient
               join formulaire_item fi on fi.id_formulaire= f.id 
               join item i on  i.id = fi.id_item
               join organe o on o.id = f.id_organe
               join medecin m
               on exists
               (select * from item i
               join formulaire_item fi on fi.id_item = i.id
               where i.intitule = 'Opérateur1'
               and m.id=fi.valeur_item)
               join service s on s.id = m.id_service
               join utilisateur u on u.doctorCode = m.doctorCode",
               "where f.date_creation BETWEEN",startDate,"AND",endDate,
               "AND","o.code=",formType,sep=" ")
cat(sqlQuery)
df=dbGetQuery(mydb,sqlQuery)

#coerce distinct column names since there is overlap in column names, this will append .1, .2, etc to duplicate column names
df=data.frame(df,check.names = TRUE)
nrow(df)
#get the doctorCode of the logged in user based on userId
sqlQuery2=paste("select doctorCode as Id from utilisateur u where u.id =", userId, sep="")
cat(sqlQuery2)
doctorCode=dbGetQuery(mydb,sqlQuery2)[1,1]

###Clean Data###

##get total number of patients per level

keeps=c("valeur_item","id_patient","intitule","id_service","id_formulaire","date_creation")
df2 = df[keeps]
#filter for curative patients
curative = df[keeps]
curative = curative[which(curative$intitule=='Résection'&df2$valeur_item==1),]

#create data frame to get total patients per level
patByLevel = df2[which(df2$intitule=='Opérateur1'),]
#get month and year of each form
patByLevel$yyyyMM=substr(patByLevel$date_creation,1,7)

#merge to curative in order to filter for curative patients
patByLevel=merge(curative,patByLevel,by=c("id_patient","id_formulaire"),all.x=TRUE)
keeps=c("valeur_item.x","id_patient","intitule.x","id_service.x","id_formulaire","date_creation.x","yyyyMM")
patByLevel = patByLevel[keeps]
#remove any duplicates in case the same patient is repeated twice per a given doctor
patByLevel2=patByLevel[!duplicated(patByLevel),]
colnames(patByLevel2) = c("valeur_item","id_patient","intitule","id_service","id_formulaire","date_creation","yyyyMM")

if(userLevel==2) #2 is overall (cusum for all results)
{
  keeps=c("valeur_item","id_patient","id_formulaire","date_creation","yyyyMM")
  patByLevel3 = patByLevel2[keeps]
  colnames(patByLevel3) = c("id_level","id_patient","id_formulaire","date_creation","yyyyMM")
  # the below is for ADMINs who will see the results of each unit
  # keeps=c("id_service","id_patient","id_formulaire")
  # patByLevel3 = patByLevel2[keeps]
  # colnames(patByLevel3) = c("id_level","id_patient","id_formulaire")
} else if(userLevel==1) #1 is unit-level (cusum for all results in unit)
{
  #get the service aka unit id of the current doctor
  serviceId=unique(df$id_service[which(df$id.7==userId)])
  patByLevel3=patByLevel2[which(patByLevel2$id_service==serviceId),]
  keeps=c("valeur_item","id_patient","id_formulaire","date_creation","yyyyMM")
  patByLevel3 = patByLevel3[keeps]
  colnames(patByLevel3) = c("id_level","id_patient","id_formulaire","date_creation","yyyyMM")
} else if(userLevel==0) #0 is surgeon level (cusum for only surgeon's patients)
{
  patByLevel3=patByLevel2[which(patByLevel2$valeur_item==doctorCode),]
  keeps=c("valeur_item","id_patient","id_formulaire","date_creation","yyyyMM")
  patByLevel3 = patByLevel3[keeps]
  colnames(patByLevel3) = c("id_level","id_patient","id_formulaire","date_creation","yyyyMM")
}
  
#error check, exit if there are no patients
if(nrow(patByLevel)==0) {
  print("There are no patients associated with this userLevel.")
  stop()
}

##get number of deaths per level
keeps=c("valeur_item","id_patient","intitule","id_formulaire")
df3 = df[keeps]

#get all patients with that question
df4=df3[which(df3$intitule=="Score de Clavien maximal dans les 90 jours postopératoires"),]
#count the patients missing clavien scores at 90 days
numMiss = length(unique(df4$id_patient[which(df4$valeur_item=="")]))
#get all patients who died
df4=df4[which(df4$valeur_item=='5'),]

#remove any duplicates in case the same patient is repeated twice per a given doctor
df5=df4[!duplicated(df4),]
keeps=c("valeur_item","id_patient","id_formulaire")
df6 = df5[keeps]
colnames(df6)=c("clavien_score_90","id_patient","id_formulaire")
final = merge(patByLevel3,df6,by=c("id_patient","id_formulaire"),all.x=T)

#fill in 0 for not dead and 1 for dead
final$clavien_score_90[which(is.na(final$clavien_score_90))] = 0
final$clavien_score_90[which(final$clavien_score_90==5)] = 1
#order by date_creation and id_patient
final=final[(order(final$date_creation,final$id_patient)),]

# get total patient counts per doctor
final=final %>% add_count(id_level)
colnames(final)[7] = "total_patient"

# get CUSUM
final$rate=0.169
final$clavien_score_90=as.numeric(final$clavien_score_90)
final$diff=final$clavien_score_90-final$rate
final$patNum=1:nrow(final)
final=within(final, acc_sum <- cumsum(diff))
final$acc_sum=as.numeric(final$acc_sum)
final$ucl=rep(ucl,nrow(final))
final$lcl=rep(lcl,nrow(final))
cusumPlot=final[c("patNum","acc_sum")]
colnames(cusumPlot)=c("x","y")
toJSON(cusumPlot)
upPlot=final[c("patNum","ucl")]
colnames(upPlot)=c("x","y")
toJSON(upPlot)
loPlot=final[c("patNum","lcl")]
colnames(loPlot)=c("x","y")
toJSON(loPlot)
plot(final$patNum,final$acc_sum,type="l",xlab="Patient Number",ylab="Cumulative Sum Mortality (Observed-Expected)",main="Cusum Plot for Mortality"
     ,sub=paste("Missing: ",numMiss,sep=""))
lines(final$patNum,final$ucl,col="red")
lines(final$patNum,final$lcl,col="blue")
#determine alerts
alerts=cusumPlot[which(cusumPlot$y>ucl),]

###Format for NodeJS###
if(plotType=="cusumLine") {
  cusumPlot
} else if(plotType=="ucl") {
  upPlot
} else if(plotType=="lcl") {
  loPlot
} else if(plotType=="alerts") {
  alerts
} else if(plotType=="missing") {
  numMiss
}   