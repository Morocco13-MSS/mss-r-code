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
# userId = 12
# plotType = "cusumLine"

#input from NodeJS (remove commenting when done testing)
startDate=paste('"',input[[1]],'"',sep="")
endDate=paste('"',input[[2]],'"',sep="")
formType = paste('"',input[[3]],'"',sep="")
userLevel = input[[4]]
userId = input[[5]]
plotType = input[[6]]

#set the upper and lower control limits (ucl and lcl)
ucl=3.5
lcl=0

#close all connections. only 16 can be open at one time
lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

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
               join organe o on o.id = f.id_organe",
               "where f.date_creation BETWEEN",startDate,"AND",endDate,
               "AND","o.code=",formType,sep=" ")
cat(sqlQuery)
df=dbGetQuery(mydb,sqlQuery)

#coerce distinct column names since there is overlap in column names, this will append .1, .2, etc to duplicate column names
df=data.frame(df,check.names = TRUE)
nrow(df)
#get the doctorCode of the logged in user based on userId
sqlQuery2="select * from medecin m
               join service s on s.id = m.id_service
               left join utilisateur u on u.doctorCode = m.doctorCode"

cat(sqlQuery2)
doctorLookUp=dbGetQuery(mydb,sqlQuery2)
doctorLookUp=data.frame(doctorLookUp,check.names = TRUE)
#get the doctorCode of the logged in user
doctorCode=doctorLookUp$doctorCode[which(doctorLookUp$id.2==userId)]
#get the service aka unit id of the current doctor
serviceId=unique(doctorLookUp$id_service[which(doctorLookUp$id.2==userId)])

# test=df[which(df$id==84),]
# test2=test[which(test$intitule=="Opérateur1"),]

#merge to doctorLookUp to get id_service
temp = df[which(df$intitule=='Opérateur1'),]
colnames(temp)[25]="doctorCode"
doctorLookUp2=merge(temp,doctorLookUp,by="doctorCode",all.x=TRUE)

###Clean Data###
keeps=c("id_patient","id_formulaire","intitule","valeur_item","date_creation")
df2=df[keeps]
df2$yyyyMM=substr(df2$date_creation,1,7)
#filter for curative patients
keeps=c("id_patient","id_formulaire","intitule","valeur_item")
curative = df[keeps]
curative = curative[which(curative$intitule=='Résection'&curative$valeur_item==1),]
curative = unique(curative)
#merge to patByLevel in order to filter for curative patients
df3=merge(df2,curative,by=c("id_patient","id_formulaire"))
keeps=c("id_patient","id_formulaire","intitule.x","valeur_item.x","date_creation","yyyyMM")
df4 = df3[keeps]
colnames(df4)=c("id_patient","id_formulaire","intitule","valeur_item","date_creation","yyyyMM")
#merge to doctorLookUp2 to get doctor information
df5=merge(doctorLookUp2,df4,by=c("id_patient","id_formulaire"),all.y=TRUE)
keeps=c("id_patient","id_formulaire","doctorCode","id_service","intitule","valeur_item","date_creation.y","yyyyMM")
df6=df5[keeps]
colnames(df6)=c("id_patient","id_formulaire","doctorCode","id_service","intitule","valeur_item","date_creation","yyyyMM")
df7=unique(df6)
# test=patByLevel5[c("id_patient","doctorCode","id_service")]
# test=unique(test)

if(userLevel==2) #2 is overall (cusum for all results)
{
  df8=df7
  # the below is for ADMINs who will see the results of each unit
  # keeps=c("id_service","id_patient","id_formulaire")
  # patByLevel3 = patByLevel2[keeps]
  # colnames(patByLevel3) = c("id_level","id_patient","id_formulaire")
} else if(userLevel==1) #1 is unit-level (cusum for all results in unit)
{
  df8=df7[which(df7$id_service==serviceId),]
} else if(userLevel==0) #0 is surgeon level (cusum for only surgeon's patients)
{
  df8=df7[which(df7$doctorCode==doctorCode),]
}

##get number of results per level
result=df8[which(df8$intitule=="Score de Clavien maximal dans les 90 jours postopératoires"),]
#count the patients missing results at 90 days
numMiss = length(unique(result$id_patient[which(result$valeur_item=="")]))
#remove those patients from consideration
removal=-which(result$valeur_item=="")
if(length(removal)!=0){
  result2=result[-which(result$valeur_item==""),]
} else {
  result2=result
}

final=result2
#fill in 0 for not dead and 1 for dead
final$deathFlag=0
final$deathFlag[which(final$valeur_item==5)] = 1
#order by date_creation and id_patient in order to get correct sequence of patients
final=final[(order(final$date_creation,final$id_patient)),]

###Data Analysis###
#get final death rate for curative patients and do not have missing data
allDead=nrow(unique(df7[which(df7$intitule=="Score de Clavien maximal dans les 90 jours postopératoires"&df7$valeur_item==5),]))
total=nrow(unique(df7[which(df7$intitule=="Score de Clavien maximal dans les 90 jours postopératoires"&df7$valeur_item!=""),]))
final$rate=allDead/total
final$deathFlag=as.numeric(final$deathFlag)
final$diff=final$deathFlag-final$rate
final$patNum=1:nrow(final)
final=within(final, acc_sum <- cumsum(diff))
final$acc_sum=as.numeric(final$acc_sum)
final$ucl=rep(ucl,nrow(final))
final$lcl=rep(lcl,nrow(final))
#determine alerts
alerts=final[which(final$acc_sum>ucl),]
keeps=c("patNum","acc_sum")
alerts=alerts[keeps]

#test Plot in R
plot(final$patNum,final$acc_sum,type="l",xlab="Patient Number",ylab="Cumulative Sum Mortality (Observed-Expected)",main="Cusum Plot for Mortality"
     ,sub=paste("Missing: ",numMiss,sep=""))
lines(final$patNum,final$ucl,col="red")
lines(final$patNum,final$lcl,col="blue")
points(alerts$patNum,alerts$acc_sum,col="red")

#Format for NodeJS
cusumPlot=final[c("patNum","acc_sum")]
colnames(cusumPlot)=c("x","y")
# toJSON(cusumPlot)
upPlot=final[c("patNum","ucl")]
colnames(upPlot)=c("x","y")
# toJSON(upPlot)
loPlot=final[c("patNum","lcl")]
colnames(loPlot)=c("x","y")
# toJSON(loPlot)
colnames(alerts)=c("x","y")
# toJSON(alerts)

if(plotType=="cusumLine") {
  cusumPlot
} else if(plotType=="ucl") {
  upPlot
} else if(plotType=="lcl") {
  loPlot
} else if(plotType=="alerts") {
  if(nrow(alerts)==0){
    alerts=""
  } else {
    alerts
  }
} else if(plotType=="missing") {
  numMiss
}