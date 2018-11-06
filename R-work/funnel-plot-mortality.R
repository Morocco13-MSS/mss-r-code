#remove workspace EXCEPT input from nodeJS r-script
rm(list = ls()[which(ls()!="input")])

library(needs)
needs(RMySQL)
needs(jsonlite)
needs(dplyr)
needs(funnelR)
needs(ggplot2)

##??remove the  (testing purposes)
startDate = '"2018-01-01"'
endDate = '"2019-01-01"'
formType = '"E"'
#userLevel is either 0 for surgeon level, 1 for unit level, or 2 for all
userLevel = 2
#taken as the utilisateur.id of the specific user logging in
userId = 12
plotType = "cusumLine"

#input from NodeJS (remove commenting when done testing)
# startDate=paste('"',input[[1]],'"',sep="")
# endDate=paste('"',input[[2]],'"',sep="")
# formType = paste('"',input[[3]],'"',sep="")
# userLevel = input[[4]]
# userId = input[[5]]
# plotType = input[[6]]

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

if(userLevel==2) #2 is overall (funnel plot for all patemts)
{
  df8=df7
  # the below is for ADMINs who will see the results of each unit
  # keeps=c("id_service","id_patient","id_formulaire")
  # patByLevel3 = patByLevel2[keeps]
  # colnames(patByLevel3) = c("id_level","id_patient","id_formulaire")
} else if(userLevel==1||userLevel==0) #1 is unit-level (funnel plot for all patients in unit), 0 is surgeon level (funnel plot for all patients in unit, same as above, but we will only show 1 dot in the scatter plot)
{
  df8=df7[which(df7$id_service==serviceId),]
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
#get all patients with specific result
result3=result2[which(result2$valeur_item=='5'),]
#remove any duplicates in case the same patient is repeated twice per a given doctor
result4=result3[!duplicated(result3),]
keeps=c("id_patient","id_formulaire","doctorCode","id_service","intitule")
result5=result4[keeps]
keeps=c("id_patient","id_formulaire","doctorCode","id_service")
df9=df8[keeps]
#merge with initial df
final = merge(df8,result4,by=c("id_patient","id_formulaire","doctorCode","id_service"),all.x=T)
keeps=c("id_patient","id_formulaire","doctorCode","id_service","date_creation.x","yyyyMM.x","intitule.y","valeur_item.y")
final2=final[keeps]
colnames(final2)=c("id_patient","id_formulaire","doctorCode","id_service","date_creation","yyyyMM","intitule.y","valeur_item.y")
final3=unique(final2)
#get total patient counts per doctor
final3=final3 %>% add_count(doctorCode)
colnames(final3)[ncol(final3)] = "total_patient_md"
#get total patient counts per unit
final3=final3 %>% add_count(id_service)
colnames(final3)[ncol(final3)] = "total_patient_unit"
final3$deathFlag=0
final3$deathFlag[which(final3$valeur_item.y==5)] = 1
keeps=c("id_patient","id_formulaire","doctorCode","id_service","date_creation","yyyyMM","total_patient_md","total_patient_unit","deathFlag")
final3=final3[keeps]
#get deaths per doctor
deathsByDoctor = final3 %>% group_by(doctorCode) %>%
  summarise(deathsMd= sum(deathFlag == 1))
#get deaths per unit
deathsByUnit = final3 %>% group_by(id_service) %>%
  summarise(deathsUnit= sum(deathFlag == 1))
#merge
finalMd= merge(final3,deathsByDoctor,by="doctorCode",all=T)
keeps = c("doctorCode","id_service","total_patient_md","deathsMd")
finalMd2=finalMd[keeps]
finalUnit= merge(final3,deathsByUnit,by="id_service",all=T)
keeps = c("doctorCode","id_service","total_patient_unit","deathsUnit")
finalUnit2=finalUnit[keeps]
finalMd3=unique(finalMd2)
finalUnit3=unique(finalUnit2)

###Data Analysis###

#calculate overall proportion
op=sum(finalMd3$deathsMd)/sum(finalMd3$total_patient_md)

colnames(finalMd3)[3]="d"
colnames(finalMd3)[4]="n"

dataSet = fundata(input=finalMd3,
                  alpha=0.95,
                  alpha2=0.80,
                  benchmark=op,
                  method='approximate',
                  step=1)

funnelPlot = funplot(input=finalMd3,  fundata=dataSet)
funnelPlot2 = ggplot_build(funnelPlot)
funnelPlot2$plot$labels$x = "Number of Patients"
funnelPlot2$plot$labels$y = "Mortality Rate"
funnelPlot3 = ggplot_gtable(funnelPlot2)
plot(funnelPlot3,main="Funnel Plot for Mortality",sub=paste("Missing: ",numMiss,sep=""))

###Format for NodeJS###
scatterPlot = finalMd3
userIdDot=scatterPlot[which(scatterPlot$doctorCode==doctorCode),]
if(userLevel==0){
  scatterPlot = userIdDot
} else if(userLevel==1||userLevel==2){
  scatterPlot = scatterPlot
}
scatterPlot$n2 = scatterPlot$n/scatterPlot$d
userIdDot$n2=userIdDot$n/userIdDot$d
keeps = c("d","n2")
scatterPlot=scatterPlot[keeps]
colnames(scatterPlot) = c("x","y")
userIdDot = userIdDot[keeps]
colnames(userIdDot) = c("x","y")

unitScatterPlot=finalUnit3
unitScatterPlot$n2 = unitScatterPlot$deathsUnit/unitScatterPlot$total_patient_unit
keeps = c("total_patient_unit","n2")
unitScatterPlot=unitScatterPlot[keeps]
colnames(unitScatterPlot) = c("x","y")
unitScatterPlot=unique(unitScatterPlot)

dataSet2 = dataSet
colnames(dataSet2)[which(colnames(dataSet2)=="d")]="x"


keeps=c("x","benchmark")
benchmarkPlot = dataSet2[keeps]
colnames(benchmarkPlot)[which(colnames(benchmarkPlot)=="benchmark")]="y"

keeps=c("x","up")
upPlot = dataSet2[keeps]
colnames(upPlot)[which(colnames(upPlot)=="up")]="y"

keeps=c("x","lo")
loPlot = dataSet2[keeps]
colnames(loPlot)[which(colnames(loPlot)=="lo")]="y"

keeps=c("x","up2")
up2Plot = dataSet2[keeps]
colnames(up2Plot)[which(colnames(up2Plot)=="up2")]="y"

keeps=c("x","lo2")
lo2Plot = dataSet2[keeps]
colnames(lo2Plot)[which(colnames(lo2Plot)=="lo2")]="y"


if(plotType=="scatter") {
  scatterPlot
} else if(plotType=="benchmark") {
  benchmarkPlot
} else if(plotType=="up") {
  upPlot
} else if(plotType=="lo") {
  loPlot
} else if(plotType=="up2") {
  up2Plot
} else if(plotType=="lo2") {
  lo2Plot
} else if(plotType=="missing") {
  numMiss
} else if(plotType=="userIdDot") {
  userIdDot
} else if(plotType=="allUnitsDots") {
  unitScatterPlot
}   
