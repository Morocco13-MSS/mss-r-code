#remove workspace EXCEPT input from nodeJS r-script
rm(list = ls()[which(ls()!="input")])

library(needs)
needs(RMySQL)
needs(jsonlite)
needs(dplyr)
needs(funnelR)
needs(ggplot2)

##??remove the below
startDate = '"2018-01-01"'
endDate = '"2019-01-01"'
formType = '"E"'
# 0 is surgeon-level (compare against surgeon's unit's doctors),  1 is unit-level (compare against ALL  units' doctors), 2 is overall (only for ADMIN and compares results by unit ie only 4 points for the 4 units)
userLevel = 0
userId = 8
plotType = "scatterPlot"

startDate=paste('"',input[[1]],'"',sep="")
endDate=paste('"',input[[2]],'"',sep="")
formType = paste('"',input[[3]],'"',sep="")
userLevel = input[[4]]
userId = input[[5]]
plotType = input[[6]]

#close all connections. only 16 can be open at one time
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

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
# don't filter by doctor/unit since they want different views and each point is a doctor or unit, however I still need the joins
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
#coerce distinct column names since there is overlap in column names, this will append .1, .2, etc to overlapping column names
df=data.frame(df,check.names = TRUE)
nrow(df)
# Get the doctorCode of the logged in user based on userId
sqlQuery2=paste("select doctorCode as Id from utilisateur u where u.id =", userId, sep="")
cat(sqlQuery2)
doctorCode=dbGetQuery(mydb,sqlQuery2)[1,1]

###Clean Data###

##get total number of patients per level
keeps=c("valeur_item","id_patient","intitule","id_service","id_formulaire")
df2 = df[keeps]
#first create data frame to get total patients per level
patByLevel = df2[which(df2$intitule=='Opérateur1'),]
#remove any duplicates in case the same patient is repeated twice per a given doctor
patByLevel2=patByLevel[!duplicated(patByLevel),]



if(userLevel==2) #2 is overall (you will see your dot AND all other dots for ALL surgeons)
{
  keeps=c("valeur_item","id_patient","id_formulaire")
  patByLevel3 = patByLevel2[keeps]
  colnames(patByLevel3) = c("id_level","id_patient","id_formulaire")
  # the below is for ADMINs who will see the results of each unit
  # keeps=c("id_service","id_patient","id_formulaire")
  # patByLevel3 = patByLevel2[keeps]
  # colnames(patByLevel3) = c("id_level","id_patient","id_formulaire")
} else if(userLevel==1|userLevel==0) #1 is unit-level (shows your dot AND other dots for other doctors in unit), #0 is surgeon-level (just see your dot on the plot and the confidence intervals are calculated with respect to doctors in that same unit)
{
  #get the service aka unit id of the current doctor
  serviceId=unique(df$id_service[which(df$id.7==userId)])
  patByLevel3=patByLevel2[which(patByLevel2$id_service==serviceId),]
  keeps=c("valeur_item","id_patient","id_formulaire")
  patByLevel3 = patByLevel3[keeps]
  colnames(patByLevel3) = c("id_level","id_patient","id_formulaire")
}

#error check, exit if there are no patients
if(nrow(patByLevel)==0) {
  print("There are no patients associated with this userLevel.")
  break()
}





##get number of deaths per level
keeps=c("valeur_item","id_patient","intitule","id_formulaire")
df3 = df[keeps]
#231	q231_item	Score de Clavien maximal dans les 90 jours postopératoires
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
#fill in "notDead" for missing clavien 90 scores
final$clavien_score_90[which(is.na(final$clavien_score_90))] = "notDead"

#get total patient counts per doctor
final=final %>% add_count(id_level)
colnames(final)[5] = "total_patient"

#get scores per doctor
# temp=as.data.frame(table(final$clavien_score_90,final$id_medecin))
# colnames(temp) = c("clavien_score_90","deaths")

#get deaths per level
deathsByLevel = final %>% group_by(id_level) %>%
  summarise(deaths= sum(clavien_score_90 == "5"))

#merge
final2 = merge(final,deathsByLevel,by="id_level",all=T)

keeps = c("id_level","total_patient","deaths")

final3=final2[keeps]

#remove duplicates
final4=final3[!duplicated(final3),]

###Data Analysis###

#calculate overall proportion
op=sum(final4$deaths)/sum(final4$total_patient)

colnames(final4)[2]="d"
colnames(final4)[3]="n"

dataSet = fundata(input=final4,
                  alpha=0.95,
                  alpha2=0.80,
                  benchmark=op,
                  method='approximate',
                  step=1)

funnelPlot = funplot(input=final4,  fundata=dataSet)
funnelPlot2 = ggplot_build(funnelPlot)
funnelPlot2$plot$labels$x = paste("Number of Patients (Missing Clavien Scores at 90 days: ",numMiss,")",sep="")
funnelPlot2$plot$labels$y = "Mortality Rate"
funnelPlot3 = ggplot_gtable(funnelPlot2)
plot(funnelPlot3)

###Format for NodeJS###
scatterPlot = final4
if(userLevel==0){
  scatterPlot = scatterPlot[which(scatterPlot$id_level==doctorCode),]
}
userIdDot=scatterPlot[which(scatterPlot$id_level==doctorCode),]
scatterPlot$n2 = scatterPlot$n/scatterPlot$d
userIdDot$n2=userIdDot$n/userIdDot$d
keeps = c("d","n2")
scatterPlot=scatterPlot[keeps]
colnames(scatterPlot) = c("x","y")
userIdDot = userIdDot[keeps]
colnames(userIdDot) = c("x","y")

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
}   


######

final4$rate = final4$d/final4$n
test = final4$rate
test2=as.data.frame(test)
cusum(test2, decision.interval = 4, se.shift = 1)

keeps=c("clavien_score_90","id_patient")
test=final[keeps]
test$clavien_score_90[which(test$clavien_score_90==999)]=""
test$clavien_score_90[which(test$clavien_score_90==5)]="1"
test2=cusum(test, decision.interval = 4, se.shift = 1)

data(pistonrings)
detach(pistonrings)
attach(pistonrings)
diameter <- qcc.groups(diameter, sample)

q <- cusum(diameter[1:25,], decision.interval = 1, se.shift = 0.001)
q <- cusum(diameter[1:25,], decision.interval = 1, se.shift = 1)

summary(q)

if(plotType=="ucl") {
  uclPlot
} else if(plotType=="upScatter") {
  upPlot
} else if(plotType=="target") {
  targetPlot
} else if(plotType=="downScatter") {
  downPlot
} else if(plotType=="lcl") {
  lclPlot
} else if(plotType=="missing") {
  numMiss
}
  

print(q$statistics)

row.names(q$data)
q$pos
q$neg

test=data.frame(pat=c(1,2,3,4,5,6,7,8,9,10), oe=c(-0.1,-0.15,-0.21,0.65,-0.2,-0.12,-0.45,0.93,-0.2,-0.21))

test2=cusum(test,decision.interval = 1, se.shift = 0.001)
