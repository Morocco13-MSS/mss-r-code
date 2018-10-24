library(RMySQL)
library(jsonlite)
library(needs)
library(dplyr)
library(funnelR)

#close all connections. only 16 can be open at one time
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

mydb = dbConnect(MySQL(), user='root', password='abcd1234!', host='localhost')
dbExecute(mydb, "use mssDB")
sapply(df,class)

c=as.data.frame(dbReadTable(mydb,"commentaire"))
cr=as.data.frame(dbReadTable(mydb,"commentaire_reponse"))
f=as.data.frame(dbReadTable(mydb,"formulaire"))
fh=as.data.frame(dbReadTable(mydb,"formulaire_historique"))
i=as.data.frame(dbReadTable(mydb,"item"))
m=as.data.frame(dbReadTable(mydb,"medecin"))
pj=as.data.frame(dbReadTable(mydb,"pj_item"))
r=as.data.frame(dbReadTable(mydb,"role"))
s=as.data.frame(dbReadTable(mydb,"service"))
u=as.data.frame(dbReadTable(mydb,"utilisateur"))

###Extract Data###

#inputs from NodeJS will fill in the where conditions below for date range, unit, organ, curative, completed forms
# don't filter by doctor/unit since they want different views and each point is a doctor or unit, however I still need the joins
#i don't think we should filter by date either initially in the beginning
df=dbGetQuery(mydb,"select * from formulaire_item fi  join formulaire f on f.id = fi.id_formulaire join patient p on p.id = f.id_patient join organe o on o.id = f.id_organe join item i on fi.id_item = i.id")
#??need to put this back into the query above...join medecin m on fi.valeur_item =m.id and i.intitule= 'Opérateur1' join service s on s.id = m.id_service where i.code = 'q99_item' and fi.valeur_item = 1 and f.date_creation BETWEEN 2018-01-01 AND 2017-01-01 and o.code = 'E' and s.id='3'

###Clean Data###

##get total number of patients per doctor
keeps=c("valeur_item","id_patient","id_item")
df2 = df[keeps]
#83	q83_item	Opérateur1
#first create data frame to get total patients per doctor
patByMd = df2[which(df2$id_item==83),]
#remove any duplicates in case the same patient is repeated twice per a given doctor
patByMd2=patByMd[!duplicated(patByMd),]
keeps=c("valeur_item","id_patient")
patByMd3 = patByMd2[keeps]
# patByMd3=data.frame(table(patByMd2$valeur_item))
colnames(patByMd3) = c("id_medecin","id_patient")


##get number of readmissions per doctor
keeps=c("valeur_item","id_patient","id_item")
df3 = df[keeps]
#231	q231_item	Score de Clavien maximal dans les 90 jours postopératoires
#get all patients with that question and died
#?? change to 5 for valeur_item
deathsbyMD=df3[which(df3$id_item==231&df3$valeur_item=='5'),]
#remove any duplicates in case the same patient is repeated twice per a given doctor
deathsbyMD2=deathsbyMD[!duplicated(deathsbyMD),]
keeps=c("valeur_item","id_patient")
deathsbyMD3 = deathsbyMD2[keeps]
colnames(deathsbyMD3)=c("clavien_score_90","id_patient")
final = merge(patByMd3,deathsbyMD3,by="id_patient",all.x=T)
#fill in 999 for missing clavien 90 scores
final$clavien_score_90[which(is.na(final$clavien_score_90))] = 999

#get total patient counts per doctor
final=final %>% add_count(id_medecin)
colnames(final)[4] = "total_patient"

#get scores per doctor
temp=as.data.frame(table(final$clavien_score_90,final$id_medecin))
colnames(temp) = c("clavien_score_90","deaths")

#get deaths per doctor
deathsbyMD4 = final %>% group_by(id_medecin) %>%
  summarise(deaths= sum(clavien_score_90 == "5"))

#merge
final2 = merge(final,deathsbyMD4,by="id_medecin",all=T)

keeps = c("id_medecin","total_patient","deaths")

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


funnelPlot = funplot(input=final4, 
                     fundata=dataSet)
funnelPlot


