library(funnelR)

###real data test
df = read.delim("/Users/linjo/Box Sync/Morocco13/Project-MSS/Techstuff/fiche_rectum.csv",sep=",", colClasses = c(rep("character", 130)))
length(unique(df$numdossier))
#45

keeps=c("numdossier","opérateur1","opérateur2","clavien_90","date_sortie")
#keeps=c("opérateur1","clavien_90")

df2 = df[,keeps]

deadDf = df2[which(df2$clavien_90==5),]
deadDf2 = data.frame(table(deadDf$opérateur1))
colnames(deadDf2) = c("opérateur1","deaths")

totalDf=data.frame(table(df2$opérateur1))
colnames(totalDf) = c("opérateur1","patientCount")

df3 = merge(df2,deadDf2,by=c("opérateur1"),all=T)
#probably need to rethink how I am coming up with total patient counts and deaths since manipulation of data frame is below
df3[is.na(df3$deaths),]$deaths=0

df4 = merge(df3,totalDf,by=c("opérateur1"))

keeps = c("opérateur1","patientCount","deaths")
df5 = df4[keeps]
df5 = unique(df5)
df5$deathRate = df5$deaths/df5$patientCount

df6=df5

colnames(df6)[4] = "n2"
colnames(df6)[2] = "d"

colnames(df6)[3] = "n"

dataSet = fundata(input=df6, 
                       alpha=0.95, 
                       alpha2=0.997, 
                  benchmark=0.089,
                       method='approximate', 
                       step=1)


testPlot = funplot(input=df6, 
                       fundata=dataSet)

testPlot
#opérateur 1 is lead surgeon?

counts=as.data.frame(table(df2))

summary(Indometh)
wide <- reshape(Indometh, v.names = "conc", idvar = "Subject",
                timevar = "time",
                direction = "wide")
wide
