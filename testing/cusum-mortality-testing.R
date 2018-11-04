# cusum packages:
# needs(qcc)
# needs(surveillance)
# needs(spcadjust)
# needs(qcr)
# needs(bda)


(74.0102-74.00118)/74.00118

print(q$statistics)

row.names(q$data)
q$pos
q$neg



data(pistonrings)
attach(pistonrings)
diameter <- qcc.groups(diameter, sample)
q <- qcc::cusum(diameter[1:25,], decision.interval = 1, se.shift = 0,sizes=sizes)
getAnywhere(cusum)
test=diameter[1:25,]
test=as.matrix(test)
sizes <- rep(5, nrow(test))
stats <- paste("stats.", "xbar", sep = "")
stats.xbar(test, sizes)
#center is the mean
#q$statistics shows the mean of each sample group
#standard erros are calculated according to: z=(statistics - center)/(std.dev/sqrt(sizes))
# and then shifted according toz.f <- z - se.shift/2

# cusum(x, ...)data(robot)
hst = robot$TMHSTMIN
out = bda::bcusum(hst,mu=45)
plot(out)

test=c(60,20,0,15,20,30,20)
sizes=c(5,100,5,5,5,5,4)
qcc::cusum(test, decision.interval = 1, se.shift = 0,sizes=sizes)

test=c(1,20,0,15,20,30,20)


####test=final[c("id_patient","clavien_score_90")]

test=final[c("id_patient","clavien_score_90")]
test$clavien_score_90[which(test$clavien_score_90==999)]=0
test$clavien_score_90[which(test$clavien_score_90==5)]=1
test$clavien_score_90=as.numeric(test$clavien_score_90)
test$rate=0.10
test$diff=test$clavien_score_90-test$rate


# Xi ~ Po(5), i=1,...,500
disProgObj <- create.disProg(week=1:500, observed= rpois(500,lambda=5),
                             state=rep(0,500))
# there should be no alarms as mean doesn't change
res <- algo.cusum(disProgObj, control = list(range = 100:500,trans="anscombe"))
plot(res)

##pchart
test=c(20,50,10)
sizes=c(100,50,100)
qcc(test,type="p",sizes=sizes)

test=bda::cusum(final$clavien_score_90)