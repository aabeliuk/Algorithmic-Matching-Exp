# Load Libraries

# library(tidyr)
# library(dplyr)
# library(lme4)

# library(stargazer)
# library(pwr)
# library(fields)

# library(aod)
# library(sjPlot)
# library(effects)

library(ggplot2)
library(Hmisc)



dic_to_list <- function(x){
  pay = list()
  split = gsub('\\{', '', toString(x))
  split = gsub('\\}', '', split)
  values = unlist(strsplit(split, ','))
  for (i in 1:length(values)) {
    v = unlist(strsplit(values[i], ':'))
    # gsub('\"', '', v[1])
    pay[i] = strtoi(v[2])
  }
  
  return(unlist(pay))
}


universal.dist <- function(mydata){
  pay = list()
  payoff = mydata$ini_payoff
  for(i in 1:length(payoff)) {
    p = dic_to_list(payoff[i])  
    pay = append(pay,p)
  }
  pay = unlist(pay)
  
  breaks = c(0,2,4,6,8,10,12,14,16,18,20)
  hist(pay, breaks = breaks, probability=TRUE, main="Universal Payoff Distribution",  xlab="Payoff",cex.lab=1.7,cex.axis=1.4,cex.main=1.7)
  abline(v=mean(pay, na.rm=TRUE),lty=2)
  return(pay)
}


#standard error
std <- function(x) sd(x,na.rm=TRUE)/sqrt(length(x))

#mean 
mean.na <- function(x) mean(x, na.rm=TRUE)


# Distribution of payoffs
dist.payoff <- function(mydata.S, mydata.F) {
  
  payoff.S = data.frame(mydata.S$payoff1, mydata.S$payoff2 ,mydata.S$payoff3 ,mydata.S$payoff4, mydata.S$payoff5 ,mydata.S$payoff6 ,mydata.S$payoff7 ,mydata.S$payoff8 ,mydata.S$payoff9 ,mydata.S$payoff10 )
  payoff.F = data.frame(mydata.F$payoff1, mydata.F$payoff2 ,mydata.F$payoff3 ,mydata.F$payoff4, mydata.F$payoff5 ,mydata.F$payoff6 ,mydata.F$payoff7 ,mydata.F$payoff8 ,mydata.F$payoff9 ,mydata.F$payoff10 )
  
  payoff.S = stack(payoff.S)
  payoff.F = stack(payoff.F)
  
  hist(payoff.F[,1], probability=TRUE, ylim = c(0,0.25), main=paste("Realized Payoff distribution ","\nfor Fair Algorithm") ,xlab="Payoff",cex.lab=1.7,cex.axis=1.4,cex.main=1.7)
  abline(v=mean(payoff.F[,1], na.rm=TRUE),lty=2)
  hist(payoff.S[,1], probability=TRUE, ylim = c(0,0.15), main=paste("Realized Payoff distribution ","\nfor Selfish Algorithm"),xlab="Payoff",cex.lab=1.7,cex.axis=1.4,cex.main=1.7)
  abline(v=mean(payoff.S[,1], na.rm=TRUE),lty=2)
  
  
}

dist.payoff.1 <- function(mydata.S, mydata.F) {
  
  payoff.S = data.frame(mydata.S$payoff1, mydata.S$payoff2 ,mydata.S$payoff3 )
  payoff.F = data.frame(mydata.F$payoff1, mydata.F$payoff2 ,mydata.F$payoff3 )
  
  payoff.S = stack(payoff.S)
  payoff.F = stack(payoff.F)
  
  breaks = c(0,2,4,6,8,10,12,14,16,18,20)
  breaks = c(0,5,10,15,20)
  hist(payoff.F[,1], breaks = breaks, probability=TRUE, ylim = c(0,0.20), main="Fair algorithm rounds 1-3",xlab="payoff")
  hist(payoff.S[,1],breaks = breaks, probability=TRUE, ylim = c(0,0.20), main="Selfish algorithm rounds 1-3",xlab="payoff")
  
}

dist.payoff.2 <- function(mydata.S, mydata.F) {
  
  payoff.S = data.frame(mydata.S$payoff4, mydata.S$payoff5 ,mydata.S$payoff6 ,mydata.S$payoff7 )
  payoff.F = data.frame(mydata.F$payoff4, mydata.F$payoff5 ,mydata.F$payoff6 ,mydata.F$payoff7 )
  
  payoff.S = stack(payoff.S)
  payoff.F = stack(payoff.F)
  
  breaks = c(0,2,4,6,8,10,12,14,16,18,20)
  breaks = c(0,5,10,15,20)
  hist(payoff.F[,1], breaks = breaks, probability=TRUE, ylim = c(0,0.20), main="Fair algorithm rounds 4-7",xlab="payoff")
  hist(payoff.S[,1], breaks = breaks, probability=TRUE, ylim = c(0,0.20), main="Selfish algorithm rounds 4-7",xlab="payoff")
  
  
}

dist.payoff.3 <- function(mydata.S, mydata.F) {
  
  payoff.S = data.frame(mydata.S$payoff8 ,mydata.S$payoff9 ,mydata.S$payoff10 )
  payoff.F = data.frame(mydata.F$payoff8 ,mydata.F$payoff9 ,mydata.F$payoff10 )
  
  payoff.S = stack(payoff.S)
  payoff.F = stack(payoff.F)
  
  breaks = c(0,2,4,6,8,10,12,14,16,18,20)
  breaks = c(0,5,10,15,20)
  hist(payoff.F[,1],breaks = breaks, probability=TRUE, ylim = c(0,0.20), main="Fair algorithm rounds 8-10",xlab="payoff")
  hist(payoff.S[,1],breaks = breaks, probability=TRUE, ylim = c(0,0.20), main="Selfish algorithm rounds 8-10",xlab="payoff")
  
  
  
}

# Abosulute SW between instances per period
SW.mean.stage <- function(mydata.S, mydata.F) {
  
  payoff.S = data.frame(mydata.S$payoff1, mydata.S$payoff2 ,mydata.S$payoff3 ,mydata.S$payoff4, mydata.S$payoff5 ,mydata.S$payoff6 ,mydata.S$payoff7 ,mydata.S$payoff8 ,mydata.S$payoff9 ,mydata.S$payoff10 )
  payoff.F = data.frame(mydata.F$payoff1, mydata.F$payoff2 ,mydata.F$payoff3 ,mydata.F$payoff4, mydata.F$payoff5 ,mydata.F$payoff6 ,mydata.F$payoff7 ,mydata.F$payoff8 ,mydata.F$payoff9 ,mydata.F$payoff10 )
  
  # payoff.S = aggregate(list(payoff1=mydata.S$payoff1, payoff2=mydata.S$payoff2 ,payoff3=mydata.S$payoff3 ,payoff4=mydata.S$payoff4, payoff5=mydata.S$payoff5 ,payoff6=mydata.S$payoff6 ,payoff7=mydata.S$payoff7 ,payoff8=mydata.S$payoff8 ,payoff9=mydata.S$payoff9 ,payoff10=mydata.S$payoff10 ), by=list( group_id=mydata.S$group_id, role_id=mydata.S$role_id, session.code=mydata.S$session.code), FUN=mean, na.rm=TRUE)
  # payoff.F = aggregate(list(payoff1=mydata.F$payoff1, payoff2=mydata.F$payoff2 ,payoff3=mydata.F$payoff3 ,payoff4=mydata.F$payoff4, payoff5=mydata.F$payoff5 ,payoff6=mydata.F$payoff6 ,payoff7=mydata.F$payoff7 ,payoff8=mydata.F$payoff8 ,payoff9=mydata.F$payoff9 ,payoff10=mydata.F$payoff10 ), by=list( group_id=mydata.F$group_id, role_id=mydata.F$role_id, session.code=mydata.F$session.code), FUN=mean, na.rm=TRUE)
  
  
  SW.S.mean = apply(payoff.S, 2, mean.na )
  SW.F.mean = apply(payoff.F, 2, mean.na )
  SW.S.std = apply(payoff.S, 2, std )
  SW.F.std = apply(payoff.F, 2, std )


  SW.Mean <- rbind(SW.F.mean, SW.S.mean)
  colnames(SW.Mean) <- seq(1,10)
  SW.std <- rbind(SW.F.std, SW.S.std)
  colnames(SW.std) <- seq(1,10)

  # barCenters <- barplot(SW.Mean, beside = TRUE, legend= c("Fair","Selfish"), args.legend = list(bty = "n"), ylim=c(0,max(SW.Mean)+4), xlab="Period")
  # errbar(barCenters,SW.Mean, SW.Mean+SW.std, SW.Mean-SW.std, add=T, xlab="", pch='.')
  
  par(bty = 'n') 
  plot(seq(1,10),SW.F.mean, xlim=c(0,11), ylim=c(0,20),col="black",ylab="Mean Payoff (cents)", xlab="Round",pch=18,cex=3,cex.lab=2.2,cex.axis=1.7)
  # hack: we draw arrows but with very special "arrowheads"
  arrows(seq(1,10), SW.F.mean-SW.F.std, seq(1,10), SW.F.mean+SW.F.std, length=0.05, angle=90, code=3) 
  arrows(seq(1,10), SW.S.mean-SW.S.std, seq(1,10), SW.S.mean+SW.S.std, length=0.05, angle=90, code=3) 
  points(seq(1,10),SW.S.mean,col="grey",pch=18,cex=3)
  
  lines(seq(1,10),SW.F.mean, ,col="black",lwd=1.2,lty=2)
  lines(seq(1,10),SW.S.mean, ylim=c(0,1),col="grey",lwd=1.2)
  legend(x=6,y=21, bty = "n", cex=2.2,lty=c(2,1),lwd=1.5,legend= c("Fair","Selfish"), col=c("black", "grey"))
  
  

  
  
  # barplot(SW.S.mean, main="SW per period in Selfish Algorithm", ylim=c(0,15))
  
  
}

# Abosulute SW between instances
SW.mean <- function(mydata.S, mydata.F) {
  
  total_payoff.S = mydata.S$payoff1 + mydata.S$payoff2  + mydata.S$payoff3  + mydata.S$payoff4 +  mydata.S$payoff5  + mydata.S$payoff6  + mydata.S$payoff7  + mydata.S$payoff8  + mydata.S$payoff9  + mydata.S$payoff10
  total_payoff.F = mydata.F$payoff1 + mydata.F$payoff2  + mydata.F$payoff3  + mydata.F$payoff4 +  mydata.F$payoff5  + mydata.F$payoff6  + mydata.F$payoff7  + mydata.F$payoff8  + mydata.F$payoff9  + mydata.F$payoff10 
  
  
  SW.S.mean = mean.na(total_payoff.S)
  SW.F.mean = mean.na(total_payoff.F)
  SW.S.std = sem(total_payoff.S)
  SW.F.std = sem(total_payoff.F)
  
  SW.Mean <- rbind(SW.F.mean, SW.S.mean)
  SW.std <- rbind(SW.F.std, SW.S.std)
  
  # barCenters <- barplot(SW.Mean, beside = TRUE, legend= c("Fair","Selfish"), ylim=c(0,max(SW.Mean)+20), args.legend = list(x=3.5,y=max(SW.Mean)+12, bty = "n"), )
  # errbar(barCenters,SW.Mean, SW.Mean+SW.std, SW.Mean-SW.std, add=T, xlab="", pch='.')
  # text(barCenters,SW.Mean-15,labels=round(SW.Mean),cex=.9)
  
  barCenters <- barplot(SW.Mean, beside = TRUE, legend= c("Fair","Selfish"),  args.legend = list(x = "topright", bty = "n", x=3.6,y=200,cex=2.2, x.intersp=0.2), ylim=c(0,200),cex=3,cex.lab=2.2,cex.axis=1.7,xlab="Algorithm",ylab="Mean Payoff (cents)")
  errbar(barCenters,SW.Mean, SW.Mean+SW.std, SW.Mean-SW.std, add=T, xlab="", pch='.')
  text(barCenters,SW.Mean+15,labels=round(SW.Mean),cex=2)
  
  # barplot(SW.S.mean, main="SW per period in Selfish Algorithm", ylim=c(0,15))
  
  
}

# PoA between instances
poa.mean <- function(mydata.S, mydata.F) {
  
  payoff.S = aggregate(list(total_payoff=mydata.S$total_payoff, payoff1=mydata.S$payoff1, payoff2=mydata.S$payoff2 ,payoff3=mydata.S$payoff3 ,payoff4=mydata.S$payoff4, payoff5=mydata.S$payoff5 ,payoff6=mydata.S$payoff6 ,payoff7=mydata.S$payoff7 ,payoff8=mydata.S$payoff8 ,payoff9=mydata.S$payoff9 ,payoff10=mydata.S$payoff10 ), by=list( group_id=mydata.S$group_id, role_id=mydata.S$role_id, session.code=mydata.S$session.code), FUN=mean, na.rm=TRUE)
  payoff.F = aggregate(list(total_payoff=mydata.F$total_payoff, payoff1=mydata.F$payoff1, payoff2=mydata.F$payoff2 ,payoff3=mydata.F$payoff3 ,payoff4=mydata.F$payoff4, payoff5=mydata.F$payoff5 ,payoff6=mydata.F$payoff6 ,payoff7=mydata.F$payoff7 ,payoff8=mydata.F$payoff8 ,payoff9=mydata.F$payoff9 ,payoff10=mydata.F$payoff10 ), by=list( group_id=mydata.F$group_id, role_id=mydata.F$role_id, session.code=mydata.F$session.code), FUN=mean, na.rm=TRUE)
  

  poa <- cbind(round(payoff.S[5:14]/payoff.F[5:14],3))
  colnames(poa) <- seq(1,10)
  poa.mean.stage = apply(poa,2,mean)
  poa.min.stage = apply(poa,2,min)
  poa.max.stage = apply(poa,2,max)
  poa.std = apply(poa,2,std)
  
  barCenters <- barplot(poa.mean.stage, main="PoA per period", ylim = c(0,1.3))
  errbar(barCenters,poa.mean.stage, poa.mean.stage+poa.std, poa.mean.stage-poa.std, add=T, xlab="", pch='.')
  # errbar(barCenters,poa.mean.stage, poa.max.stage, poa.min.stage, add=T, xlab="", pch='.')
  
  return(poa)
  
}

# PoA between instances
poa.total <- function(mydata.S, mydata.F) {
  
  payoff.S = aggregate(list(total_payoff=mydata.S$total_payoff), by=list( group_id=mydata.S$group_id, role_id=mydata.S$role_id, session.code=mydata.S$session.code), FUN=mean, na.rm=TRUE)
  payoff.F = aggregate(list(total_payoff=mydata.F$total_payoff), by=list( group_id=mydata.F$group_id, role_id=mydata.F$role_id, session.code=mydata.F$session.code), FUN=mean, na.rm=TRUE)
  
  
  poa <- cbind(round(payoff.S[4]/payoff.F[4],3))
  colnames(poa)<-"study"
  # colnames(poa) <- seq(1,10)
  poa.mean.stage = apply(poa,2,mean)
  poa.min.stage = apply(poa,2,min)
  poa.max.stage = apply(poa,2,max)
  poa.std = apply(poa,2,std)
  print(poa.mean.stage)
  print(poa.min.stage)
  print(poa.max.stage)
  
  
  barCenters <- barplot(poa.mean.stage, ylim = c(0,poa.max.stage), xlab="", ylab="Price of Anarchy",pch=18,cex=2,cex.lab=2.,cex.axis=1.7)
  errbar(barCenters,poa.mean.stage,  poa.max.stage, poa.min.stage, add=T, xlab="", pch='.')
  # errbar(barCenters,poa.mean.stage, poa.max.stage, poa.min.stage, add=T, xlab="", pch='.')
  
  return(poa)
  
}

sem<-function(x){
  return(sd(x)/sqrt(length(x)))
}

fun.retention.rate <- function(x){
  return(sum(x==2, na.rm=T)/length(x) )
  
}

fun.retention.rate.sem <- function(x){
  
  return(sem(x==2, na.rm=T)/length(x) )
  
}


fun.exit.rate <- function(x){
  
  return(sum(x==3, na.rm=T)/length(x[!is.na(x)]) )
  
}

fun.exit.count <- function(x){
  
  return(sum(x==3, na.rm=T) )
  
}


#Retention rates per period
retention.rates.period <- function(mydata.S, mydata.F) {
  
  option.S = data.frame(mydata.S$option1, mydata.S$option2 ,mydata.S$option3 ,mydata.S$option4, mydata.S$option5 ,mydata.S$option6 ,mydata.S$option7 ,mydata.S$option8 ,mydata.S$option9)
  option.F = data.frame(mydata.F$option1, mydata.F$option2 ,mydata.F$option3 ,mydata.F$option4, mydata.F$option5 ,mydata.F$option6 ,mydata.F$option7 ,mydata.F$option8 ,mydata.F$option9)
  
  S.retention = apply(option.S, 2, fun.retention.rate )
  F.retention = apply(option.F, 2, fun.retention.rate )

  
  S.exit = apply(option.S, 2, fun.exit.rate )
  F.exit = apply(option.F, 2, fun.exit.rate )
  
  retention <- rbind(F.retention, S.retention)
  exit <- rbind(F.exit, S.exit)
  colnames(retention) <- seq(1,9)
  colnames(exit) <- seq(1,9)
  # xvalues = rbind(seq(1,9),seq(1,9))
  par(bty = 'n') 
  plot(seq(1,9),F.retention, xlim=c(0,10), ylim=c(0,0.8),col="black",ylab="User Engagment", xlab="Round",pch=18,cex=3,cex.lab=2.2,cex.axis=1.7) 
  lines(seq(1,9),F.retention, ylim=c(0,1),col="black",lwd=3,lty=2)
  lines(seq(1,9),S.retention, ylim=c(0,1),col="grey",lwd=3)
  legend(x = "topright", bty = "n", cex=2.2,lty=c(2,1),lwd=1.5, legend= c("Fair","Selfish"), col=c("black", "grey"))
  
  
  # barCenters <- plot(xvalues,retention, beside = TRUE, legend= c("Fair","Selfish"), args.legend = list(x = "topright", bty = "n"), ylim=c(0,1.1))
  # barCenters <- barplot(exit, main="Drop Rate", beside = TRUE, legend= c("Fair","Selfish"), args.legend = list(x = "topright", bty = "n", inset=c(-0.2,-0.1)), ylim=c(0,0.2))
  
  # diff <- cbind(round(option.S [4:12]-option.F[4:12],3))
  
  return(retention)
  
}

#Retention rates 
retention.rates.overall <- function(mydata.S, mydata.F) {
  
  option.S = data.frame(mydata.S$option1, mydata.S$option2 ,mydata.S$option3 ,mydata.S$option4, mydata.S$option5 ,mydata.S$option6 ,mydata.S$option7 ,mydata.S$option8 ,mydata.S$option9)
  option.F = data.frame(mydata.F$option1, mydata.F$option2 ,mydata.F$option3 ,mydata.F$option4, mydata.F$option5 ,mydata.F$option6 ,mydata.F$option7 ,mydata.F$option8 ,mydata.F$option9)
  
  S.retention = apply(option.S, 1, fun.retention.rate )
  F.retention = apply(option.F, 1, fun.retention.rate )
  
  S.retention.mean = mean(S.retention)
  F.retention.mean = mean(F.retention)
  S.retention.se = sem(S.retention)
  F.retention.se = sem(F.retention)
  
  ret.Mean <- rbind(F.retention.mean, S.retention.mean)
  ret.std <- rbind(F.retention.se, S.retention.se)
  
  
  S.exit = apply(option.S, 1, fun.exit.count )
  F.exit = apply(option.F, 1, fun.exit.count )
  S.exit.mean = sum(S.exit)/length(S.exit)
  F.exit.mean = sum(F.exit)/length(F.exit)

  exit <- rbind(F.exit.mean, S.exit.mean)
  
  barCenters <- barplot(ret.Mean, beside = TRUE, legend= c("Fair","Selfish"),  args.legend = list(x = "topright", bty = "n",cex=2.2, x.intersp=0.2), ylim=c(0,0.8),cex=3,cex.lab=2.2,cex.axis=1.7, ylab="User Engagment",xlab="Algorithm")
  errbar(barCenters, ret.Mean, ret.Mean+ret.std, ret.Mean-ret.std, add=T, xlab="", pch=18)
  text(barCenters,ret.Mean+0.08,labels=round(ret.Mean,2),cex=2)
  
  # barCenters <- barplot(exit, main="Drop Rate", beside = TRUE, legend= c("Fair","Selfish"),args.legend = list(x = "topright", bty = "n", x=3.5,y=max(exit)+0.11), ylim=c(0,max(exit)+0.1))
  # text(barCenters, exit-0.03,labels=round(exit,2),cex=.9)
  
  
  
}

drop.rates.overall <- function(mydata.S, mydata.F) {
  
  # option.S = data.frame(mydata.S$option1, mydata.S$option2 ,mydata.S$option3 ,mydata.S$option4, mydata.S$option5 ,mydata.S$option6 ,mydata.S$option7 ,mydata.S$option8 ,mydata.S$option9)
  # option.F = data.frame(mydata.F$option1, mydata.F$option2 ,mydata.F$option3 ,mydata.F$option4, mydata.F$option5 ,mydata.F$option6 ,mydata.F$option7 ,mydata.F$option8 ,mydata.F$option9)
  
  drop.S = aggregate(list(op1=mydata.S$option1, op2=mydata.S$option2 ,op3=mydata.S$option3 ,op4=mydata.S$option4, op5=mydata.S$option5 ,op6=mydata.S$option6 ,op7=mydata.S$option7 ,op8=mydata.S$option8 ,op9=mydata.S$option9 ), by=list( group_id=mydata.S$group_id, role_id=mydata.S$role_id, session.code=mydata.S$session.code), FUN=fun.exit.count)
  drop.F = aggregate(list(op1=mydata.F$option1, op2=mydata.F$option2 ,op3=mydata.F$option3 ,op4=mydata.F$option4, op5=mydata.F$option5 ,op6=mydata.F$option6 ,op7=mydata.F$option7 ,op8=mydata.F$option8 ,op9=mydata.F$option9 ), by=list( group_id=mydata.F$group_id, role_id=mydata.F$role_id, session.code=mydata.F$session.code), FUN=fun.exit.count)
  drop.S =  drop.S[4:12]
  drop.F =  drop.F[4:12]
  drop.S = rowSums(drop.S)
  drop.F = rowSums(drop.F)
  
  users.S = aggregate(list(users=mydata.S$visited ), by=list( group_id=mydata.S$group_id, role_id=mydata.S$role_id, session.code=mydata.S$session.code), FUN=sum)
  users.F = aggregate(list(users=mydata.F$visited ), by=list( group_id=mydata.F$group_id, role_id=mydata.F$role_id, session.code=mydata.F$session.code), FUN=sum)
  users.S = users.S[4]
  users.F = users.F[4]
  
  drop.rate.S = drop.S/users.S
  colnames(drop.rate.S)=c('rate')
  drop.rate.F = drop.S/users.F
  colnames(drop.rate.F)=c('rate')
  

  
  S.drop.mean = mean(drop.rate.S$rate)
  F.drop.mean = mean(drop.rate.F$rate)
  S.drop.se = sem(drop.rate.S$rate)
  F.drop.se = sem(drop.rate.F$rate)
  
  ret.Mean <- rbind(F.drop.mean, S.drop.mean)
  ret.std <- rbind(F.drop.se, S.drop.se)
  
  
  barCenters <- barplot(ret.Mean, beside = TRUE, legend= c("Fair","Selfish"),  args.legend = list(x = "topright", bty = "n", cex=1.2,y=0.22,x=3), ylim=c(0,0.2))
  errbar(barCenters, ret.Mean, ret.Mean+ret.std, ret.Mean-ret.std, add=T, xlab="", pch=18)
  text(barCenters+0.35,ret.Mean+0.009,labels=round(ret.Mean,2),cex=1.2)
  

  
  
  
}


fun.count.change <- function(x){
  
  return(sum(x==2,na.rm=T))
  
}


# Prob distribution of change
prob.dist.3 <- function(option, payoff) {
  
  opt = option[!is.na(option)]
  pay = payoff[!is.na(payoff)]
  #oder by payoff
  arg = order(pay)
  opt = opt[arg]
  pay = pay[arg] 
  c1 = which(pay>3)[1] - 1
  c2 = which(pay>6)[1] - 1
  c3 = which(pay>9)[1] - 1
  c4 = which(pay>12)[1] - 1
  c5 = which(pay>15)[1] - 1
  c6 = which(pay>18)[1] - 1
  c7 = length(pay)
  #probabilities of change
  p1 = sum(opt[1:c1]==2,na.rm=T)/c1
  p2 = sum(opt[(c1+1):c2]==2,na.rm=T)/(c2-c1)
  p3 = sum(opt[(c2+1):c3]==2,na.rm=T)/(c3-c2)
  p4 = sum(opt[(c3+1):c4]==2,na.rm=T)/(c4-c3)
  p5 = sum(opt[(c4+1):c5]==2,na.rm=T)/(c5-c4)
  p6 = sum(opt[(c5+1):c6]==2,na.rm=T)/(c6-c5)
  p7 = sum(opt[(c6+1):c7]==2,na.rm=T)/(c7-c6)  
  
  return(c(p1,p2,p3,p4,p5,p6,p7))
}


# Prob distribution of change
prob.dist <- function(option, payoff) {

  opt = option[!is.na(option)]
  pay = payoff[!is.na(payoff)]
  #oder by payoff
  arg = order(pay)
  opt = opt[arg]
  pay = pay[arg] 
  c1 = which(pay>5)[1] - 1
  c2 = which(pay>10)[1] - 1
  c3 = which(pay>15)[1] - 1
  c4 = length(pay)
  c3 = ifelse(is.na(c3),c4,c3)
  #probabilities of change
  p1 = sum(opt[1:c1]==2,na.rm=T)/c1
  p2 = sum(opt[(c1+1):c2]==2,na.rm=T)/(c2-c1)
  p3 = sum(opt[(c2+1):c3]==2,na.rm=T)/(c3-c2)
  p4 = sum(opt[(c3+1):c4]==2,na.rm=T)/(c4-c3)
  
  return(c(p1,p2,p3,p4))
}


plot.prob.dist <- function(mydata) {
  
  payoff = data.frame(mydata$payoff1, mydata$payoff2 ,mydata$payoff3 ,mydata$payoff4, mydata$payoff5 ,mydata$payoff6 ,mydata$payoff7 ,mydata$payoff8 ,mydata$payoff9  )
  option = data.frame(mydata$option1, mydata$option2 ,mydata$option3 ,mydata$option4, mydata$option5 ,mydata$option6 ,mydata$option7 ,mydata$option8 ,mydata$option9  )
  
  
  payoff = stack(payoff)
  option = stack(option)
 
  p = prob.dist.3(option[,1], payoff[,1])
  
  # barplot(p,names.arg=c("0-5","6-10","11-15","16-20"), ylim=c(0,1), main="")

  return(p)
  }




plot.prob.dist.stage.3 <- function(mydata) {
  
  p1 = prob.dist(data.frame(mydata$option1, mydata$option2 ,mydata$option3 ), data.frame(mydata$payoff1, mydata$payoff2 ,mydata$payoff3) )
  p2 = prob.dist(data.frame(mydata$option4, mydata$option5 ,mydata$option6  ), data.frame(mydata$payoff4, mydata$payoff5 ,mydata$payoff6) )
  p3 = prob.dist(data.frame(mydata$option7, mydata$option8 ,mydata$option9 ), data.frame(mydata$payoff7, mydata$payoff8 ,mydata$payoff9))

  
  barplot(p1,names.arg=c("0-5","6-10","11-15","16-20"), ylim=c(0,1), main="Rounds 1-3")
  barplot(p2,names.arg=c("0-5","6-10","11-15","16-20"), ylim=c(0,1), main="Rounds 4-6")
  barplot(p3,names.arg=c("0-5","6-10","11-15","16-20"), ylim=c(0,1), main="Rounds 7-9")
 
}



plot.prob.dist.stage <- function(mydata) {
  p1 = prob.dist(mydata$option1, mydata$payoff1)
  p2 = prob.dist(mydata$option2, mydata$payoff2)
  p3 = prob.dist(mydata$option3, mydata$payoff3)
  p4 = prob.dist(mydata$option4, mydata$payoff4)
  p5 = prob.dist(mydata$option5, mydata$payoff5)
  p6 = prob.dist(mydata$option6, mydata$payoff6)
  p7 = prob.dist(mydata$option7, mydata$payoff7)
  p8 = prob.dist(mydata$option8, mydata$payoff8)
  p9 = prob.dist(mydata$option9, mydata$payoff9)
  

  par(mfrow=c(3,3))
  barplot(p1,names.arg=c("0-5","6-10","11-15","16-20"), ylim=c(0,1), main="Stage 1")
  barplot(p2,names.arg=c("0-5","6-10","11-15","16-20"), ylim=c(0,1), main="Stage 2")
  barplot(p3,names.arg=c("0-5","6-10","11-15","16-20"), ylim=c(0,1), main="Stage 3")
  barplot(p4,names.arg=c("0-5","6-10","11-15","16-20"), ylim=c(0,1), main="Stage 4")
  barplot(p5,names.arg=c("0-5","6-10","11-15","16-20"), ylim=c(0,1), main="Stage 5")
  barplot(p6,names.arg=c("0-5","6-10","11-15","16-20"), ylim=c(0,1), main="Stage 6")
  barplot(p7,names.arg=c("0-5","6-10","11-15","16-20"), ylim=c(0,1), main="Stage 7")
  barplot(p8,names.arg=c("0-5","6-10","11-15","16-20"), ylim=c(0,1), main="Stage 8")
  barplot(p9,names.arg=c("0-5","6-10","11-15","16-20"), ylim=c(0,1), main="Stage 9")
}

count_conditions <- function(x){
  c1 = sum(x=="self", na.rm=T)
  c2 = sum(x=="fair", na.rm=T)
  return(c1>0 & c2>0)
}


delta.switch <- function(mydata, opt) {
  
  payoff = data.frame(mydata$payoff1, mydata$payoff2 ,mydata$payoff3 ,mydata$payoff4, mydata$payoff5 ,mydata$payoff6 ,mydata$payoff7 ,mydata$payoff8 ,mydata$payoff9, mydata$payoff10  )
  option = data.frame(mydata$option1, mydata$option2 ,mydata$option3 ,mydata$option4, mydata$option5 ,mydata$option6 ,mydata$option7 ,mydata$option8 ,mydata$option9  )
  
  x = c()
  y = c()
  for (i in 1:nrow(option)) {
    o = option[i,]
    s = which(o==opt)
    if (length(s)>0){
      for (j in 1:length(s)){
        p1 = payoff[i,s[j]]
        p2 = payoff[i,s[j]+1]
        x = append(x,p1)
        y = append(y,(p2-p1))
        
      }
    }
  }
  prob = length(which(y>=0))/length(y)
  print(prob)

  plot(x,y)
  # lines(x = c(0,20), y = c(0,20))
  
}


correlation.switch <- function(mydata) {
  
  payoff = data.frame(mydata$payoff1, mydata$payoff2 ,mydata$payoff3 ,mydata$payoff4, mydata$payoff5 ,mydata$payoff6 ,mydata$payoff7 ,mydata$payoff8 ,mydata$payoff9  )
  option = data.frame(mydata$option1, mydata$option2 ,mydata$option3 ,mydata$option4, mydata$option5 ,mydata$option6 ,mydata$option7 ,mydata$option8 ,mydata$option9  )
  
  x = c()
  y = c()
  for (i in 1:nrow(option)) {
    o = option[i,]
    s = which(o[2:9]==2)
    if (length(s)>0){
      for (j in 1:length(s)){
        p1 = max(unlist(payoff[i,1:s[j]]))
        # p1 = payoff[i,s[j]]
        p2 = payoff[i,s[j]+1]
        x = append(x,p1-p2)
        y = append(y,p2)
      }
    }
  }
  pos = length(which(x>0))/length(x)
  print(pos)
  plot(x,y)
  # lines(x = c(0,20), y = c(0,20))
  
}
mean.diff <- function(x,y){
  n =length(x)
  diff = c()
  if(n>1){
    for(i in 1:(n-1)){
      if(y[i]==2)
        diff = append(diff,x[i+1]-x[i])
    }
    if(length(diff)>=1)
      return(mean(diff))
    else
      return(0)
  }else
    return(0)
  
}

learn.diff <- function(x,y,a){
  n = length(x)
  switch = 10.0
  stay = 0.0
  if(n>1){
    for(i in 1:(n-1)){
      if(y[i]==2){
        switch = switch + a*((x[i+1]-x[i])-switch) 
      }
      else if(y[i]==1){
        stay = stay + a*((x[i+1]-x[i])-stay)
      }
    }
  }
  return (switch-stay)
  # return (switch)
  
}


learn.diff.2 <- function(x,y,a){
  n = length(x)
  switch = 1
  stay = 1
  if(n>1){
    for(i in 1:(n-1)){
      if(y[i]==2){
        delta = (x[i+1]-x[i])/(21-x[i])
        switch = switch + a*(delta-switch) 
      }
      else if(y[i]==1){
        delta = (x[i+1]-x[i])/(21-x[i])
        stay = stay + a*(delta-stay)
      }
    }
  }
  return (switch-stay)
  # return (switch)
  
}

normalize.payoff <- function(x){
  n =length(x)
  p = x[n]
  if (n>1){
    if(sd(x>0))
      return((p-mean(x))/sd(x))
    else
      return((p-mean(x)))
  }
  else{
    return((p-mean(x)))
  }
  
}


normalize.2.payoff <- function(x){
  n =length(x)
  p = x[n]
  if (n>1){
    return((p-min(x))/(max(x)-min(x)))
  }
  else{
    return((p-min(x)))
  }
  
}



learn.percentile <- function(x){
  n =length(x)
  p = x[n]
  n1 = 0.0
  n2 = 0.0
  for(i in 1:n){
    if(x[i]<p)
      n1 = n1+1
    else if(x[i]==p)
      n2 = n2+1
  }
  return ((n1+0.5*n2)/n)
}

correlation.test <- function(mydata) {
  
  payoff = data.frame(mydata$payoff1, mydata$payoff2 ,mydata$payoff3 ,mydata$payoff4, mydata$payoff5 ,mydata$payoff6 ,mydata$payoff7 ,mydata$payoff8 ,mydata$payoff9  )
  option = data.frame(mydata$option1, mydata$option2 ,mydata$option3 ,mydata$option4, mydata$option5 ,mydata$option6 ,mydata$option7 ,mydata$option8 ,mydata$option9  )
  condition = mydata$condition
  
  x1 = c()
  x2 = c()
  x3 = c()
  x4 =c()
  x5 = c()
  pn.s = c()
  pn.f = c()
  p.s = c()
  p.f = c()
  y=c()
  z=c()
  col = c()
  t = c()
  round = c()
  for (i in 1:nrow(option)) {
    opt = option[i,]
    opt = opt[!is.na(opt)]
    pay = payoff[i,]
    pay = pay[!is.na(pay)]
    if (length(opt)>2){
      for (j in 2:length(opt)){
        if (opt[j]<3){
          
          if (condition[i]=="fair"){
            cond = 0
            col = append(col,"blue")
          }
          else if(condition[i]=="self"){
            cond = 1
            col = append(col,"red")
          }
          z=append(z,cond)
          p1 = max(pay[1:max(1,(j-1))])
          # p1 = learn.percentile(pay[1:j])
          p3 = mean.diff(pay[1:j], opt[1:j])
          p4 = learn.diff.2(pay[1:j], opt[1:j],0.1)
         
          p5 = normalize.payoff(pay[1:j]) 
          hist = mean(opt[1:max(1,(j-1))])
          # hist = opt[max(1,(j-1))]
          # p1 = payoff[i,max(1,(j-1))]
          p2 = pay[j]
          dif = (p2-p1)
          
          x1 = append(x1,p1) #current pay
          x2 = append(x2,pay[max(1,(j-1))]) # previous pay
          
         
         
          x3 = append(x3, p5 )
          
          x4 = append(x4, p4 )
          x5 = append(x5, p4)
          y = append(y,opt[j]-1)
          t = append(t,hist-1)
          round = append(round,j)
          
          if (condition[i]=="fair"){
            pn.f = append(pn.f,p5)
            p.f = append(p.f,p2)
          }
          else if(condition[i]=="self"){
            pn.s = append(pn.s,p5)
            p.s = append(p.s,p2)
          }
          
        }
      }
    }
  }
 

  data <- data.frame(y = y, x1= x1, x2=x2, x3=x3, x4=x4, x5=x5, round=round, t=t, z= z)
  

  color_transparent <- adjustcolor(col, alpha.f = 0.3)   
  plot(x3, y, col=color_transparent, pch=20)
  # lines(x = c(0,20), y = c(0,20))
  
    mylogit <- glm(y ~ x1*x3+z  , data=data, family = "binomial")
  print(summary(mylogit))
  print(exp(cbind(OR = coef(mylogit), confint(mylogit))))

  
  # # sjp.glm(mylogit, type = "pred", show.ci = TRUE, vars=c("x","z"))
  # # sjp.glm(mylogit, type = "pred", facet.grid = FALSE, show.ci = TRUE, vars = "x")
  # yweight <- predict(mylogit, list(x1 = xweight, z=rep(0,length(xweight)), t = round(runif(length(xweight), min=0, max=1)) ), type="response")
  # lines(xweight, yweight, col="blue")
  # yweight <- predict(mylogit, list(x1 = xweight, z=rep(1,length(xweight)), t = round(runif(length(xweight), min=0, max=1)) ), type="response")
  # lines(xweight, yweight, col="red")
  
  # print(with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)))
  # print(wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 2:3))
  
  # xweight <- seq(min(data$x3), max(data$x3), 0.1)
  # # Create a temporary data frame of hypothetical values
  # temp.data <- data.frame(x3 = rep(xweight, 2), z = c(rep(0,length(xweight)), rep(1,length(xweight)) ) )
  # # Predict the fitted values given the model and hypothetical data
  # predicted.data <- as.data.frame(predict(mylogit, newdata = temp.data, type="link", se=TRUE))
  # # Combine the hypothetical data and predicted values
  # new.data <- cbind(temp.data, predicted.data)
  # # Calculate confidence intervals
  # std <- qnorm(0.95 / 2 + 0.5)
  # new.data$ymin <- mylogit$family$linkinv(new.data$fit - std * new.data$se)
  # new.data$ymax <- mylogit$family$linkinv(new.data$fit + std * new.data$se)
  # new.data$fit <- mylogit$family$linkinv(new.data$fit)  # Rescale to 0-1
  # 
  # # Plot everything
  # p <- ggplot(data, aes(x=x3, y=y, color=factor(z,labels=c("Fair","Selfish"))))
  # p = p  + geom_point() +
  #   geom_ribbon(data=new.data, aes(y=fit, ymin=ymin, ymax=ymax,
  #                                  fill=factor(z,labels=c("Fair","Selfish")) ), alpha=0.5) +
  #   geom_line(data=new.data, aes(y=fit, color=factor(z,labels=c("Fair","Selfish")))) +
  #   labs(x="Current payoff - Mean Payoff", y="Probability of Switching", color="Algorithm", fill="Algorithm")
  # print(p)
  
  # par(mfrow=c(2,2))
  # hist(p.f)
  # hist(p.s)
  # hist(pn.f)
  # hist(pn.s)
  # 


  
}





correlation.test.2 <- function(mydata) {
  
  payoff = data.frame(mydata$payoff1, mydata$payoff2 ,mydata$payoff3 ,mydata$payoff4, mydata$payoff5 ,mydata$payoff6 ,mydata$payoff7 ,mydata$payoff8 ,mydata$payoff9  )
  option = data.frame(mydata$option1, mydata$option2 ,mydata$option3 ,mydata$option4, mydata$option5 ,mydata$option6 ,mydata$option7 ,mydata$option8 ,mydata$option9  )
  condition = mydata$condition
  
  x1 = c()
  x2 = c()
  x3 = c()
  x4 =c()
  x5 = c()
  pn.s = c()
  pn.f = c()
  p.s = c()
  p.f = c()
  y=c()
  z=c()
  col = c()
  t = c()
  round = c()
  for (i in 1:nrow(option)) {
    opt = option[i,]
    opt = opt[!is.na(opt)]
    pay = payoff[i,]
    pay = pay[!is.na(pay)]

    for (j in 1:min(10,length(opt))){
      if (opt[j]<3){    
        if (condition[i]=="fair"){
          cond = 0
          col = append(col,"blue")
        }
        else if(condition[i]=="self"){
          cond = 1
          col = append(col,"red")
        }
       
        
        p1 = pay[j]
        # p1 = normalize.payoff(pay[1:j]) 
        # p1 = pay[j]-mean(pay[1:max(1,j)])
        p2= mean(pay[1:j])
        p3 = 0
        if(j>1){
          p3 = opt[j-1]-1
        }
        
       
     
        x1 = append(x1,p1) #current pay
        x2 = append(x2,p2) #mean pay
        x3 = append(x3,p3) #last action
        x4 = append(x4,j) #round #
        y = append(y,opt[j]-1) #option chosen
        z = append(z,cond) #condition
  
      }
    }
    
  }
  
  data <- data.frame(y = y, x1= x1,x2=x2,x3=x3, x4=x4, z= z)
  
  
  color_transparent <- adjustcolor(col, alpha.f = 0.3)   
  plot(x1, y, col=color_transparent, pch=20)
  # lines(x = c(0,20), y = c(0,20))
  
  mylogit <- glm(y ~ x1*x2  , data=data, family = "binomial")
  print(summary(mylogit))
  print(exp(cbind(OR = coef(mylogit), confint(mylogit))))
  
  
  # # sjp.glm(mylogit, type = "pred", show.ci = TRUE, vars=c("x","z"))
  # # sjp.glm(mylogit, type = "pred", facet.grid = FALSE, show.ci = TRUE, vars = "x")
  # yweight <- predict(mylogit, list(x1 = xweight, z=rep(0,length(xweight)), t = round(runif(length(xweight), min=0, max=1)) ), type="response")
  # lines(xweight, yweight, col="blue")
  # yweight <- predict(mylogit, list(x1 = xweight, z=rep(1,length(xweight)), t = round(runif(length(xweight), min=0, max=1)) ), type="response")
  # lines(xweight, yweight, col="red")
  
  # print(with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)))
  # print(wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 2:3))
  
  # xweight <- seq(min(data$x3), max(data$x3), 0.1)
  # # Create a temporary data frame of hypothetical values
  # temp.data <- data.frame(x3 = rep(xweight, 2), z = c(rep(0,length(xweight)), rep(1,length(xweight)) ) )
  # # Predict the fitted values given the model and hypothetical data
  # predicted.data <- as.data.frame(predict(mylogit, newdata = temp.data, type="link", se=TRUE))
  # # Combine the hypothetical data and predicted values
  # new.data <- cbind(temp.data, predicted.data)
  # # Calculate confidence intervals
  # std <- qnorm(0.95 / 2 + 0.5)
  # new.data$ymin <- mylogit$family$linkinv(new.data$fit - std * new.data$se)
  # new.data$ymax <- mylogit$family$linkinv(new.data$fit + std * new.data$se)
  # new.data$fit <- mylogit$family$linkinv(new.data$fit)  # Rescale to 0-1
  # 
  # # Plot everything
  # p <- ggplot(data, aes(x=x3, y=y, color=factor(z,labels=c("Fair","Selfish"))))
  # p = p  + geom_point() +
  #   geom_ribbon(data=new.data, aes(y=fit, ymin=ymin, ymax=ymax,
  #                                  fill=factor(z,labels=c("Fair","Selfish")) ), alpha=0.5) +
  #   geom_line(data=new.data, aes(y=fit, color=factor(z,labels=c("Fair","Selfish")))) +
  #   labs(x="Current payoff - Mean Payoff", y="Probability of Switching", color="Algorithm", fill="Algorithm")
  # print(p)
  
  # par(mfrow=c(2,2))
  # hist(p.f)
  # hist(p.s)
  # hist(pn.f)
  # hist(pn.s)
  # 

}





correlation.test.normalize <- function(mydata) {
  
  payoff = data.frame(mydata$payoff1, mydata$payoff2 ,mydata$payoff3 ,mydata$payoff4, mydata$payoff5 ,mydata$payoff6 ,mydata$payoff7 ,mydata$payoff8 ,mydata$payoff9  )
  option = data.frame(mydata$option1, mydata$option2 ,mydata$option3 ,mydata$option4, mydata$option5 ,mydata$option6 ,mydata$option7 ,mydata$option8 ,mydata$option9  )
  condition = mydata$condition
  
  x1 = c()
  y = c()
  z = c()
  col = c()
  for (i in 1:nrow(option)) {
    opt = option[i,]
    opt = opt[!is.na(opt)]
    pay = payoff[i,]
    pay = pay[!is.na(pay)]
    if (length(opt)>1){
      for (j in 1:length(opt)){
        if (opt[j]<3){
          
          if (condition[i]=="fair"){
            cond = 0
            col = append(col,"blue")
          }
          else if(condition[i]=="self"){
            cond = 1
            col = append(col,"red")
          }
          z=append(z,cond)
          
          p = normalize.payoff(pay[1:j]) 
          x1 = append(x1,p) #current normalize pay
          
          y = append(y,opt[j]-1)

          
        }
      }
    }
  }
  
 
  data <- data.frame(y = y, x1= x1, z= z)
  
  
  color_transparent <- adjustcolor(col, alpha.f = 0.3)   
  plot(x1, y, col=color_transparent, pch=20)
  # lines(x = c(0,20), y = c(0,20))
  
  mylogit <- glm(y ~ x1+z  , data=data, family = "binomial")
  print(summary(mylogit))
  print(exp(cbind(OR = coef(mylogit), confint(mylogit))))
  
  
  # # sjp.glm(mylogit, type = "pred", show.ci = TRUE, vars=c("x","z"))
  # # sjp.glm(mylogit, type = "pred", facet.grid = FALSE, show.ci = TRUE, vars = "x")
  # yweight <- predict(mylogit, list(x1 = xweight, z=rep(0,length(xweight)), t = round(runif(length(xweight), min=0, max=1)) ), type="response")
  # lines(xweight, yweight, col="blue")
  # yweight <- predict(mylogit, list(x1 = xweight, z=rep(1,length(xweight)), t = round(runif(length(xweight), min=0, max=1)) ), type="response")
  # lines(xweight, yweight, col="red")
  
  # print(with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)))
  # print(wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 2:3))
  
  xweight <- seq(min(data$x1)-1, max(data$x1)+1, 0.1)
  # Create a temporary data frame of hypothetical values
  temp.data <- data.frame(x1 = rep(xweight, 2), z = c(rep(0,length(xweight)), rep(1,length(xweight)) ) )
  # Predict the fitted values given the model and hypothetical data
  predicted.data <- as.data.frame(predict(mylogit, newdata = temp.data, type="link", se=TRUE))
  # Combine the hypothetical data and predicted values
  new.data <- cbind(temp.data, predicted.data)
  # Calculate confidence intervals
  std <- qnorm(0.95 / 2 + 0.5)
  new.data$ymin <- mylogit$family$linkinv(new.data$fit - std * new.data$se)
  new.data$ymax <- mylogit$family$linkinv(new.data$fit + std * new.data$se)
  new.data$fit <- mylogit$family$linkinv(new.data$fit)  # Rescale to 0-1
  
  # Plot everything
  p <- ggplot(data, aes(x=x1, y=y, color=factor(z,labels=c("Fair","Selfish"))) )
  p = p  + geom_point(alpha=0.5) +
    geom_ribbon(data=new.data, aes(y=fit, ymin=ymin, ymax=ymax,
                                   fill=factor(z,labels=c("Fair","Selfish")) ), alpha=0.5) +
    geom_line(data=new.data, aes(y=fit, color=factor(z,labels=c("Fair","Selfish")))) +
    labs(x="Normalize Payoff", y="Probability of Switching", color="Algorithm", fill="Algorithm")
  print(p)
  
}

#replace Nas in payoff with outside payment of 6 cents
add.outside_payment <- function(mydata){
  
  mydata.op = data.frame(mydata)
  
  mydata.op$payoff1[is.na(mydata.op$payoff1)] <- 6
  mydata.op$payoff2[is.na(mydata.op$payoff2)] <- 6
  mydata.op$payoff3[is.na(mydata.op$payoff3)] <- 6
  mydata.op$payoff4[is.na(mydata.op$payoff4)] <- 6
  mydata.op$payoff5[is.na(mydata.op$payoff5)] <- 6
  mydata.op$payoff6[is.na(mydata.op$payoff6)] <- 6
  mydata.op$payoff7[is.na(mydata.op$payoff7)] <- 6
  mydata.op$payoff8[is.na(mydata.op$payoff8)] <- 6
  mydata.op$payoff9[is.na(mydata.op$payoff9)] <- 6
  mydata.op$payoff10[is.na(mydata.op$payoff10)] <- 6
  
  return(mydata.op)
  
}

read.data <- function(study) {
  # Load data.
  file_name = paste("../data/", study,".csv", sep="")
  mydata.S <- read.csv(file=file_name,head=T,sep=c(","))
  
  # Extract interesting (+ descriptive) columns
  cols <- c("participant.id_in_session","session.code","participant.visited","participant.payoff","matchingAlg.1.group.id_in_subsession","matchingAlg.1.group.super_group","matchingAlg.1.group.alg","matchingAlg.1.player.sm_options",
            "matchingAlg.1.player.current_slot_machine_id","matchingAlg.1.player.payoff_current", "matchingAlg.1.player.offer_accepted","matchingAlg.1.subsession.round_number",
            "matchingAlg.2.player.current_slot_machine_id","matchingAlg.2.player.payoff_current", "matchingAlg.2.player.offer_accepted","matchingAlg.2.subsession.round_number",
            "matchingAlg.3.player.current_slot_machine_id","matchingAlg.3.player.payoff_current", "matchingAlg.3.player.offer_accepted","matchingAlg.3.subsession.round_number", 
            "matchingAlg.4.player.current_slot_machine_id","matchingAlg.4.player.payoff_current", "matchingAlg.4.player.offer_accepted","matchingAlg.4.subsession.round_number", 
            "matchingAlg.5.player.current_slot_machine_id","matchingAlg.5.player.payoff_current", "matchingAlg.5.player.offer_accepted","matchingAlg.5.subsession.round_number", 
            "matchingAlg.6.player.current_slot_machine_id","matchingAlg.6.player.payoff_current", "matchingAlg.6.player.offer_accepted","matchingAlg.6.subsession.round_number", 
            "matchingAlg.7.player.current_slot_machine_id","matchingAlg.7.player.payoff_current", "matchingAlg.7.player.offer_accepted","matchingAlg.7.subsession.round_number", 
            "matchingAlg.8.player.current_slot_machine_id","matchingAlg.8.player.payoff_current", "matchingAlg.8.player.offer_accepted","matchingAlg.8.subsession.round_number", 
            "matchingAlg.9.player.current_slot_machine_id","matchingAlg.9.player.payoff_current", "matchingAlg.9.player.offer_accepted","matchingAlg.9.subsession.round_number", 
            "matchingAlg.10.player.current_slot_machine_id","matchingAlg.10.player.payoff_current","matchingAlg.10.subsession.round_number", 
            "survey.1.player.secret_code","survey.1.player.mTurk_id")
  mydata2.S <- mydata.S[,cols]
  
  # Rename columns
  colnames(mydata2.S) <- c("id","session.code","visited","total_payoff","role_id","group_id","condition","ini_payoff",
                           "slot1","payoff1","option1","round1","slot2","payoff2","option2","round2", "slot3","payoff3","option3","round3", 
                           "slot4","payoff4","option4","round4", "slot5","payoff5","option5","round5", "slot6","payoff6","option6","round6",
                           "slot7","payoff7","option7","round7",  "slot8","payoff8","option8","round8", "slot9","payoff9","option9","round9",
                           "slot10","payoff10","round10", "code", "mturk_id")
  
  
  # Remove players that timeout and didnt finished
  mydata3.S <- mydata2.S[mydata2.S[,"visited"]!="0",]
  mydata4.S <- mydata3.S[is.na(mydata3.S[,"option1"])==0,]
  mydata4.S <- mydata4.S[mydata4.S[,"mturk_id"]!="",]
  # mydata4.S <- mydata4.S[mydata4.S[,"option1"]!=3,]
  
  
  #Remove groups without two condititions
  groups = aggregate(list(condition=mydata4.S$condition ), by=list( group_id=mydata4.S$group_id, session.code=mydata4.S$session.code), FUN=count_conditions)
  groups = groups[groups[,"condition"]==TRUE,]
  mydata4.S=subset(mydata4.S, paste(mydata4.S$group_id,mydata4.S$session.code) %in% paste(groups$group_id, groups$session.code))
  
  
  return(mydata4.S)

}

plot.study <- function(study){
  
  #load data
  mydata = read.data(study)
  
  mydataself <- mydata[mydata$"condition"=="self",]
  mydatafair <- mydata[mydata$"condition"=="fair",]
  
  #replace Nas in payoff with outside payment of 6 cents
  mydata.op = add.outside_payment(mydata)
  mydataself.op <- mydata.op[mydata.op$"condition"=="self",]
  mydatafair.op <- mydata.op[mydata.op$"condition"=="fair",]
  
  
  ###################### PLOTS #########################
  
  #Distribution of payoffs
  file_name = paste("../Figs/dist_", study,".pdf", sep="")
  pdf(file=file_name, width=10, height=8)
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE) )
  par(mar=c(5,5,4,2)+.2)
  pay = universal.dist(mydata.op)
  dist.payoff(mydataself.op, mydatafair.op)
  dev.off() 
  
  
  #Social Welfare
  file_name = paste("../Figs/SW_", study,".pdf", sep="")
  pdf(file=file_name, width=8, height=6)
  par(mfrow=c(2,2))
  par(mar=c(5,5,4,2)+.2)
  layout(matrix(c(1,2,2), nrow = 1, ncol = 3, byrow = TRUE))
  SW.mean(mydataself.op, mydatafair.op)
  SW.mean.stage(mydataself.op, mydatafair.op)
  dev.off() 
  
  
  #Engagement Rates
  file_name = paste("../Figs/Engagement_", study,".pdf", sep="")
  pdf(file=file_name, width=8, height=6)
  par(mfrow=c(2,2))
  par(mar=c(5,5,4,2)+.2)
  layout(matrix(c(1,2,2), nrow = 1, ncol = 3, byrow = TRUE))
  retention.rates.overall(mydataself.op, mydatafair.op)
  retention.rates.period(mydataself.op, mydatafair.op)
  dev.off() 
  
  
  #Drop Rate
  file_name = paste("../Figs/Drop_", study,".pdf", sep="")
  pdf(file=file_name, width=6, height=8)
  par(mfrow=c(1,1))
  drop.rates.overall(mydataself.op, mydatafair.op)
  dev.off() 
  
  

  #Prob. of switching
  #Plot aggregated q for each algortihm
  # p1 <- plot.prob.dist(mydatafair)
  # p2 <- plot.prob.dist(mydataself)
  # par(mfrow=c(1,2))
  # barplot(p1,names.arg=c("0-3","4-6","7-9","10-12","13-15","16-18","19-20"), ylim=c(0,1), main="Fair Algorithm", xlab="payoff")
  # barplot(p2,names.arg=c("0-3","4-6","7-9","10-12","13-15","16-18","19-20"), ylim=c(0,1), main="Selfish Algorithmn",xlab="payoff")
  
  #Plot by stages
  file_name = paste("../Figs/Switch_Fair_", study,".pdf", sep="")
  pdf(file=file_name, width=8, height=5)
  par(mfrow=c(1,3))
  plot.prob.dist.stage.3(mydatafair)
  dev.off()
  
  file_name = paste("../Figs/Switch_Selfish_", study,".pdf", sep="")
  pdf(file=file_name, width=8, height=5)
  par(mfrow=c(1,3))
  plot.prob.dist.stage.3(mydataself)
  dev.off()
  
  # par(mfrow=c(1,1))
  # correlation.test.2(mydata4.S)
  # correlation.switch(mydataself)
  
  #Price of Anarchy
  file_name = paste("../Figs/PoA_", study,".pdf", sep="")
  pdf(file=file_name, width=5, height=8)
  par(mfrow=c(1,1))
  par(mar=c(5,5,4,2)+.2)
  poa = poa.total(mydataself, mydatafair)
  dev.off()
  
  
  
}


#plot all figures

plot.study("study_a")
plot.study("study_b")
plot.study("study_c")





