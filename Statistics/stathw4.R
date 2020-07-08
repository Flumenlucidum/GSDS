#1 d
nSim<-1000
pboot<-rep(0,nSim)
pboot
for (i in c(1:1000)){
  x<-rbinom(200,1,0.8)

  y<-rbinom(200,1,0.74)

  pboot[i]<-sum(x)/200-sum(y)/200
}
pboot
square<-rep(0,nSim)
for (i in c(1:1000)){
  square[i]<-(pboot[i]-0.06)^2
}
seboot<-sqrt(sum(square)/nSim)
seboot
c(0.06-seboot*1.645,0.06+1.645*seboot)


qnorm(0.975,0,1)


#3
nSim<-1000
rejectnum<-0
for (i in c(1:nSim)){
  data<-rpois(20,1)
  stat<-(mean(data)-1)/sqrt(mean(data)/20)
  if (abs(stat)>1.96){
    rejectnum<-rejectnum+1
  }
}
rejectnum

#4
profdata<-RateMyProfessor
profdata[is.na(profdata)]<-0
Pvaluelist<-rep(0,20)

for (i in c(4:23)){
  prof0<-profdata$star_rating[profdata[,i]==0]
  prof1<-profdata$star_rating[profdata[,i]==1]
  Mu0<-mean(prof0)
  Mu1<-mean(prof1)

  n0<-length(prof0)
  n1<-length(prof1)
  
  var0<-sum((prof0-Mu0)^2)/n0
  var1<-sum((prof1-Mu1)^2)/n1
  SE<-sqrt(var0/n0+var1/n1)
  Theta<-abs(Mu1-Mu0)
  W<-Theta/SE
  Pvalue<-pchisq(W^2,df=1,lower.tail=FALSE)
  Pvaluelist[i-3]<-Pvalue
}
Pvaluelist
Result<-cbind(colnames(profdata)[4:23],as.numeric(Pvaluelist))
View(Result)


#(b)

0.05/20

Result<-cbind(Result,rep(0,20))
for (i in c(1:20)){
  Result[i,3]<-as.logical(as.numeric(Result[i,2])<=0.0025)
} 
View(Result)

#(c)
order<-order(Pvaluelist)
Pvalue_ordered<-Pvaluelist[order]
Pvalue_ordered
Resultordered<-Result[order,]
View(Resultordered)
ResultBH<-cbind(Resultordered,rep(0,20))
View(ResultBH)
for (i in c(1:20)){
  ResultBH[i,4]<-as.logical(as.numeric(ResultBH[i,2])<=0.0025*i)
}
0.0025*13

#(d)
nper<-1000000
perlist<-rep(0,nper)
#same process for all variables
prof0<-profdata$star_rating[profdata$beware_of_pop_quizzes==0]
prof1<-profdata$star_rating[profdata$beware_of_pop_quizzes==1]
Mu0<-mean(prof0)
Mu1<-mean(prof1)
for (i in c(1:nper)){
  prof.perm<-sample(profdata$beware_of_pop_quizzes)
  Mu0.perm<-mean(profdata$star_rating[prof.perm==0])
  Mu1.perm<-mean(profdata$star_rating[prof.perm==1])
  perlist[i]<-abs(Mu1.perm-Mu0.perm)
}
perlist
theta<-abs(Mu1-Mu0)
theta
plist_perm<-mean(theta<=perlist)
plist_perm
plist.perm[20]<-plist_perm
plist.perm

View(Result)
permu_sig<-rep(0,20)
order_perm<-order(plist.perm)
plist.perm_ordered<-plist.perm[order_perm]
plist.perm_ordered
Resultordered_perm<-Result[order_perm,]
View(Resultordered_perm)
Resultordered_perm<-Resultordered_perm[,1:3]
Resultordered_perm<-cbind(Resultordered_perm,plist.perm_ordered)
for (i in c(1:20)){
  permu_sig[i]<-as.logical(as.numeric(Resultordered_perm[i,4])<=0.0025*i)
  }
permu_sig
Resultordered_perm<-cbind(Resultordered_perm,permu_sig)
Resultordered_perm[1:13,1]



a<-c(1.1,0.57,0.02,0.31,1.2)
var(a)
sqrt(var(a)*sqrt(5))
