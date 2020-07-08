#number2
?rnorm
c<-rnorm(100,0,1)
c
plot(c)
f<-density(rnorm(100,0,1))
f
plot(f)
library(ggplot2)
library(dplyr)
library(gridExtra)

a<-c(1:1000)
b<-rep(0,1000)
test<-cbind(a,b)
test

nSim<-1000
for (j in c(1:nSim)){
  c<-rnorm(100,0,1)
  x_range<-seq(min(c),max(c),length.out = 1000)
  
  ecdfnorm<-ecdf(c)
  lower<-rep(0,1000)
  upper<-rep(0,1000)
  for (i in c(1:1000)){
    lower[i]<-max(ecdfnorm(x_range)[i]-sqrt((1/200)*log(2/0.05)),0)
    upper[i]<-min(ecdfnorm(x_range)[i]+sqrt((1/200)*log(2/0.05)),1)
  }
  a<-rep(0,1000)
  for (k in c(1:1000)){
    a[k]<-lower[k]<=pnorm(x_range[k],0,1) & pnorm(x_range[k],0,1)<=upper[k]
  }
  g<-rep(1,1000)
  if(identical(a,g)){
    test[j,2]<-1
  }
  else{}
}
test

colSums(test)

c<-rnorm(100,0,1)
x_range<-seq(min(c),max(c),length.out = 1000)

ecdfnorm<-ecdf(c)
lower<-rep(0,1000)
upper<-rep(0,1000)
for (i in c(1:1000)){
  lower[i]<-max(ecdfnorm(x_range)[i]-sqrt((1/200)*log(2/0.05)),0)
  upper[i]<-min(ecdfnorm(x_range)[i]+sqrt((1/200)*log(2/0.05)),1)
}
a<-rep(0,1000)
for (k in c(1:1000)){
  a[k]<-lower[k]<=pnorm(x_range[k],0,1) & pnorm(x_range[k],0,1)<=upper[k]
}
plot(x_range,pnorm(x_range,0,1))
g<-rep(1,1000)
identical(a,g)
pnorm(1.96,0,1)
points(x_range,lower,col='red')
points(x_range,upper,col='blue')


#number 3
#generating log normal
y<-rnorm(50,0,1)
x<-exp(y)


#Three types of bootstrap interval
#repeating of bootstrap
B<-1000
Tboot<-rep(0,B)
for (i in c(1:B)){
  xstar<-sample(x, replace = TRUE)
  Tboot[i]<-var(xstar)
}
Tboot

#percentile Interval
perint<-c(quantile(Tboot,0.025), quantile(Tboot,0.975))



#normal interval
t_hat<-var(x)
se<-sqrt(var(Tboot))
norint<-c(t_hat-1.96*se,t_hat+1.96*se)


#pivotal interval
pivint<-c(2*t_hat-quantile(Tboot,0.975),2*t_hat-quantile(Tboot,0.025))


install.packages('intervals')
install.packages('IRanges')
pivintlist<-matrix(0,100,2)
perintlist<-matrix(0,100,2)
norintlist<-matrix(0,100,2)
y<-rnorm(50,0,1)
x<-exp(y)
nSim<-100
for (j in c(1:nSim)){
  y<-rnorm(50,0,1)
  x<-exp(y)
  B<-1000
  Tboot<-rep(0,B)
  sample(x, replace = TRUE)
  for (i in c(1:B)){
    xstar<-sample(x, replace = TRUE)
    Tboot[i]<-var(xstar)
  }
  
  #percentile Interval
  perint<-c(quantile(Tboot,0.025), quantile(Tboot,0.975))
  perintlist[j,1]<-perint[1]
  perintlist[j,2]<-perint[2]
  #normal interval
  t_hat<-var(x)
  se<-sqrt(var(Tboot))
  norint<-c(t_hat-1.96*se,t_hat+1.96*se)
  norintlist[j,1]<-norint[1]
  norintlist[j,2]<-norint[2]
  
  #pivotal interval
  pivint<-c(2*t_hat-quantile(Tboot,0.975),2*t_hat-quantile(Tboot,0.025))
  pivintlist[j,1]<-pivint[1]
  pivintlist[j,2]<-pivint[2]
  
}

perintlistfin<-cbind(c(1:100),perintlist)

norintlistfin<-cbind(c(1:100),norintlist)

pivintlistfin<-cbind(c(1:100),pivintlist)

plot(perintlistfin[,3],perintlistfin[,1],type='l', xlim =c(0,20) )
lines(perintlistfin[,2],perintlistfin[,1],type='l',col='red')

plot(norintlistfin[,3],norintlistfin[,1],type='l', xlim =c(0,20) )
lines(norintlistfin[,2],norintlistfin[,1],type='l',col='red')

plot(pivintlistfin[,3],pivintlistfin[,1],type='l', xlim =c(0,20) )
lines(pivintlistfin[,2],pivintlistfin[,1],type='l',col='red')

real<-(exp(1)-1)*exp(1)
pivscore<-0
perscore<-0
norscore<-0
for (j in c(1:100)){
  if (pivintlist[j,1]<=real & pivintlist[j,2]>=real){
    pivscore<-pivscore+1  
  }
  else{}
}
pivscore
for (j in c(1:100)){
  if (perintlist[j,1]<=real & perintlist[j,2]>=real){
    perscore<-perscore+1  
  }
  else{}
}
for (j in c(1:100)){
  if (norintlist[j,1]<=real & norintlist[j,2]>=real){
    norscore<-pivscore+1  
  }
  else{}
}
pivscore
perscore
norscore

#4
x<-rnorm(100,5,1)
B<-1000
Tboot<-rep(0,1000)
for (i in c(1:B)){
  
  xstar<-sample(x,replace=TRUE)
  Tboot[i]<-exp(mean(xstar))
}
Tboot
se<-sqrt(var(Tboot))
perint<-c(quantile(Tboot,0.025),quantile(Tboot,0.975))
perint

theta_hat<-exp(mean(x))
theta_hat
norint<-c(theta_hat-1.96*se,theta_hat+1.96*se)
norint

pivint<-c(2*theta_hat-quantile(Tboot,0.975),2*theta_hat-quantile(Tboot,0.025))
pivint

hist(Tboot)

x<-c(100:200)

y<-(1/(x*(1/10)*sqrt(2*pi)))*exp(-((log(x)-5)^2)/(2*(1/100)))
plot(x,y,type='l',main='True Sampling Distribution')

#6
#call data
df<-read.table('coris.txt',sep=',',header = TRUE)
df
View(df)

#change chd as a factor
df$chd<-factor(df$chd)
table(df$chd)

hist(df$tobacco,main='Tobacco distribution')
qqnorm(df$tobacco)
qqline(df$tobacco,col='red')

df$logtob<-log(df$tobacco+0.1)
hist(log(df$tobacco), main='logtob')

a<-boxplot(df$logtob~df$chd, main='logtob by CHD')
a
library(dplyr)

df0<-df%>%filter(chd==0)
View(df0)  

df1<-df%>%filter(chd==1)
median(df1$logtob)

B<-1000
Tboot<-rep(0,B)
for (i in c(1:B)){
  xstar<-sample(df0$logtob, replace = TRUE)
  Tboot[i]<-median(xstar)
}
Tboot

#percentile Interval
perint<-c(quantile(Tboot,0.025), quantile(Tboot,0.975))



#normal interval
t_hat<-median(df0$logtob)
se<-sqrt(var(Tboot))
norint<-c(t_hat-1.96*se,t_hat+1.96*se)


#pivotal interval
pivint<-c(2*t_hat-quantile(Tboot,0.975),2*t_hat-quantile(Tboot,0.025))

perint
norint
pivint


B<-1000
Tboot<-rep(0,B)
for (i in c(1:B)){
  xstar<-sample(df1$logtob, replace = TRUE)
  Tboot[i]<-median(xstar)
}
Tboot

#percentile Interval
perint<-c(quantile(Tboot,0.025), quantile(Tboot,0.975))



#normal interval
t_hat<-median(df1$logtob)
se<-sqrt(var(Tboot))
norint<-c(t_hat-1.96*se,t_hat+1.96*se)


#pivotal interval
pivint<-c(2*t_hat-quantile(Tboot,0.975),2*t_hat-quantile(Tboot,0.025))

perint
norint
pivint

#median difference
meddif<-abs(median(df1$logtob)-median(df0$logtob))
meddif
B<-1000
Tboot<-rep(0,B)
for (i in c(1:B)){
  xstar<-sample(df0$logtob, replace = TRUE)
  ystar<-sample(df1$logtob, replace = TRUE)
  Tboot[i]<-abs(median(ystar)-median(xstar))
}
Tboot
se<-sqrt(var(Tboot))
se

#mean difference
meandif<-abs(mean(df1$logtob)-mean(df0$logtob))
meandif
B<-1000
Tboot<-rep(0,B)
for (i in c(1:B)){
  xstar<-sample(df0$logtob, replace = TRUE)
  ystar<-sample(df1$logtob, replace = TRUE)
  Tboot[i]<-abs(mean(ystar)-mean(xstar))
}
Tboot
se<-sqrt(var(Tboot))
se
