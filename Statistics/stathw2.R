data_matrix<-matrix(0,1000,21)
N_sim<-1000
rbinom(1,1,0.5)
c<-20000
for (i in 1:N_sim){
  data_matrix[i,1]<-c
  for (j in 2:21){
    
    a<-rbinom(1,1,0.5)
    if (a==0){
    data_matrix[i,j]<-data_matrix[i,j-1]*1/2
    }
    else if(a==1){
      data_matrix[i,j]<-data_matrix[i,j-1]*2
    }
  }
}
data_matrix
mean(data_matrix[,1])
((5/4)^20)*20000
2^3




N_sim<-10000
money_sum<-matrix(c(1:20000),10000,2,byrow=F)


for (i in 1:10000){
  
  if (i==1){
  b<-rbinom(1,1,0.5)
  if (b==0){
    money_sum[1,2]<-(-1)
  }
  else if(b==1){
    money_sum[1,2]<-1
  }
  }
  else{
  a<-rbinom(1,1,0.5)
  if (a==0){
  money_sum[i,2]<-money_sum[i-1,2]-1
  }
  else if(a==1){
    money_sum[i,2]<-money_sum[i-1,2]+1
  }
  }
}
plot(money_sum, type='l')
rm(list=ls())
library(ggplot2)
ggplot(data.frame(x=c(-2,2)),aes(x=x)+stat_function(fun=dunif, args=list(min=0,max=1),colour='black',size=1)+ggtitle('Uniform Distribution'))
?ggplot   #wrong one 
ggplot(data.frame(x=c(-2,2)), aes(x=x)) +stat_function(fun=dunif, args=list(min = 0, max = 1), colour="black", size=1) +
ggtitle("Uniform Distribution")
?rep
n<-c(1:100)
y<-rep(1/2,100 )
plot(n,y, type='l',main='Plot of expected sample mean')
?plot
z<-(1/(12*n))
plot(n,z, type='l',main='Plot of Variance of sample mean')

par(mfrow=c(1,1))
num<-rep(0,1000)
s<-matrix(0,4,4)

for (i in 1:1000){
n<-100
x<-runif(n,0,1)
sample_mean<-sum(x)/n
num[i]<-sample_mean

}
s[4,1]<-n
s[4,2]<-mean(num)
s[4,3]<-var(num)
s[4,4]<-1/(12*n)

hist(num,breaks=seq(0,1,by=0.01), main='Plot of Sample mean(n=1,5,25,100)')
s
?hist
s
