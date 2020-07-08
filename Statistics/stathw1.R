?binom.test
binom.test(x,10,0.3)
?rbern

m_list<-c()
for (i in 1:100)
{m<-rbinom(10,1,0.3)
sum(m)
m_list[i]<-sum(m)
}
mean(m_list)

n_list<-c()
for (i in 1:100)
{n<-rbinom(100,1,0.3)
sum(n)
n_list[i]<-sum(n)
}
mean(n_list)

l_list<-c()
for (i in 1:100)
{l<-rbinom(1000,1,0.3)
sum(l)
l_list[i]<-sum(l)
}
mean(l_list)

?rbinom
?rpois

set.seed(1)

N1<-rpois(1000,10) 
X<-rbinom(1000,N1,0.3)


set.seed(1)
N2<-rpois(1000,10)
Y<-rbinom(1000,N2,0.7)

plot(X,Y, main='Scatter Plot')
cor(X,Y)

##COVID19 Simulation

data<-matrix(c(1:2000),100,20)
for (j in 1:100){ 
  data[j,1]<-5
  for (i in 1:19){
    
    poi<-rpois(1,data[j,i]*10)
    new_infected<-rbinom(1,poi,0.2)
    data[j,i+1]<-new_infected
    }
}
boxplot(data,main='New Infection by Time Unit',use.cols = T)


data1<-matrix(c(1:2000),100,20)
for (j in 1:100){ 
  data1[j,1]<-5
  for (i in 1:19){
    if (i<10){
    poi<-rpois(1,data1[j,i]*10)
    new_infected<-rbinom(1,poi,0.2)
    data1[j,i+1]<-new_infected
    }
    else{
      poi<-rpois(1,data1[j,i]*10)
      new_infected<-rbinom(1,poi,0.14)
      data1[j,i+1]<-new_infected
    }
  }
}
boxplot(data1,main='New Infection by Time Unit(Mask)',use.cols = T)

data2<-matrix(c(1:2000),100,20)
for (j in 1:100){ 
  data2[j,1]<-5
  for (i in 1:19){
    if (i<10){
      poi<-rpois(1,data2[j,i]*10)
      new_infected<-rbinom(1,poi,0.2)
      data2[j,i+1]<-new_infected
    }
    else if(i>11){
      poi<-rpois(1,data2[j,i]*3)
      new_infected<-rbinom(1,poi,0.14)
      data2[j,i+1]<-new_infected
      
    }
    else {
      poi<-rpois(1,data2[j,i]*10)
      new_infected<-rbinom(1,poi,0.14)
      data2[j,i+1]<-new_infected
    }
  }
}
boxplot(data2,main='New Infection by Time Unit(Mask+Social distancing)',use.cols = T)
