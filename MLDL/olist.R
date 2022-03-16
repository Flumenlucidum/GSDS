library(dplyr)

setwd("C:/Users/main/Desktop/2021-1/강화학습")
item<-read.csv('item_with_order.csv')
order<-read.csv('olist_orders_dataset.csv')
products<-read.csv('olist_products_dataset.csv')

for(i in 1:nrow(item)){
  item[i,3]<-paste0('product_',as.character(which(products$product_id %in% item[i,3])))
}

products$product_id_new<-1:nrow(products)
products$product_id_new<-paste0('product_',as.character(products$product_id_new))
products<-products[c(10,1:9)]

order_by_time<-order[order(order$order_purchase_timestamp),]
table(order_by_time$order_status)
order_by_time<-order_by_time[which(order_by_time$order_status!='canceled' & order_by_time$order_status!='unavailable'),]
dim(products)[1]
order_by_time$product_1<-0
for(i in 1:dim(products)[1]){
  order_by_time[[paste0('product_',as.character(i))]]<-0
}
c<-unique(order_by_time$day)

as.Date(order_by_time$order_purchase_timestamp[1])
order_by_time$day<-as.Date(order_by_time$order_purchase_timestamp)

for(i in 1:length(days)){

  test<-table(item[which(item$order_id %in% order$order_id[which(order$day==days[i])]),9])
  for(j in 1:length(test)){
    ee[i,as.numeric(names(test)[j])] <- test[[names(test)[j]]]
  }
}


aa<-read.csv('product_info.csv')
View(aa)

tt<-read.csv('timeline_V2.csv')
a<-rowSums(tt)
n<-rpois(n=999,lambda = 179.9511)
sum(a)

d<-rpois(1000,179.9511)+rbinom(1000,1,1/614)*1000
hist(d)
c<-d[which(d<400)]
hist(c)
#rare spike in demand 
hist(b)
b<-a[which(a<600)]
success <- 0:1400
mean(a)
#create plot of probability mass function
plot(success, dpois(success, lambda=179.9511), type='l')
y<-dpois(success, lambda=179.9511)
y[1400]  
hist(rbeta(10000,2,5)*1000)


install.packages('fitdistrplus')
library(fitdistrplus)
plotdist(a,histo=T,demp=T)
hist(a)
dp<-fitdist(a2,'pois')
str(dd)
dt<-fitdist(a,'t')
str(dp)
a2<-a/max(a)
lambda <- 0.5
tMax <- 100
max(a)
a2<-a/mean(a)
#rpois(10000,1)*mean(a)
hist(rbeta(1000,2.45, 15.97)*max(a))

hist(rpois(10000,1)*mean(a))

hist(rpois(1000,179.9511)+rbinom(1000,1,1/614)*1000)

10000/60
