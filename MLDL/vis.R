Time
plot(Time$date,Time$test,type='l',col='blue')
plot(Time$date,Time$confirmed,type='l',col='red')
plot(Time$date,Time$~,type='l')
library(ggplot2)
install.packages('reshape')
library(reshape)
data <- data.frame(Time)
Molten <- melt(data, id.vars = "date")
ggplot(Molten, aes(x = date, y = value, colour = variable)) + geom_line()
lines(Timein$date,Timein$Infected,type='l',col='red',lty=1)
lines(Timein$date,Timein$released,type='l',col='blue')
plot(Timein$date,Timein$confirmed,type='l',col='green')
Timein
plot(Timein1$date,Timein1$newInfected,type='l',col='red')
Timein

Time
View(Time)
plot(Time$date,Time$newinfected, type='l')

library(dplyr)
library(tidyr)
library(ggplot2)
Hosfin
Hosfin%>% 
  filter(Hosfin$location_name=='Lithuania')%>%
  ggplot(.,aes(x=Hosfin$date,y=Hosfin$confirmed_infections, color=Hosfin$location_name))+geom_line()

Hospitalization_all_locs$confirmed_infections%>%
mutate_all(~replace(., is.na(.), 0))
View(Hospitalization_all_locs)
dim(Hospitalization_all_locs)
Hospitalization_all_locs[is.na(Hospitalization_all_locs)] <- 0


ggplot(Hospitalization_all_locs,aes(x=date,y=confirmed_infections, color=location_name))+geom_line()
Summary_stats_all_locs%>%
  filter(stay_home_start_date!=''&educational_fac_start_date!=''&any_gathering_restrict_start_date!='')%>%
  group_by(location_name)%>%
  summarise(mean(all_bed_capacity))

#Structural Break Analysis 
installed.packages('strucchange')
library(strucchange)

library(xlsx)
library(forecast)
library(tseries)
library(strucchange)

## load the data from a CSV or Excel file. This example is done with an Excel sheet.
prod_df <- read.xlsx(file = 'agricultural_productivity.xls', sheetIndex = 'Sheet1', rowIndex = 8:65, colIndex = 2, header = FALSE)
colnames(prod_df) <- c('Rice')
colnames(Hospitalization_all_locs)<-c()
## store rice data as time series objects
rice <- ts(prod_df$Rice, start=c(312, 1), end=c(31362, 1), frequency=1) 

covid<-ts(Hospitalization_all_locs$confirmed_infections,start=c(13979,3),end=c(14110,3),frequency=1)
# store the breakpoints\
break
bp.rice <- breakpoints(rice ~ 1)
summary(bp.rice)
bp.covid<-breakpoints(covid~1)

## the BIC chooses 5 breakpoints; plot the graph with breakdates and their confidence intervals
plot(bp.rice)
plot(rice)
lines(bp.rice)
plot(bp.covid)
plot(covid)
lines(bp.covid)
bp.covid
## confidence intervals
ci.rice <- confint(bp.rice)
ci.rice
lines(ci.rice)

ci.covid<-confint(bp.covid)
ci.covid
lines(ci.covid)


###### Final Result

clusfin$label<-c(2, 0, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 0, 2, 2, 2, 2, 2, 2, 0, 1, 1,
  2, 2, 2, 2, 0, 2, 1, 1, 1, 2, 1, 2, 0, 2, 2, 1, 2, 1, 2, 0, 2, 2,
  1, 1, 1, 1, 2, 2, 0)

Hospitalization_all_locs$confirmed_infections%>%
  mutate_all(~replace(., is.na(.), 0))
View(Hospitalization_all_locs)
dim(Hospitalization_all_locs)
##start

View(clusfin)
clusfin$top1<-rep(0,51)
clusfin$top2<-rep(0,51)
clusfin$top3<-rep(0,51)
for (i in c(1:51)){
  clusfin$top1[i]<-order(clusfin[i,4:23],decreasing = T)[1]
  clusfin$top2[i]<-order(clusfin[i,4:23],decreasing = T)[2]
  clusfin$top3[i]<-order(clusfin[i,4:23],decreasing = T)[3]
}

sectormat<-cbind(c(1:20),colnames(clusfin)[4:23])
sectormat
clusfin$biz1<-rep(0,51)
clusfin$biz2<-rep(0,51)
clusfin$biz3<-rep(0,51)
for (i in c(1:51)){
  clusfin$biz3[i]<-sectormat[clusfin$top3[i],2]
}
sectormat[7,2]
sectormat[,2]<-as.character(sectormat[,2])
sectormat[7,2]

clusfin$top1<-as.numeric(clusfin$top1)
clusfin$top2<-as.numeric(clusfin$top2)
clusfin$top3<-as.numeric(clusfin$top3)
clusfin$biz1<
View(clusfin)
clusfin<-clus_norm
write.csv(clusfin,'C://Users//main//Desktop//2020-1//MLDL//FinalProject//clufin.csv', row.names = T)
View(clusfin)
Hospitalization_all_locs[is.na(Hospitalization_all_locs)] <- 0
hos<-Hospitalization_all_locs
hos$newdate<-as.Date(hos$date)
View(hos)

clusfin0<-clusfin[which(clusfin$label==0),]
clusfin1<-clusfin[which(clusfin$label==1),]
clusfin2<-clusfin[which(clusfin$label==2),]

sumpol_notus$noness<-sumpol_notus$'all_non-ess_business_start_date'
sumpol<-Summary_stats_all_locs[which(is.na(Summary_stats_all_locs$stay_home_start_date)==FALSE & is.na(Summary_stats_all_locs$educational_fac_start_date)==FALSE & is.na(Summary_stats_all_locs$any_gathering_restrict_start_date)==FALSE & is.na(Summary_stats_all_locs$any_business_start_date)==FALSE & is.na(Summary_stats_all_locs$noness)==FALSE),]
View(sumpol)
hosfin<-hos[which(hos$newdate>as.Date('2020-02-14') & hos$newdate<as.Date('2020-05-26')),]
View(hosfin)
View(sumpol)
sumpol_notus$newstayhome<-as.Date(sumpol_notus$stay_home_start_date)
sumpol_notus$newedufac<-as.Date(sumpol_notus$educational_fac_start_date)
sumpol_notus$newgathering<-as.Date(sumpol_notus$any_gathering_restrict_start_date)
sumpol_notus$newanybiz<-as.Date(sumpol_notus$any_business_start_date)
sumpol_notus$newnoness<-as.Date(sumpol_notus$noness)
stlist<-clusfin2$statereal
c<-sumpol$location_name
hosfinpolicy<-hosfin[which(hosfin$location_name %in% c),]
hosfinreal<-hosfinpolicy[which(hosfinpolicy$location_name %in% stlist),]
policyind<-rep(0,5)
sumpolreal<-sumpol[which(sumpol$location_name %in% stlist),]
for (a in sumpolreal$location_name){
  minrow<-min(which(hosfinreal$location_name==a))
  maxrow<-max(which(hosfinreal$location_name==a))
  h<-hosfinreal[minrow:maxrow,]
  covid<-ts(h$confirmed_infections,start=h$newdate[1],end=h$newdate[101],frequency = 1)
  bp.covid<-breakpoints(covid~1)
  d<-bp.covid$breakpoints
  if(is.na(d[1])==FALSE){
    dt<-as.Date('2020-02-14')
    st<-as.numeric(sumpolreal$newstayhome[which(sumpolreal$location_name==a)]-dt)+10
    ed<-as.numeric(sumpolreal$newedufac[which(sumpolreal$location_name==a)]-dt)+10
    ga<-as.numeric(sumpolreal$newgathering[which(sumpolreal$location_name==a)]-dt)+10
    bz<-as.numeric(sumpolreal$newanybiz[which(sumpolreal$location_name==a)]-dt)+10
    es<-as.numeric(sumpolreal$newnoness[which(sumpolreal$location_name==a)]-dt)+10

    datepol<-c(st,ed,ga,bz,es)

    for (i in c(1:5)){
      e<-c()
      for (j in c(1:length(d))){
        val<-abs(datepol[i]-d[j])
        e[j]<-val
        }
      valfin<-min(e)
      policyind[i]<-policyind[i]+valfin
      }
  }
}

datepol
d
policyind
finalresult<-policyind
finalresult

sumpolreal$location_name
clusfin2$statereal

if(is.na(d)==FALSE){
  dt<-as.Date('2020-02-14')
  st<-as.numeric(sumpol$newstayhome[which(sumpol$location_name==a)]-dt)+10
  ed<-as.numeric(sumpol$newedufac[which(sumpol$location_name==a)]-dt)+10
  ga<-as.numeric(sumpol$newgathering[which(sumpol$location_name==a)]-dt)+10
  bz<-as.numeric(sumpol$newanybiz[which(sumpol$location_name==a)]-dt)+10
  es<-as.numeric(sumpol$newnoness[which(sumpol$location_name==a)]-dt)+10
  
  datepol<-c(st,ed,ga,bz,es)
  
  for (i in c(1:5)){
    e<-c()
    for (j in c(1:length(d))){
      val<-abs(datepol[i]-d[j])
      e[j]<-val
    }
    valfin<-min(e)
    policyind[i]<-policyind[i]+valfin
  }
}
datepol
policyind

clus

library(dplyr)
View(clusfin)
k<-clusfin%>%
  group_by(label)%>%
  summarise_each(funs(mean),-statereal)

barplot(as.matrix(k[c(1,2,3),c(22:24)]),beside=T)


View(sumpol)
sumpol_us<-sumpol[which(sumpol$location_name %in% clusfin$statereal),]
View(sumpol_us)
sumpol_notus$newstayhomefin<-rep(0,22)
sumpol_notus$newstayhomefin<-as.Date(sumpol_notus$stay_home_end_date, origin = "1970-01-01")
sumpol_notus$newedufacfin<-rep(0,22)
sumpol_notus$newedufacfin<-as.Date(sumpol_notus$educational_fac_end_date, origin = "1970-01-01")
sumpol_notus$newgatheringfin<-rep(0,22)
sumpol_notus$newgatheringfin<-as.Date(sumpol_notus$any_gathering_restrict_end_date, origin = "1970-01-01")
sumpol_notus$newanybizfin<-rep(0,22)
sumpol_notus$newanybizfin<-as.Date(sumpol_notus$any_business_end_date, origin = "1970-01-01" )
sumpol_notus$newnonessfin<-rep(0,22)
sumpol_notus$newnonessfin<-as.Date(sumpol_notus$`all_non-ess_business_end_date`, origin = "1970-01-01" )
class(policy_date[1,30])
policy_date$X1<-as.Date(policy_date$X1, origin = "1970-01-01")
sumpol_notus$newnoness<-as.Date(sumpol_notus$newnoness, origin="1970-01-01")
for (i in c(1:22)){
  for (j in c(1:152)){
    if (is.na(sumpol_notus$newstayhome[i])==T){
      policy_date[j,5*i-4+146]<-0
    }
    else if (sumpol_notus$newstayhome[i]>policy_date$X1[j]){
      policy_date[j,5*i-4+146]<-0
    }
    else if (is.na(sumpol_notus$newstayhome[i])==T){
      policy_date[j,5*i-4+146]<-1
    }
    else if (sumpol_notus$newnonessfin[i]>=policy_date$X1[j]){
      policy_date[j,5*i-4+146]<-1
    }
    else {policy_date[j,5*i-4+146]<-0}
  }
}
warnings()

write.csv(policy_date,'C://Users//main//Desktop//2020-1//MLDL//FinalProject//policydatefin.csv', row.names = T)
write.csv(sumpol_us,'C://Users//main//Desktop//2020-1//MLDL//FinalProject//sumpol_us.csv', row.names = T)



sumpol_notus<-Summary_stats_all_locs[which(Summary_stats_all_locs$location_name %in% clusfin$statereal),]
View(sumpol_notus)
sumpol_notus<-sumpol_notus[which(!sumpol_notus$location_name %in% sumpol_us$location_name),]
View(sumpol_notus)
View(sumpol_us)
sumpol$newstayhomefin<-rep(0,108)
sumpol$newstayhomefin<-as.Date(sumpol$stay_home_end_date, origin = "1970-01-01")
sumpol$newedufacfin<-rep(0,108)
sumpol$newedufacfin<-as.Date(sumpol$educational_fac_end_date, origin = "1970-01-01")
sumpol$newgatheringfin<-rep(0,108)
sumpol$newgatheringfin<-as.Date(sumpol$any_gathering_restrict_end_date, origin = "1970-01-01")
sumpol$newanybizfin<-rep(0,108)
sumpol$newanybizfin<-as.Date(sumpol$any_business_end_date, origin = "1970-01-01" )
sumpol$newnonessfin<-rep(0,108)
sumpol$newnonessfin<-as.Date(sumpol$`all_non-ess_business_end_date`, origin = "1970-01-01" )

################################

a='Lithuania'
minrow<-min(which(Hosfin$location_name==a))
maxrow<-max(which(Hosfin$location_name==a))
h<-Hosfin[minrow:maxrow,]
covid<-ts(h$confirmed_infections,start=h$newdate[1],end=h$newdate[91],frequency = 1)
bp.covid<-breakpoints(covid~1)
plot(covid, main='Daliy confirmed infections in Lithuania')
lines(bp.covid)
d<-bp.covid$breakpoints
if(is.na(d)==FALSE){
  dt<-as.Date('2020-02-14')
  st<-as.numeric(sumpol$newstayhome[which(sumpol$location_name==a)]-dt)+10
  ed<-as.numeric(sumpol$newedufac[which(sumpol$location_name==a)]-dt)+10
  ga<-as.numeric(sumpol$newgathering[which(sumpol$location_name==a)]-dt)+10
  bz<-as.numeric(sumpol$newanybiz[which(sumpol$location_name==a)]-dt)+10
  es<-as.numeric(sumpol$newnoness[which(sumpol$location_name==a)]-dt)+10
  
  datepol<-c(st,ed,ga,bz,es)
  
  for (i in c(1:5)){
    e<-c()
    for (j in c(1:length(d))){
      val<-abs(datepol[i]-d[j])
      e[j]<-val
    }
    valfin<-min(e)
    policyind[i]<-policyind[i]+valfin
  }
}

Time
par(mfrow=c(1,1))
plot(Time$date,Time$newinfected,type='l',col='red')
library(quantmod)
usd_krw<-getSymbols(Symbols = 'USD/KRW', src='oanda',from=Sys.Date()-150,to=Sys.Date(),auto.assign = F)
lines(usd_krw)
plot(usd_krw)

plot(addLines(h=1200,on=1,col='red'))

kospi<-getSymbols(Symbols='^KS11',
                  src='yahoo',
                  from='2020-02-15',
                  to='2020-05-17',auto.assign = F)


plot(kospi$KS11.Close)
kospi
apple<-getSymbols(Symbols='AAPL',
                  src='yahoo',
                  from=Sys.Date()-100,
                  to=Sys.Date(),auto.assign = F)
plot(apple)

samsung<-getSymbols('005930.KS',
                    src='yahoo',
                    from=Sys.Date()-60,
                    to=Sys.Date(),auto.assign = F)
plot(samsung)

##### Clustering of USA states by pop and sectors

clusfin<-stateclusterrela
clusfin<-select(clus, -state, -Housingprice)
View(clusfin)
# feature scaling of each variable

## clustering 
# Normalization을 위한 함수 생성
normfunc <- function(x) {
  return((x-mean(x))/sd(x))
}

# lapply 함수를 이용해 모든 열에 Normalization 적용
clus_norm <- as.data.frame(lapply(clusfin[2:24], normfunc))
View(clus_norm)
clusfin.variables
install.packages('tidyverse')
library(cluster)
library(factoextra)
library(gridExtra)


clusfin[,-c(1)]<-scale(clusfin[,-c(1)])

View(clusfin)

kmeans2<-kmeans(clus_norm,centers=10,nstart=25)

fviz_cluster(kmeans2,data=clusfin)
View(clusfin)
label<-c(2, 2, 1, 2, 0, 1, 2, 2, 2, 0, 1, 2, 2, 1, 1, 2, 2, 2, 2, 2, 1, 1,
        1, 1, 2, 1, 2, 2, 2, 2, 1, 2, 0, 1, 2, 1, 2, 1, 1, 2, 2, 2, 1, 0,
        2, 2, 1, 1, 2, 1, 2)

clusfin$label<-label


########
clus.stand <- scale(clusfin[-1])
#wine.stand <- wine[-1]
head(wine.stand)

# k-means 기본값
k.means.fit <- kmeans(clus_norm, 3) # k=3 인 경우에 k-means

k.means.fit.5 <- kmeans(clus_norm, 5) # k=5 인 경우에 k-means
k.means.fit.3.25 <- kmeans(wine.stand, 3, nstart = 25) # 다른 초기값을 25개 셋 시도
#attributes(k.means.fit)
#k.means.fit$centers
#k.means.fit$cluster
#k.means.fit$size

k.means.fit

# Elbow method로 적절한 k 값 찾기 - wss, within sum of squares
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="# of Clusters",
       ylab="Within group sum of squares")}

wssplot(wine.stand)
wssplot(wine.stand, nc = 6)


# 2차원 평면에 k-means 결과값 도식화 - k=3이 k=5보다 더 낫다는 것을 눈으로 증명
clusplot(clus_norm, k.means.fit$cluster, main='2차원 평면에 도식화 (k=3)',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

clusplot(wine, k.means.fit.5$cluster, main='2차원 평면에 도식화 (k=5)',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


# confusion matrix
table(wine[,1],k.means.fit$cluster)

table(wine[,1],k.means.fit.5$cluster)

# Hierarchical clustering 으로 접근
d <- dist(wine.stand, method = "euclidean") # Euclidean 거리 계산법으로
H.fit <- hclust(d, method = "ward.D2") # Ward는 within-cluster variance를 최소화

plot(H.fit) # dendogram 도식화
groups <- cutree(H.fit, k = 3) # k=3 활용
rect.hclust(H.fit, k = 3, border = "red")

# Confusion matrix
table(wine[,1],groups) # Hierarchical의 퍼포먼스가 더 안 좋다 - 왜? 데이터 형태에 주의


clusfin%>% 
  group_by(label)%>%
  plot(mean(pop))