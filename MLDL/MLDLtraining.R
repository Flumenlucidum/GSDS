name_list<-sumpol_us$location_name
put<-rep(0,145)
i=0
View(datafinal)

for (name in name_list ){
  put[5*i+1]<-paste(as.character(name),'stayhome',sep=' ')
  put[5*i+2]<-paste(as.character(name),'edu',sep=' ')
  put[5*i+3]<-paste(as.character(name),'gathering',sep=' ')
  put[5*i+4]<-paste(as.character(name),'weakbiz',sep=' ')
  put[5*i+5]<-paste(as.character(name),'strictbiz',sep=' ')
  i<-i+1
}

put<-as.data.frame(put)
View(put)

rm(list=ls())
#data preparation
library(dplyr)
data_hos<-Hospitalization_all_locs
data_sum<-Summary_stats_all_locs
View(data_sum)
data_hos<-select(data_hos,V1,location_name, date, confirmed_infections)
View(data_hos)
cols<-c(1,19:28)
data_sum<-data_sum[,cols]
View(data_sum)
#date is from 2.25-6.3 this should be the batchsize
#changing the date as comparable one 
data_sum$stay_home_start_date<-as.Date(data_sum$stay_home_start_date,origin='1970-01-01')
data_sum$stay_home_end_date<-as.Date(data_sum$stay_home_end_date,origin='1970-01-01')
data_sum$educational_fac_start_date<-as.Date(data_sum$educational_fac_start_date,origin='1970-01-01')
data_sum$educational_fac_end_date<-as.Date(data_sum$educational_fac_end_date,origin='1970-01-01')
data_sum$any_gathering_restrict_start_date<-as.Date(data_sum$any_gathering_restrict_start_date,origin='1970-01-01')
data_sum$any_gathering_restrict_end_date<-as.Date(data_sum$any_gathering_restrict_end_date,origin='1970-01-01')
data_sum$any_business_start_date<-as.Date(data_sum$any_business_start_date,origin='1970-01-01')
data_sum$any_business_end_date<-as.Date(data_sum$any_business_end_date,origin='1970-01-01')
data_sum$`all_non-ess_business_start_date`<-as.Date(data_sum$`all_non-ess_business_start_date`,origin='1970-01-01')
data_sum$`all_non-ess_business_end_date`<-as.Date(data_sum$`all_non-ess_business_end_date`,origin='1970-01-01')

data_hos$date<-as.Date(data_hos$date,origin='1970-01-01')

#only choose the date between 2.25 -6.3
data_hos<-data_hos[which(data_hos$date>='2020-06-03'&data_hos$date<='2020-06-13'),]

## filling in the datafinal # normalize!! 
nation_list<-data_sum$location_name
data_hos<-data_hos[which(is.na(data_hos$confirmed_infections)==F),]
data_hos$confirmed_infections<-as.numeric(data_hos$confirmed_infections)
code<-0
data_hos$rela_confirmed<-rep(0,20886)
for (nation in nation_list){
  if (nation %in% data_hos$location_name){
    minrow<-min(which(data_hos$location_name==nation))
    maxrow<-max(which(data_hos$location_name==nation))
    max_value<-max(data_hos[minrow:maxrow,4])
    for (i in c(minrow:maxrow)){
      data_hos[i,5]<-data_hos[i,4]/max_value
    }
  }
  else{}
}
datafinal[1,]<-0
dim(datafinal)
statelist<-stateclusterfinal$state_or_gu
statelist
data_sum_usa<-data_sum[which(data_sum$location_name %in% statelist),]
View(data_sum_usa)
for (nation in statelist){
  
  if (nation %in% unique(data_hos$location_name)){
    minrow<-min(which(data_hos$location_name==nation))
    maxrow<-max(which(data_hos$location_name==nation)) 
    if (is.na(data_sum[which(data_sum$location_name==nation),]$stay_home_start_date)==T && is.na(data_sum[which(data_sum$location_name==nation),]$educational_fac_start_date)==T && is.na(data_sum[which(data_sum$location_name==nation),]$any_gathering_restrict_start_date==T && is.na(data_sum[which(data_sum$location_name==nation),]$any_business_start_date)==T && is.na(data_sum[which(data_sum$location_name==nation),]$`all_non-ess_business_start_date`)==T)){
      a<-1 
      for (i in c(minrow:maxrow)){
        row<-c(0,0,0,0,0,a,a,data_hos[i,4],nation)
        
        datafinal<-rbind(datafinal,row)
        a<-a+1
      }
    }
    else{
      cumuldate<-1
      for (date in as.matrix(data_hos[minrow:maxrow,3])){
        
        row<-rep(0,9)
        for (i in c(1:5)){
          if (is.na(data_sum[which(data_sum$location_name==nation),2*i])==T & is.na(data_sum[which(data_sum$location_name==nation),2*i+1])==T){
            row[i]<-0
          }
          else if (date<data_sum[which(data_sum$location_name==nation),2*i]){
            row[i]<-0
          }
          else if (is.na(data_sum[which(data_sum$location_name==nation),2*i+1]==T)){
            row[i]<-as.numeric(as.Date(date)-as.Date(as.matrix(data_sum[which(data_sum$location_name==nation),2*i]))+1)
          }
          else if (date>=data_sum[which(data_sum$location_name==nation),2*i]&date<=data_sum[which(data_sum$location_name==nation),2*i+1]){
            row[i]<-as.numeric(as.Date(date)-as.Date(as.matrix(data_sum[which(data_sum$location_name==nation),2*i]))+1)
            
          }
          else if (date>data_sum[which(data_sum$location_name==nation),2*i+1]){
            row[i]<-0
          }
        }
      
        if (identical(row[1:5],rep(0,5)) & datafinal[nrow(datafinal),6]!=0){

            row[6]<-nopol+1
            nopol<-nopol+1
        }
        else if (identical(row[1:5],rep(0,5))){
          row[6]<-1
          nopol<-1
        }
        else{
          row[6]<-0
          nopol<-0
        }
        
        row[7]<-cumuldate
        
        row[8]<-data_hos[minrow:maxrow,][cumuldate,4]
        row[9]<-nation
        row[10]<-date
        datafinal<-rbind(datafinal,row)
        cumuldate<-cumuldate+1
    }
    
 
    }
   
   }
  
}
datafinal$confirmed<-as.numeric(datafinal$confirmed)
datafinal$confirmed<-round(datafinal$confirmed)

write.csv(statecluster_arima,file='C://Users//main//Desktop//2020-1//MLDL//FinalProject//stateclusterarima_rolling.csv')
write.csv(datafinal,file='C://Users//main//Desktop//2020-1//MLDL//FinalProject//trainingdata_usa_since0603.csv')

y<-as.numeric(datafinal$confirmed)
x1<-as.numeric(datafinal$stayhome_cumul)
x2<-as.numeric(datafinal$edu_cumul)
x3<-as.numeric(datafinal$gathering_cumul)
x4<-as.numeric(datafinal$weakbiz_cumul)
x5<-as.numeric(datafinal$strictbiz_cumul)
model_all<-lm(y~x1)
summary(model_all)
View(datafinal)
lines(as.numeric(unlist(datafinal[which(datafinal$nationcode=='Alabama'),8]))~as.numeric(unlist(datafinal[which(datafinal$nationcode=='Alabama'),1])),type='l')

plot(as.numeric(unlist(datafinal[which(datafinal$nationcode=='Connecticut'),8]))~as.numeric(unlist(datafinal[which(datafinal$nationcode=='Connecticut'),1])),type='l',col='red')

# 분기별 데이터 + Additive

trend.Cali <- ma(as.numeric(unlist(datafinal[which(datafinal$nationcode=='Connecticut'),8])), order = 4, centre = T)
plot(as.ts(as.numeric(unlist(datafinal[which(datafinal$nationcode=='Connecticut'),8]))))
lines(trend.Cali,col='red')


# 월별 데이터 + Multiplicative 
library(forecast)
trend.cali <- ma(as.numeric(unlist(datafinal[which(datafinal$nationcode=='Connecticut'),8])), order = 13, centre = T)
plot(as.ts(as.numeric(unlist(datafinal[which(datafinal$nationcode=='Connecticut'),8]))))
lines(trend.cali, col='red')



#### 2. 트렌드 제거하고 데이터 보기

detrend.cali<-as.numeric(unlist(datafinal[which(datafinal$nationcode=='Connecticut'),8]))-trend.cali
plot(as.ts(detrend.cali))

trend.tx <- ma(as.numeric(unlist(datafinal[which(datafinal$nationcode=='Texas'),8])), order = 12, centre = T)
plot(as.ts(as.numeric(unlist(datafinal[which(datafinal$nationcode=='Texas'),8]))))
lines(trend.tx,col='red')
detrend.tx<-as.numeric(unlist(datafinal[which(datafinal$nationcode=='Texas'),8]))-trend.tx
plot(as.ts(detrend.tx))
detrend.tx
#### 3. 트렌드 제거된 데이터에서 계절성 평균치 내기
#- 계절별 증감분을 제거하고나면 순수하게 설명 못한 부분만 남음

matrix.tx <- t(matrix(data = detrend.tx, nrow = 4)) # 행렬 계산을 이용하면 쉽게 계절별 평균을 낼 수 있음
matrix.tx
seasonal.tx <- colMeans(matrix.tx, na.rm = T) # 분기별 자료니까 4개 행을 사용
plot (as.ts(rep(seasonal.tx, 16))) # 꼭 16을 쓸 필요는 없으나, 계절성이 16번 반복된다고 가정


#### 4. 오차항 잡아내기
#- 설명 못한 부분(오차)이 머신러닝의 적용 대상이 되면 정확도를 높일 수 있음

random.tx <- dat - trend.tx - seasonal.tx
lines(as.ts(random.tx))

dat<-as.numeric(unlist(datafinal[3:145,11]))
dat[1]
dat<-dat[15:length(dat)]
plot(as.ts(dat), main='Connecticut')
dev.off()
fit_arma<-arima(dat, order=c(2,2,2),include.mean = F)
res<-fit_arma$residuals
lines(fitted(fit_arma),col=4)
datafinal[34,12]<-dat[1]-fitted(fit_arma)[1]
plot(as.ts(dat-fitted(fit_arma)))
checkresiduals(fit_arma)
autoplot(forecast(fit_arma))
par(mfrow=c(1,2))
datafinal$logconfirmed<-rep(0,4593)
for (i in c(2:4593)){
  datafinal$logconfirmed[i]<-log(datafinal$confirmed[i]+0.0001)-log(datafinal$confirmed[i-1]+0.0001)
}
fit_arma<-arima(res, order=c(2,2,2),include.mean = F)
lines(fitted(fit_arma),col=4)
datafinal$overtrend<-rep(0,4593)
statelist
for (nation in statelist){
  
  if (nation %in% unique(datafinal$nationcode)){
    minrow<-min(which(datafinal$nationcode==nation))
    maxrow<-max(which(datafinal$nationcode==nation))
    datafinal[minrow,11]<-0
    dat<-as.numeric(unlist(datafinal[minrow+1:maxrow,11]))
    fit_arma<-arima(dat,order=c(2,2,2),include.mean = F)
    for (i in c(2:(maxrow-minrow+1))){
      datafinal[(i+minrow-1),12]<-fit_arma$residuals[i]
    }
  }
  else{}
}

plot(as.ts(datafinal[which(datafinal$nationcode=='Alabama'),11]))
lines(as.ts(datafinal[which(datafinal$nationcode=='Florida'),11]),col='blue')
for (nation in statelist){
  
  if (nation %in% unique(datafinal$nationcode)){
    minrow<-min(which(datafinal$nationcode==nation))
    maxrow<-max(which(datafinal$nationcode==nation))
    datafinal[minrow,11]<-0
    dat<-as.numeric(unlist(datafinal[minrow+1:maxrow,11]))
    for (i in c(2:(maxrow-minrow+1))){
      datafinal[(i+minrow-1),12]<-fit_arma$residuals[i]
    }
  }
  else{}
}

datafinal$spikedate<-rep(0,4593)
for (nation in statelist){
  
  if (nation %in% unique(datafinal$nationcode)){
    minrow<-min(which(datafinal$nationcode==nation))
    maxrow<-max(which(datafinal$nationcode==nation))
    for (i in c((minrow+20):maxrow)){
      if (datafinal[i,11]>1){
        datafinal[i,13]<-1
      }
    }
  }
  else{}
}
library(forecast)
datafinalprev<-datafinal
plist<-rep(0,51)
i<-1
View(datafinal)
datafinal<-rolling
minrow<-min(which(datafinal$nationcode=='North Carolina'))
maxrow<-max(which(datafinal$nationcode=='North Carolina'))
dat<-as.numeric(unlist(datafinal[minrow:maxrow,14]))
library(forecast)
dat
plot(as.ts(dat),main='N.Carolina (moving average)')  
arma<-auto.arima(dat)
lines(fitted(arma),col='blue')
error<-arma$residuals
plot(as.ts(error),main='Error(N.Carolina)')
  a<-shapiro.test(error)
  plist[i]<-a$p.value
  i=i+1
}
plist
as.Date('2020-02-26')-as.Date('2020-06-03')

View(data_hos)
data_hos_usa<-data_hos[which(data_hos$location_name=='United States of America'),]
View(data_hos_usa)
covid_smooth<-matrix(1,2,0)
data_hos_usa$log_confirmed<-rep(0,100)

for (i in c(2:100)){
  data_hos_usa[i,6]<-log(data_hos_usa[i,4])-log(data_hos_usa[(i-1),4])
}
logusa<-data_hos_usa$log_confirmed[20:length(logusa)]
plot(as.ts(logusa))
arma<-auto.arima(logusa)
error<-arma$residuals
plot(error)
qqnorm(error)
qqline(error)
shapiro.test(error)
hist(error)

plot(a,error)
a<-as.numeric(fitted(arma))

covid_smooth<-matrix(rep(0,33*51*3),33*51,3)
covid_smooth
statecluster_arima$`p-value`<-plist

n=10
t<-seq(0,4,0.01)
g<-(t^n)*exp(-n*t)
plot(t,g, type='l')
g
t
