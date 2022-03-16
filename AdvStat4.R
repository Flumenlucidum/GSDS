library(ggplot2)
data(psid,package = 'faraway')
head(psid)
#longitudinal data

psid$year_centered=psid$year-78
psid$Male=0
psid$Male[psid$sex=='M']=1
plot(psid$year_centered,psid$income)

ggplot(psid,aes(x=year,y=income+100,group=person))+geom_line()+facet_wrap(~sex)+scale_y_log10()

#fit linear model

library(lme4)
ml<-lmList(log(income)~year_centered|person,data=psid)
intercepts<-sapply(ml,coef)[1,]
slopes<-sapply(ml,coef)[2,]

psex<-psid$sex[match(1:85,psid$person)]
View(psex)
peduc<-psid$educ[match(1:85,psid$person)]
plot(intercepts,slopes)
par(mfrow=c(2,2))
boxplot(intercepts~psex)
boxplot(intercepts~peduc)
boxplot(slopes~psex)
boxplot(slopes~peduc)

par(mfrow=c(1,1))

#Linear mixed model
library(pbkrtest)
#In case of longitudinal data, setting fixed effect in the model is inefficient
#set each person's intercept and slope as random. (year_centered and person are random)
#if you want only the intercept then put (1|person)
mmod<-lmer(log(income)~year_centered*Male+age+educ+(year_centered|person),data=psid)
summary(mmod)
# cov(r_i0,r_i1)=corr * std(r_i0) * std(r_i1)

#Test fixed effect 
#H_0 :beta_ys=0 H_1: beta_ys not zero

library(pbkrtest)
#full model
mmod_ML<-lmer(log(income)~year_centered*Male + age +educ+(year_centered|person),data=psid,REML=F)
#reduced model
mmod_ML2<-lmer(log(income)~year_centered+Male+age+educ+(year_centered|person),data=psid,REML = F)

T_LRT<-2*(logLik(mmod_ML)-logLik(mmod_ML2))

P_LRT<-pchisq(T_LRT,df=1,lower.tail = F)
P_LRT
#male *year is the only difference df =1 

#Kenward-Roger test (F- test)
#numerator df is one for sure 
#denominator df is not certain
#more observation than its unit of number

KRmodcomp(mmod_ML,mmod_ML2)

#confidence interval
confint(mmod,method='Wald')
#beta +- 1.95 S.E

confint(mmod,method='boot')
#variance component confint possible

#individual level slope 
mmod<-lmer(log(income)~year_centered*Male+age+educ+(year_centered|person),data=psid)
par (mfrow=c(2,3))
for (i in 1:6){
  psid_1<-psid[psid$person==i,]
  X1<-cbind(1,psid_1$year_centered,psid_1$Male,psid_1$age,psid_1$educ,psid_1$year_centered*psid_1$Male)
  Y_hat<-X1%*% as.numeric(coef(mmod)$person[i,])
  
  plot(log(psid_1$income)~psid_1$year_centered,main=paste('Person',i),xlab='year(centered)',ylab='log income')
  points(Y_hat~psid_1$year_centered,type='l',col='red')
}


#Generalized Linear Model 

data(wcgs,package='faraway')
head(wcgs)
View(wcgs)

table(wcgs$chd)
par(mfrow=c(1,3))
plot(cigs~chd,data=wcgs)
plot(age~chd,data=wcgs)
plot(weight~chd,data=wcgs)
par(mfrow=c(1,1))

#Make numerical y

wcgs$y<-ifelse(wcgs$chd =='no',0,1)

#Logistic regression with g(phi)=log-odds
#Y~bernoulli(phi)

#IRWLS fit the regression model

tol=10^-5
Y<-wcgs$y
X<-model.matrix(Y~cigs+age+weight,data=wcgs)
X[1:2,]

p<-ncol(X)
beta<-rep(0,p)
for (i in 1:10){
  eta=(X%*%beta)[,1]
  mu=exp(eta)/(1+exp(eta))
  v=mu*(1-mu)
  Z<-eta+(Y-mu)/v
  VX=X*v
  beta_new<-solve(t(X)%*%VX,t(VX)%*%Z)
  
  diff<-sum((beta-beta_new)^2)
  cat("Iter[",i,"]\n")
  cat("beta_new:",beta_new,"\n")
  cat("diff:", diff, "\n")
  beta=beta_new
  if(diff<tol){
    break()
  }
}


#confidence interval

I=t(X)%*%VX
Var_ML<-solve(I)
sqrt(diag(Var_ML))

#95%CI
beta[2]-1.96*sqrt(Var_ML[2,2])

beta[2]+1.96*sqrt(Var_ML[2,2])


#Wald test
#weight signigicantly affects the CHD risk?

#H0 : beta3=0   
#Wald statistics (beta_hat)^2/var(beta_hat)

b4_wald=beta[4]^2/Var_ML[4,4]
b4_pvalue=pchisq(b4_wald,df=1,lower.tail=F)
b4_wald
b4_pvalue

#LRT for both age, weight
#mean value under H0

#if y follows bernoulli
#loglikelihood_zero = sigma[ylogpi + (1-y)log(1-pi)]

IRWLS_logit<-function(X,Y,tol=10^-5,Maxiter=100){
  p<-ncol(X)
  beta<-rep(0,p)
  for (i in 1:Maxiter){
    eta=(X%*%beta)[,1]
    mu=exp(eta)/(1+exp(eta))
    v<-mu*(1-mu)
    Z<-eta+(Y-mu)/v
    VX=X*v
    beta_new=solve(t(X)%*%VX,t(VX)%*%Z)
    
    diff<-sum((beta-beta_new)^2)
    beta=beta_new
  }  
    if (diff>tol){
      warning("IRWLS did not converge.")
    }

    
    return(list(beta=beta,mu=mu))
}

X0<-model.matrix(Y~cigs,data=wcgs)

H1_result<-IRWLS_logit(X,Y)
H0_result<-IRWLS_logit(X0,Y)

l1=sum(Y*log(H1_result$mu)+(1-Y)*log(1- H1_result$mu))
l0=sum(Y*log(H0_result$mu)+(1-Y)*log(1- H0_result$mu))

T_LRT=2*(l1-l0)
T_LRT
pval=pchisq(T_LRT,df=2,lower.tail = F)
pval

#glm function

out.glm<-glm(Y~cigs+age+weight,data=wcgs,family=binomial(link='logit'))
summary(out.glm)

#LRT
out.glm.H0<-glm(Y~cigs,data=wcgs,family = binomial(link = 'logit'))
anova(out.glm,out.glm.H0,test='LRT')
#full reduced model

#Diagnostics

#Deviance of full and intercept only model 

Deviance_Logistic<-function(Y,mu){
  idx1<-which(Y==1)
  idx0<-which(Y==0)
  
  Devi<-rep(0,length(Y))
  Devi[idx0]<-2*(-log(1-mu[idx0]))
  Devi[idx1]<-2*(-log(mu[idx1]))
  
  D_all<-sum(Devi)
  return(list(D=D_all,Devi=Devi))
}

out.glm0<-glm(y~1,data=wcgs,family=binomial(link='logit'))
out.glm1<-glm(y~cigs+age+weight,data=wcgs,family=binomial(link='logit'))

mu0<-out.glm0$fitted.values
mu1<-out.glm1$fitted.values

D1<-Deviance_Logistic(Y,mu1)
D0<-Deviance_Logistic(Y,mu0)

D1$D
D0$D

deviance(out.glm1)

out.glm1$null.deviance


#Residual plot with deviance residual

rd<-sqrt(abs(D1$Devi))*sign(Y-mu1)
par(mfrow=c(1,2))
plot(out.glm1$linear.predictors,rd)
#linear predictor =XTB
plot(mu1,rd)
# mu1  exp(eta)/(1+exp(eta))

par(mfrow=c(1,1))
#Get leverages
X<-model.matrix(out.glm1)
V<-diag(mu1*(1-mu1))
XVX<-t(X)%*%V%*%X
XV_sqrt<-t(X)%*%sqrt(V)
H=t(XV_sqrt)%*%solve(XVX)%*%XV_sqrt
h=diag(H)

plot(h)


####boot package

library(boot)
diag1<-glm.diag(out.glm1)
names(diag1)
plot(diag1$h)

##Test
#H0 beta_2, beta_3 =0 

#T=D0-D1 
#T1~chisq df 2 

out.glm_H0<-glm(y~cigs,data=wcgs,family = binomial(link = 'logit'))
T_test=deviance(out.glm_H0)-deviance(out.glm1)
pchisq(T_test,df=2,lower.tail = F)

#use anova
anova(out.glm1,out.glm_H0,test='LRT')


#Note 11
Pneu<-read.table('pneumonia_MPV.txt',h=F)
colnames(Pneu)<-c('idnum','exposure','cases','miners')
Pneu$no_case=Pneu$miners-Pneu$cases
Pneu

#logit(pi_i)=b_0+b_1*exposure
#y~bin(n_i,pi_i)
out.glm.interceptOnly<-glm(cbind(cases,no_case)~1,family = binomial,data=Pneu)

out.glm.linear<-glm(cbind(cases,no_case)~exposure,family = binomial,data=Pneu)
summary(out.glm.linear)
View(Pneu)

l1<-logLik(out.glm.linear)[1]
l0<-logLik(out.glm.interceptOnly)[1]
N<-sum(Pneu$miners)
L1<-exp(l1)
L0<-exp(l0)
R2_CS<-1-(L0/L1)^{2/N}
MaxR2<-1-(L0)^{2/N}
R2_MaxAdj=R2_CS/MaxR2
R2_CS
R2_MaxAdj

#saturated logistic model
#logit(pi_i)=b_0+b_1*I(x=15)+ .... + beta_7*I(X=51.5)
#referential coding

Pneu$pi_sat= Pneu$cases/Pneu$miners
Pneu$logit_sat=log(Pneu$pi_sat/(1-Pneu$pi_sat))
Pneu$odds_sat=exp(Pneu$logit_sat)
View(Pneu)
#cell mean coding
out.glm.saturated<-glm(cbind(cases,no_case)~factor(exposure)-1,family = binomial,data=Pneu)
summary(out.glm.saturated)
#referential coding
out.glm.saturated_ref<-glm(cbind(cases,no_case)~factor(exposure),family = binomial,data=Pneu)
summary(out.glm.saturated_ref)

#plot

#saturated model predictor 8
#linear model predictor 2 
#comparison 
par(mfrow=c(1,2))

#deviance possible
#log(pi/(1-pi))
plot(out.glm.saturated$data$exposure,out.glm.saturated$linear.predictors,col='red',type='l' ,main='logit',ylab='logit')
points(out.glm.linear$data$exposure,out.glm.linear$linear.predictors,col='blue',type='l' )

#pi
plot(out.glm.saturated$data$exposure,out.glm.saturated$fitted.values,col='red',type='l',main='prob',ylab='prob' )
points(out.glm.linear$data$exposure,out.glm.linear$fitted.values,col='blue',type='l' )
#linear model is 
#expand to individual level 
EXPAND<-NULL
for(i in 1:nrow(Pneu)){
  EXPAND<-rbind(EXPAND,cbind(rep(1,Pneu$cases[i]),Pneu$exposure[i]))
  EXPAND<-rbind(EXPAND,cbind(rep(0,Pneu$no_case[i]),Pneu$exposure[i]))
}
Expand.df<-data.frame(EXPAND)
colnames(Expand.df)<-c('Y','exposure')

head(Expand.df)
nrow(Expand.df)

out.glm.linear.ind<-glm(Y~exposure,data=Expand.df,family = binomial)
summary(out.glm.linear.ind)

out.glm.linear$deviance
out.glm.linear.ind$deviance

#GOF test
pchisq(out.glm.linear$deviance,df=6,lower.tail = F)

pchisq(out.glm.linear.ind$deviance,df=369,lower.tail = F)


#HW4

#2(b) 
library(openxlsx)
beta=c(-1,-1,-1)
tab<-read.xlsx('hw4.xlsx')
head(tab)
y<-tab$Y
eps<-1e-5
X<-model.matrix(Y~X1+X2,data=tab)
head(X)
for (i in 1:10){
  eta<-(X%*%beta)[,1]
  mu<--(1/eta)
  v<-mu^2
  Z<-eta+(y-mu)/v
  VX=X*v
  beta_new<-solve(t(X)%*%VX,t(VX)%*%Z)
  
  diff<-sum((beta-beta_new)^2)
  cat("Iter[",i,"]\n")
  cat("beta_new:", beta_new,"\n")
  cat("diff: ",diff,"\n")
  beta=beta_new
  
  if(diff<eps){
    break()
  }
}
beta_new
X%*%beta_new

#(c)
I<-t(X)%*%VX
Var_ML<-solve(I)
diag(Var_ML)
sqrt(diag(Var_ML))
beta[3]-1.96 *sqrt(Var_ML[3,3]) 
beta[3]+1.96 *sqrt(Var_ML[3,3]) 

#(d)
IRWLS_logit<-function(X,Y,tol=10^-5,Maxiter=100){
  p<-ncol(X)
  beta<-rep(-1,p)
  for (i in 1:Maxiter){
    eta<-(X %*% beta)[,1]
    mu<--(1/eta)
    v<-mu^2
    Z<-eta+(Y-mu)/v
    VX=X*v
    beta_new<-solve(t(X)%*%VX,t(VX)%*%Z)
    diff<-sum((beta-beta_new)^2)
    print(diff)
    beta=beta_new
    
    if(diff<tol){
      break()
    }
    
  }
  
  if(diff>tol){
    warning("IRWLS did not converge.")
  }
  
  return(list(beta=beta,mu=mu))
}

X0<-model.matrix(Y~1,data=tab)
head(y)
head(X)

H1_result<-IRWLS_logit(X,y)
H0_result<-IRWLS_logit(X0,y)
H1_result$mu

l1=sum(3*((-y/H1_result$mu)-log(H1_result$mu)))
l0=sum(3*((-y/H0_result$mu)-log(H0_result$mu)))
T=2*(l1-l0)
T
pval=pchisq(T,df=2,lower.tail = F)
pval

#test
library(glmnet)
out.glm<-glm(Y~X1+X2,data=tab,family =Gamma(link='inverse'))
out.glm
out.glm.H0<-glm(Y~ 1, data=tab, family=Gamma)
anova(out.glm, out.glm.H0, test="LRT")


#3

T1=0
T2=1
T3=2
T4=3
iter<-100
y<-rep(0,200)


  for (j in 1:25){
    r0<-rnorm(1,0,0.5)
    r1<-rnorm(1,0,0.5)
    for (i in 1:4){
      y[4*(j-1)+i]<-0.5+0.5*(i-1)+0.5*0+r0+r1*(i-1)+rnorm(1,0,1)
    }
  }
  for (j in 26:50){
    r0<-rnorm(1,0,0.5)
    r1<-rnorm(1,0,0.5)
    for (i in 1:4){
      y[4*(j-1)+i]<-0.5+0.5*(i-1)+0.5*1+r0+r1*(i-1)+rnorm(1,0,1)
    }
  }

X<-rep(0,200)
X[1:100]=0
X[101:200]=1
Ti<-rep(0,200)
for (i in 1:50){
  Ti[4*i-3]=0
  Ti[4*i-2]=1
  Ti[4*i-1]=2
  Ti[4*i]=3
}

data<-cbind(Ti,X,y)
View(data)

library(pbkrtest)

data(psid, package="faraway")
View(psid)
data<-data.frame(data)
person<-rep(0,200)
for (i in 1:50){
  person[4*i-3]=i
  person[4*i-2]=i
  person[4*i-1]=i
  person[4*i]=i
}
data<-cbind(data,person)
LRT_rejec<-rep(0,100)
KR_rejec<-rep(0,100)


for(iter in 1:100){

data$y<-rep(0,200)


for (j in 1:25){
  r0<-rnorm(1,0,0.5)
  r1<-rnorm(1,0,0.5)
  for (i in 1:4){
    data$y[4*(j-1)+i]<-0.5+0.5*(i-1)+0.5*0+r0+r1*(i-1)+rnorm(1,0,1)
  }
}
for (j in 26:50){
  r0<-rnorm(1,0,0.5)
  r1<-rnorm(1,0,0.5)
  for (i in 1:4){
    data$y[4*(j-1)+i]<-0.5+0.5*(i-1)+0.5*1+r0+r1*(i-1)+rnorm(1,0,1)
  }
}
mmod_ML <- lmer(y ~ Ti + X+(Ti|person)+(1|person),  data=data, REML=FALSE)
mmod_ML2 <- lmer(y ~Ti+(Ti|person)+(1|person),  data=data, REML=FALSE)

T_LRT<-2*(logLik(mmod_ML) - logLik(mmod_ML2))
P_LRT<-pchisq(T_LRT, df=1, lower.tail=FALSE)
if(P_LRT<0.05){
  LRT_rejec[iter]<-1
}
KR<-KRmodcomp(mmod_ML,mmod_ML2)
if(KR$stats$p.value<0.05){
  KR_rejec[iter]<-1
}

}

mmod_ML <- lmer(y ~ Ti + X+(1|person)+(Ti|person),  data=data, REML=FALSE)
mmod_ML
KR<-KRmodcomp(mmod_ML,mmod_ML2)
KR$stats$p.value
sum(LRT_rejec)
sum(KR_rejec)
