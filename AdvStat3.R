library(faraway)
data(state)
statedata<-data.frame(state.x77,row.names = state.abb)
nrow(statedata)
head(statedata)

lmod<-lm(Life.Exp~.,statedata)
summary(lmod)

#Backward elimination 
lmod1<-lmod
coef.deleted<-list()
p<-7
alpha<-0.05

for (i in 1:p){
  
  coef<-summary(lmod1)$coefficients[-1,]
  if(length(which(coef[,4]>=alpha))==0){
    break
  }
  idx<-which(coef[,4]==max(coef[,4]))
  coef.deleted[[i]]<-list(names=names(idx),coef[idx,])
  update_formula1<-paste(" ~ . - ",names(idx))
  lmod1<-update(lmod1,update_formula1)
  
}

coef.deleted

summary(lmod1)

#Criterion based
library(leaps)
install.packages('bestglm')
library(bestglm)
out<-bestglm(mar_test,IC='BICq',t=1,family=binomial)
data("SAheart")
View(SAheart)
#stepwise selection with AIC

step(lmod,direction='both')

#simulation

#data generation

set.seed(100)
n<-200
p<-100
beta<-0.15
X<-matrix(rnorm(n*p),ncol=p)
Y<-rowSums(X[,1:10])*beta +rnorm(n)

data1<-data.frame(Y=Y,X=X)
#true model
summary(lm(Y~X.1+X.2+X.3+X.4+X.5+X.6+X.7+X.8+X.9+X.10,data=data1))

#stepwise with AIC
lmod1<-lm(Y~.,data=data1)
lmod_AIC<-step(lmod1,trace = 0,direction = 'both')
summary(lmod_AIC)
nrow(summary(lmod_AIC)$coefficients)

#stepwise with BIC

lmod_BIC<-step(lmod1,trace=0,direction='both',k=log(n))
summary(lmod_BIC)
nrow(summary(lmod_BIC)$coefficients)


#prediction

set.seed(200)
n<-200
p<-100
beta<-0.15
X<-matrix(rnorm(n*p),ncol=p)
Y<-rowSums(X[,1:10])*beta + rnorm(n)
data_test<-data.frame(Y=Y,X=X)
Y_AIC<-predict(lmod_AIC,data_test)
Y_BIC<-predict(lmod_BIC,data_test)
cor(Y,Y_AIC)
cor(Y,Y_BIC)

###shrinkage

#data generation 

n<-300
p<-150
beta<-0.15
X<-matrix(rnorm(n*p),ncol=p)
Y<-rowSums(X)*beta +rnorm(n)
data1<-data.frame(Y=Y,X=X)
idx_train<-1:200
idx_test<-201:300
train<-data1[idx_train,]
test<-data1[idx_test,]

#Linear regression

modlm<-lm(Y~.,train)
summary(modlm)$r.squared
#root mean square error
rmse<-function(x,y){sqrt(mean((x-y)^2))}
rmse(fitted(modlm),train$Y)
rmse(predict(modlm,test),test$Y)


#Ridge regression 
install.packages('glmnet')
library(glmnet)
cv_output<-cv.glmnet(X[idx_train,],Y[idx_train],alpha=0,nfolds=10)
#alpha 0 ridge 1 lasso  in between elastic net
#10 fold cross validation to find lambda
par(mfrow=c(1,1))
plot(cv_output)

best_lam<-cv_output$lambda.min
best_lam
out.ridge<-glmnet(X[idx_train,],Y[idx_train],alpha=0,lambda = best_lam)
predict.Y<-predict(out.ridge,X)
rmse(predict.Y[idx_train],train$Y)

rmse(predict.Y[idx_test],test$Y)

#shrinkage
coef.lm<-coef(modlm)
coef.ridge<-coef(out.ridge)
par(mfrow=c(1,2))
boxplot(coef.lm[-1],main='LM')
boxplot(coef.ridge[-1],main='RIDGE')
#true beta =0.15, ridge variability is low

#Data generation from a sparse model

set.seed(200)
n<-300 #200 train 100 test
p<-100
beta=0.3

X<-matrix(rnorm(n*p),ncol=p)
Y<-rowSums(X[,1:10])*beta +rnorm(n)

#LASSO

out1<-glmnet(X[idx_train,],Y[idx_train],alpha=1)
par(mfrow=c(1,1))
plot(out1)
#similar to forward selection 

cv_output<-cv.glmnet(X[idx_train,],Y[idx_train],alpha=1,nfolds=10)
plot(cv_output)
best_lam<-cv_output$lambda.min
best_lam

out.lasso<-glmnet(X[idx_train,],Y[idx_train],alpha=1,lambda=best_lam)
predict.Y<-predict(out.lasso,X)
rmse(predict.Y[idx_train],train$Y)
rmse(predict.Y[idx_test],test$Y)

#non zero beta 
coef.lasso<-coef(out.lasso)[-1]
which(abs(coef.lasso)>0)

#shrinkage
boxplot(coef.lasso[1:10])
#Lasso variable selection + coefficient

#SCAD adaptive Lasso

#Elastic net

#glmnet does not give alpha changing
Elastic_Net<-function(X_var,Y_var,alpha_seq){
  out_obj<-NULL
  for (i in 1:length(alpha_seq)){
    cv_output<-cv.glmnet(X_var,Y_var,alpha=alpha_seq[i],nfolds=10)
    idx<-which(cv_output$lambda==cv_output$lambda.min)
    measure=cv_output$cvm[idx]
    out_obj<-rbind(out_obj,c(alpha_seq[i],cv_output$lambda.min,measure))
  }

  idx<-which(out_obj[,3]==min(out_obj[,3]))
  re<-list(best=out_obj[idx,],out_obj=out_obj)
  return(re)
}

out<-Elastic_Net(X[idx_train,],Y[idx_train],alpha_seq=c(0,0.25,0.5,0.75,1))
out

out.elastic<-glmnet(X[idx_train,],Y[idx_train],alpha=out$best[1],lambda=out$best[2])
predict.Y<-predict(out.elastic,X)
rmse(predict.Y[idx_train],train$Y)
rmse(predict.Y[idx_test],test$Y)
#comparison with lasso

coef.elastic<-coef(out.elastic)[-1]
which(abs(coef.elastic)>0)



######HW3
#1

library(faraway)
data('fat')
View(fat)
fat<-fat[-c(1,3)]

idx_test<-rep(0,25)
for (i in 1:25){
    idx_test[i]<-i*10
}
idx_test
train<-fat[-idx_test,]
test<-fat[idx_test,]
#linear regression with all predictors
mdl_lr<-lm(siri~.,data=train)
summary(mdl_lr)
#root mean square error
rmse<-function(x,y){sqrt(mean((x-y)^2))}
rmse(fitted(mdl_lr),train$siri)
rmse(predict(mdl_lr,test),test$siri)

#linear regression with AIC

library(leaps)
b<-regsubsets(siri ~., data=train)

rs<-summary(b)
rs$which
nParam<-rowSums(rs$which)
nrow(fat)

AIC<-252*log(rs$rss/252)+nParam*2
plot(nParam,AIC)

#stepwise selection with AIC

mdl_AIC<-lm(siri~.,data=train)
lmod_AIC<-step(mdl_AIC,trace = 0,direction = 'both')
summary(lmod_AIC)
nrow(summary(lmod_AIC)$coefficients)

rmse(fitted(lmod_AIC),train$siri)
rmse(predict(lmod_AIC,test),test$siri)


library(glmnet)
View(fat)
X<-as.matrix(train[-1])
Y<-as.matrix(train$siri)
cv_output<-cv.glmnet(as.matrix(train[-1]),as.matrix(train$siri),alpha=0,nfolds=10)
#alpha 0 ridge 1 lasso  in between elastic net
#10 fold cross validation to find lambda
par(mfrow=c(1,1))
plot(cv_output)


best_lam<-cv_output$lambda.min
best_lam

out.ridge<-glmnet(X,Y,alpha=0,lambda = best_lam)
out.ridge
predict.siri<-predict(out.ridge,X)
rmse(predict.siri,train$siri)
predict_test.siri<-predict(out.ridge,as.matrix(test[-1]))
rmse(predict_test.siri,test$siri)



#LASSO


cv_output<-cv.glmnet(X,Y,alpha=1,nfolds=10)
plot(cv_output)
best_lam<-cv_output$lambda.min
best_lam

out.lasso<-glmnet(X,Y,alpha=1,lambda=best_lam)
predict.Y<-predict(out.lasso,X)
rmse(predict.Y,train$siri)
predict.test.Y<-predict(out.lasso,as.matrix(test[-1]))
rmse(predict.test.Y,test$siri)


#Elastic net

#glmnet does not give alpha changing
Elastic_Net<-function(X_var,Y_var,alpha_seq){
  out_obj<-NULL
  for (i in 1:length(alpha_seq)){
    cv_output<-cv.glmnet(X_var,Y_var,alpha=alpha_seq[i],nfolds=10)
    idx<-which(cv_output$lambda==cv_output$lambda.min)
    measure=cv_output$cvm[idx]
    out_obj<-rbind(out_obj,c(alpha_seq[i],cv_output$lambda.min,measure))
  }
  
  idx<-which(out_obj[,3]==min(out_obj[,3]))
  re<-list(best=out_obj[idx,],out_obj=out_obj)
  return(re)
}
fat<-as.matrix(fat)
out<-Elastic_Net(fat[-idx_test,-1],fat[-idx_test,1],alpha_seq=c(0,0.25,0.5,0.75,1))
out

out.elastic<-glmnet(fat[-idx_test,-1],fat[-idx_test,1],alpha=out$best[1],lambda=out$best[2])
predict.Y<-predict(out.elastic,fat[-idx_test,-1])
rmse(predict.Y,train$siri)
predict.test.Y<-predict(out.elastic,fat[idx_test,-1])
rmse(predict.test.Y,test$siri)

a<-239/(239+253)
n<-239+253
9 +11+ 21 +22+ 34 +27+ 30 +35+17 +24+34 +29 +29 +26 +22 +27+30 +38 +13 +14
492-239
a*(1-a)/n

a-1.96/sqrt(n/(a*(1-a)))


u<-239/0.5 -253/0.5
i<-492/(0.5^2)
u^2/i
qchisq(0.025,1)
