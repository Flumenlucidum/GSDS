#Class

library(faraway)
data(gala)
hist(gala$Species)

par(mfrow=c(1,3))
hist(gala$Species)
hist(log(gala$Species))
hist(sqrt(gala$Species))
attach(gala)
View(gala)
mdl_log<-lm(log(Species)~Area+Elevation+Nearest+Scruz+Adjacent,data=gala)
mdl_sqrt<-lm(sqrt(Species)~Area+Elevation+Nearest+Scruz+Adjacent,data=gala)
summary(mdl_log)
summary(mdl_sqrt)

par(mfrow=c(2,2))
plot(mdl_log,which=1)
plot(mdl_sqrt,which=1)
plot(mdl_log,which=2)
abline(0,1)
plot(mdl_sqrt,which=2)
abline(0,1)

#Leverage Calculation
X=model.matrix(~Area+Elevation+Nearest+Scruz+Adjacent,data=gala)
n<-nrow(X)
p<-ncol(X)
H<-X%*%solve(t(X)%*% X)%*% t(X)
h<-diag(H)
sum(h)
par(mfrow=c(1,1))
plot(h)
abline(h=2*p/n)
# In calculation y is not used 

#Studentized residual 

res<-resid(mdl_sqrt)
Sigma<-summary(mdl_sqrt)$sigma

stures<-res/(Sigma*sqrt(1-h))

par(mfrow=c(2,2))
plot(res~fitted(mdl_sqrt))
plot(stures~fitted(mdl_sqrt))
plot(res,stures)

#Influential point -> Cook's Distance
D=1/p * res^2 *(h/(1-h))
length(which(D>1))
plot(D)

mdl_sqrt_1=lm(sqrt(Species)~Area+Elevation+Nearest+Scruz+Adjacent,subset = (D<1),data=gala)
mdl_sqrt$coefficients
mdl_sqrt_1$coefficients

col=rep('black',length(D))
col[D>1]='red'
par(mfrow=c(1,1))
plot(sqrt(Species)~ Area, data=gala, col=col)
abline(lm(sqrt(Species) ~ Area,data=gala))

#Partial Regression

gala1<-gala[D<1,]
gala1.m<-as.matrix(gala1)
IDX_Predictor<-3:7
par(mfrow=c(2,3))

for (i in 1:5){
  idx.x<-IDX_Predictor[i]
  idx.nox<-IDX_Predictor[-i]
  resid.y=resid(lm(sqrt(gala1.m[,1])~gala1.m[,idx.nox]))
  resid.x=resid(lm(gala1.m[,idx.x]~gala1.m[,idx.nox]))
  plot(resid.y ~ resid.x)  
}
par(mfrow=c(1,1))


#Weighted Least Square 
# Y= 1 + 0.5 X1 + 0.5 X2 + e 
#V(ei)=sigma_i ^2 
#sigma_i from chi square df 3 
# X1 generated from a standard normal dist 
#X2 generated from Bernoulli  p=0.5 

set.seed(100)
n<-100
sigma<-rchisq(n,df=3)
X1<-rnorm(n)
X2<-rbinom(n,1,0.5)
Y<-1+0.5*X1+0.5*X2+rnorm(n,sd=sigma)

X=cbind(1,X1,X2)
V<-diag(sigma^2)
W<-diag(1/sigma^2)

beta_OLS<-solve(t(X)%*% X, t(X) %*%Y)
beta_WLS<-solve(t(X)%*%W%*%X, t(X)%*%W%*%Y)

beta_OLS
beta_WLS

#lm WLS
lm(Y~X1+X2,weights = 1/sigma^2)

#generating multiple times
nsim<-1000
beta_OLS_A<-matrix(rep(0,nsim*3),ncol=3)
beta_WLS_A<-beta_OLS_A

for (i in 1:nsim){
  Y<-1+0.5*X1+0.5*X2+rnorm(n,sd=sigma)
  X<-cbind(1,X1,X2)
  V<-diag(sigma^2)
  W<-diag(1/sigma^2)
  beta_OLS<-solve(t(X)%*%X,t(X)%*%Y)
  beta_WLS<-solve(t(X)%*%W%*%X,t(X)%*%W%*%Y)
  beta_OLS_A[i,]<-beta_OLS
  beta_WLS_A[i,]<-beta_WLS
  
}

colMeans(beta_OLS_A)
colMeans(beta_WLS_A)

#variance
apply(beta_OLS_A,2,var)
apply(beta_WLS_A,2,var)

#INT
n<-1000
x1<-rnorm(n)
y<-x1*0.3+rcauchy(n)

Z<-qnorm(rank(y)/(n+1))
out1<-lm(y~x1)
out2<-lm(Z~x1)
summary(out1)
summary(out2)
par(mfrow=c(2,2))
plot(out1,which=2)
plot(out2,which=2)

#When effect size is large 
#INT changes the y not considering X's effect

n<-1000
X1<-rnorm(n) #main interest
X2<-rnorm(n) # predictor to adjust for

y<-0.3*X1+X2+rcauchy(n)
res_y<-resid(lm(y~X2))
Z<-qnorm(rank(res_y)/(n+1))
out2<-lm(Z~X1) # X2 together is fine 
par(mfrow=c(1,1))
plot(out2,which=2)


#Assignment

data("cheddar")
View(cheddar)
#1 (a)
mdl_cheddar=lm(taste~Acetic+H2S+Lactic,data=cheddar)
summary(mdl_cheddar)

#(b)
mdl_ched_original=lm(taste~exp(Acetic)+exp(H2S)+Lactic,data=cheddar)
summary(mdl_ched_original)

#(c)
anova(mdl_cheddar,mdl_ched_original)

#(d)
3.9118/100

#2
#(a)
data(sat)
View(sat)
mdl_sat<-lm(total~expend+ratio+salary,data=sat)
summary(mdl_sat)
mdl_sat_sub1<-lm(total~expend+ratio,sat)
anova(mdl_sat_sub1,mdl_sat)
mdl_sat_sub2=lm(total~1,data=sat)
anova(mdl_sat_sub2,mdl_sat)

#(b)
nsim<-10000
summary(mdl_sat)$fstatistic
f_obs<-summary(mdl_sat)$fstatistic[1]
fstats<-numeric(nsim)

for (i in 1:nsim){
  mdl<-lm(sample(total)~expend+ratio+salary,data=sat)
  fstats[i]<-summary(mdl)$fstatistic[1]
}
p_val=(sum(fstats>f_obs)+1)/(nsim+1)
p_val

#(c)
summary(mdl_sat)
qt(0.975,46)
-8.823+c(-1,1)*qt(0.975,46)*4.697
confint(mdl_sat)

nsim<-10000
set.seed(123)
mat_boot<-matrix(NA,nsim,4)
resids<-resid(mdl_sat)
preds<-fitted(mdl_sat)
for (i in 1:nsim){
  y_boot=preds+sample(resids,replace = T)
  mdl_boot<-update(mdl_sat,y_boot~.)
  mat_boot[i,]<-coef(mdl_boot)
}

colnames(mat_boot)<-c('Intercept',colnames(sat[,1:3]))
mat_boot<-data.frame(mat_boot)
apply(mat_boot,2,function(x) quantile(x,c(0.025,0.975)))

#3
#(a)

mdl_sat<-lm(total~expend+ratio+salary+takers,data=sat)
hist(sat$total)
plot(mdl_sat,which=1)
mdl_sat<-lm(log(total)~expend+ratio+salary+takers,data=sat)
plot(mdl_sat,which=1)

#(b)
plot(mdl_sat,which=2)
par(mfrow=c(1,1))
hist(sat$total)
hist(log(sat$total))

#(c)
X<-model.matrix(~expend+ratio+salary+takers,data=sat)
Hat<-X%*% solve(t(X)%*%X)%*% t(X)
h<-diag(Hat)
plot(h)
abline(h=2*5/50)

#(d)
plot(mdl_sat,which=1)
a<-summary(mdl_sat)
abline(h=-3*0.03311)
abline(h=3*0.03311)

#(e)
#studentized residual
p=4+1
sigma<-summary(mdl_sat)$sigma
stures<-resid(mdl_sat)/(sigma*sqrt(1-h))
D=1/p * stures^2 *(h/(1-h))
length(which(D>1))

#(f)
summary(mdl_sat)
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
for (i in 1:4){
  plot(log(sat$total)~X[,i+1], main=colnames(sat)[i])
}       
plot(log(sat$total)~sat$expend)
log(sat$total)
par(mar=c(1,1,1,1))

#4
nsim<-10000
coef_mat<-matrix(NA,nsim,6)

beta_0=1
beta_1=0

for (i in 1:nsim){
X<-rbinom(30,1,0.1)
e_lognormal=rlnorm(30)
e_cauchy=rcauchy(30)
e_unif=runif(30,-1,1)
Y=beta_0+beta_1*X+e_lognormal
coef_mat[i,c(1,2)]<-summary(lm(Y~X))$coefficients[c(2,8)]
Y=beta_0+beta_1*X+e_cauchy
coef_mat[i,c(3,4)]<-summary(lm(Y~X))$coefficients[c(2,8)]
Y=beta_0+beta_1*X+e_unif
coef_mat[i,c(5,6)]<-summary(lm(Y~X))$coefficients[c(2,8)]
}

#(a) bias of beta 1 
colnames(coef_mat)<-c('b_ln','p_ln','b_cauchy','p_cauchy','b_unif','p_unif')
par(mfrow=c(2,2))
for (i in c(1,3,5)){
hist(coef_mat[,i],main=colnames(coef_mat)[i])  
}
mean(coef_mat[,1])
mean(coef_mat[,3])
mean(coef_mat[,5])

#(b) 0.05 0.01 0.001
mat_p<-matrix(NA,3,3)


for (i in 1:3){
  
mat_p[i,1]<-length(which(coef_mat[,2*i]<0.05))/nsim
mat_p[i,2]<-length(which(coef_mat[,2*i]<0.01))/nsim
mat_p[i,3]<-length(which(coef_mat[,2*i]<0.001))/nsim

}
mat_p
