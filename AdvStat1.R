install.packages('faraway')
library(faraway)
data(gala)
nrow(gala)
head(gala)
attach(gala)


mdl<- lm(Species~Area + Elevation +Nearest + Scruz +Adjacent)
summary(mdl)

X<-model.matrix(~Area +Elevation+Nearest+Scruz+Adjacent)
y=Species
X[1:2,]
y[1:2]

beta = solve(t(X)%*%X) %*% t(X)%*%y

beta

#or

beta = solve(t(X)%*%X,t(X) %*% y)
#normal equation method
H= X %*% solve(t(X)%*% X) %*% t(X)

sum((H%*% H -H )^2)
sum((H %*% X -X)^2)

y_fitted= H%*%y
e=y-y_fitted
t(X) %*% e
mean(e)

#Variance of parameter estimates
n=nrow(X)
p=ncol(X)

sigma2= sum(e^2)/(n-p)  # p is number of columns!!
VarB=solve(t(X) %*% X) *sigma2
SE=sqrt(diag(VarB))

Tval= beta/SE
Pval=pt(abs(Tval), df =n-p, lower.tail=F)*2

data.frame(beta=beta, SE=SE, Tval= Tval, Pval=Pval)


#sum of square
y_mean=mean(y)
TSS= sum ((y-y_mean)^2)
ESS= sum((y_fitted-y_mean)^2)
RSS= sum((y-y_fitted)^2)
TSS
ESS
RSS
R2=1-RSS/TSS
R2
R2_adj<- 1- RSS/(n-p) / (TSS/(n-1))
R2_adj
gala$Adiff<- gala$Area - gala $Adjacent
X=model.matrix(~Area+Elevation+Nearest + Scruz + Adjacent + Adiff, data=gala)
out=try(solve(t(X)%*%X),silent = T)
out
mdl= lm (Species~Area+Elevation + Nearest + Scruz + Adjacent + Adiff, data=gala)
summary(mdl)
#lm does not make error but if system is singular it removes unnecessary variables

# some noise in the data Adiff
gala$Adiff<- gala$Area - gala $Adjacent + runif(n,0,0.1)
mdl= lm (Species~Area+Elevation + Nearest + Scruz + Adjacent + Adiff, data=gala)
summary(mdl)

#beta change multicollinearity -> unstable estimation
# R square is similar to the previous value 
detach()
