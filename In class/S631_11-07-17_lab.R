library(alr4)
options(digits=4)



print(physics)
m1 <- lm(y ~ x, data=physics, weights=1/SD^2)


plot(y~x,data=physics)
abline(m1)


summary(m1)

m2 <- lm(y ~ x+I(x^2), data=physics, weights=1/SD^2)
summary(m2)


plot(y~x,data=physics)
abline(m1)
lines(physics$x, fitted(m2), col='red', pch=20)


anova(m1,m2)

summary(m2)


predict(m2,data.frame(x=c(.07)),interval="prediction")
predict(m2,data.frame(x=c(.07)),interval="prediction",weights=1/5^2)



## Example: Sniffer Data
help(sniffer)
m4 <- lm(Y ~ ., sniffer)

plot(~.,sniffer)

## The usual estimate of $Var(\hat {\boldsymbol \beta})$
X = model.matrix(m4)
Y = sniffer$Y
n = length(Y)
p1 = dim(X)[2]
H = X%*%solve(t(X)%*%X)%*%t(X)
sigma2hat = t(Y)%*%(diag(n)-H)%*%Y/(n-p1)
hatvarbeta = c(sigma2hat)*solve(t(X)%*%X)
hatvarbeta
#or simply
vcov(m4)

sqrt(diag(vcov(m4)))
summary(m4)$coef[,2]

e = (diag(n)-H)%*%Y
dim(hii)
hii = diag(H)
hatvarbetaw = solve(t(X)%*%X)%*%t(X)%*%diag(c(e^2)/(1-hii)^2)%*%X%*%solve(t(X)%*%X)
sqrt(diag(hatvarbetaw))


hccm(m4, type="hc3")
sqrt(diag(hccm(m4, type="hc3")))

library(lmtest)


summary(m4)$coef
coeftest(m4, vcov=hccm)

cbind("OLS Est"=summary(m4)$coef[,1],
      "OLS SE"=sqrt(diag(vcov(m4))),
      "HC3 SE"=sqrt(diag(hccm(m4, type="hc3"))))


## A Test for Constant Variance


plot(m4,which = 1,add.smooth=F)


X=model.matrix(Y~.,sniffer)
Y=sniffer$Y
n=dim(sniffer)[1]
H=X%*%solve(t(X)%*%X)%*%t(X)
RSS=c(t(Y)%*%(diag(n)-H)%*%Y)
e=(diag(n)-H)%*%Y
U=n*e^2/RSS
lambda=solve(t(X)%*%X)%*%t(X)%*%U
lin_com=X%*%lambda


e=resid(m4)
op=par(mfrow=c(2,2)) 
with(sniffer,plot(fitted(m4),e))
with(sniffer,plot(GasPres,e))
with(sniffer,plot(TankTemp,e))
with(sniffer,plot(lin_com,e))
par(op)


## Constructing the test for nonconstant variance


m4 <- lm(Y ~ ., sniffer)
X=model.matrix(Y~.,sniffer)
Y=sniffer$Y
n=dim(sniffer)[1]
H=X%*%solve(t(X)%*%X)%*%t(X)
RSS=c(t(Y)%*%(diag(n)-H)%*%Y)
e=(diag(n)-H)%*%Y
U=n*e^2/RSS
Z=fitted(m4)
m.aux=lm(U~Z)
SSreg=sum((fitted(m.aux)-mean(U))^2)
S=SSreg/2
p_value=1-pchisq(S,df=1)


c("S"=S,"P-value"=p_value)

ncvTest(m4)

Z=cbind(1,sniffer$TankTemp,sniffer$GasTemp)
H.aux=Z%*%solve(t(Z)%*%Z)%*%t(Z)
J=rep(1,n)%*%t(rep(1,n))
S=c(t(U)%*%(H.aux-1/n*J)%*%U)/2
c("S"=S,"P-value"=1-pchisq(S,df=dim(Z)[2]-1))
ncvTest(m4, ~ TankTemp + GasTemp)


Z1=with(sniffer,ncvTest(m4, ~ GasPres))
Z2=with(sniffer,ncvTest(m4, ~ TankTemp))
Z3=with(sniffer,ncvTest(m4, ~ GasPres+TankTemp))
Z4=with(sniffer,ncvTest(m4, ~ GasPres+TankTemp+GasTemp+TankPres))
Z5=with(sniffer,ncvTest(m4))
table1=rbind(with(Z1,c(Df,ChiSquare,p)),with(Z2,c(Df,ChiSquare,p)),with(Z3,c(Df,ChiSquare,p)),with(Z4,c(Df,ChiSquare,p)),with(Z5,c(Df,ChiSquare,p)))
row.names(table1)=c("GasPres","TankTemp","TankTemp,GasPres","TankTemp,GasTemp,TankPres,GasPres","Fitted values")
colnames(table1)=c("df","Test statistic","p-Value")
table1

1-pchisq(Z4$ChiSquare-Z3$ChiSquare,Z4$Df-Z3$Df)

1-pchisq(Z3$ChiSquare-Z2$ChiSquare,Z3$Df-Z2$Df)

1-pchisq(Z3$ChiSquare-Z1$ChiSquare,Z3$Df-Z1$Df)

m4a <- lm(Y ~ ., sniffer, weights = TankTemp)
summary(m4)
summary(m4a)
