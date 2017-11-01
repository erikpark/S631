require(alr4)


names(UN11)
class(UN11$group)
levels(UN11$group)
str(UN11)
plot(lifeExpF ~ group, data=UN11)
Boxplot(lifeExpF ~ group, data=UN11, id.n=2)

# Let's look at the dummy variables
head(model.matrix(~ -1 + group, UN11), 10)

# And this is what R does by default (model matrix X)
head(model.matrix(~group, UN11), 10)


m1=lm(lifeExpF~group,data=UN11)
summary(m1)$coef
bd=summary(m1)$coef
plot(Effect(c("group"), m1))

#Review of matrix algebra

X = model.matrix(~group, data = UN11)
n = dim(X)[1]
p1 = dim(X)[2]
Y = UN11$lifeExpF
H = X%*%solve(t(X)%*%X)%*%t(X)
ehat = Y- H%*%Y
s2 = 1/(n-p1)*t(ehat)%*%ehat
1/(n-p1)*t(Y)%*%(diag(n) - H)%*%Y
c(s2)*solve(t(X)%*%X)
vcov(m1)

# Test of significance for beta2-beta3=0
a=c(0,1,-1)
se_b2b3=sqrt(t(a)%*%vcov(m1)%*%a)
se_b2b3
b2b3=as.numeric(coef(m1)[2]-coef(m1)[3])
t_val=b2b3/se_b2b3
p_val=2*(1-pt(abs(t_val),m1$df))
c("b2-b3"=b2b3,"SE"=se_b2b3,"t-Value"=t_val,"p-Value"=p_val)

## Adding a Continuous Predictor
scatterplot(lifeExpF ~ log(ppgdp)| group, data=UN11,
            smooth=FALSE, boxplots=FALSE,
            ylab="Female Life Expectancy")
m3long=lm(lifeExpF~group+log(ppgdp)+group:log(ppgdp),
          data=UN11)
#or simply
m3=lm(lifeExpF~group*log(ppgdp),data=UN11)
m3

plot(Effect(c("group", "ppgdp"), m3, xlevels=list(ppgdp = 100)),
     rug=FALSE, grid=TRUE, multiline=TRUE)
plot(Effect(c("group", "ppgdp"), m3, xlevels=list(ppgdp = c(100, 10000, 40000) )),
     rug=FALSE, grid=TRUE, multiline=TRUE)
plot(Effect(c("group", "ppgdp"), m3,  ),
     rug=FALSE, grid=TRUE, multiline=TRUE,
     transform.x=list(ppgdp=list(trans=log, inverse=exp)),
     ticks.x=list(ppgdp=list(at=c(20000, 40000, 60000, 80000, 99999))))

# or alternatively

UN11$lppgdp=log(UN11$ppgdp)
m3a=lm(lifeExpF~group*lppgdp,data=UN11)
plot(Effect(c("group", "lppgdp"), m3a, xlevels=100),
     rug=FALSE, grid=TRUE, multiline=TRUE)

## Interpreting Output
round(summary(m3)$coef,3)

scatterplot(lifeExpF ~ log(ppgdp)| group, data=UN11,
            smooth=FALSE, boxplots=FALSE,
            ylab="Female Life Expectancy")

## The Main Effects Model

m2=lm(lifeExpF~group+log(ppgdp),data=UN11)
round(summary(m2)$coef,3)

plot(allEffects(m2, xlevels=50), ylim=c(60,85),
     grid=TRUE, multiline=TRUE)

## Many Factors
## Example: Textile Experiment: Worsted Yarn (Box and Cox, 1964)
str(Wool)
var_nam = c("len","amp","load")
Wool[,var_nam] = lapply(Wool[,var_nam],factor) 
str(Wool)
mw1=lm(log(cycles)~.,data=Wool); mw3=lm(log(cycles)~len*amp*load,data=Wool)
mw2=lm(log(cycles)~len*amp+len*load+amp*load,data=Wool)
plot(allEffects(mw2), grid=TRUE, multiline=TRUE)



## Polynomial Regression
## Example: Cakes

m6 <- lm(Y ~ X1 + X2 + I(X1^2) + I(X2^2) + X1:X2, data=cakes)
#or simply
m6a <- lm(Y ~ polym(X1, X2, degree=2, raw=TRUE), data=cakes)
colnames(model.matrix(~ polym(X1, X2, degree=2, raw=TRUE), data=cakes))

z <- with(cakes, polym(X1, X2, degree=2, raw=TRUE))
head(model.matrix(~ z))

plot(allEffects(m6), grid=TRUE, multiline=TRUE)

## Cubic Regression: Simulated Data

options(digits = 6)
set.seed(191)
x = .02 * runif(20)
y = 1 + 2*x - 1*x^2 - 1.5*x^3 + rnorm(20)
m5 = lm(y ~ x + I(x^2) + I(x^3))
#or alternatively
m5a = lm(y ~ poly(x, 3, raw=TRUE))
summary(m5)
summary(m5a)

plot(Effect("x", m5))

summary(m5a <- lm(y ~ poly(x, 3, raw=TRUE)))$coef
summary(m5b <- lm(y ~ poly(x, 3)))$coef

cbind(predict(m5a),predict(m5b))[1:10,]

