rm(list=ls())
require(alr4)
options(digits=3)


# Example 2
# How the fuel consumption change as a function of state characteristics


head(fuel2001)
help(fuel2001)
str(fuel2001)
summary(fuel2001)

# we are interested in making a better comparison between states and fueled consumed
# so we find percapita values
# also, income is divided by 1000 to better check the results
# Miles has a range over more than one order of magnitude
# log transformation

fuel2001 <- transform(fuel2001,
                      Dlic=1000 * Drivers/Pop,
                      Fuel=1000 * FuelC/Pop,
                      Income = Income/1000,
                      logMiles = log(Miles))
# transform() is a quicker way to transform a bunch of columns of data from a data frame, compared to doing each one individually (fuel2001$Miles <- log(fuel2001$Miles))

fuel2001s = fuel2001[, c("Fuel", "Dlic", "Income", "Tax", "logMiles")]
summary(fuel2001s)

scatterplotMatrix(~ Tax+Dlic+Income+logMiles+Fuel,
                  data=fuel2001,
                  diagonal="none",
                  smoother=FALSE)

scatterplotMatrix(~ Tax+Dlic+Income+logMiles+Fuel,
                  data=fuel2001,
                  diagonal="none")
# Green line is OLS line, red is the loess curve for sake of comparison.

# we could also use the correlation matrix (as a complement)
cor(fuel2001s)

#f <- fuel2001[,c(7, 8, 3, 10, 9)]
#f1 <- fuel2001[,c(7, 8, 3)]
#summary(f1)
apply(fuel2001s, 2, sd)
#round(cor(f), 3)

m.fuel = lm(Fuel ~ Tax + Dlic + Income + logMiles , data = fuel2001)
summary (m.fuel)
# The different estimated $\beta$'s show that for each additional increment, you see a change in fuel consumption, when all other regressors are held constant.

avPlots(m.fuel)

confint(m.fuel)

summary(fuel2001s)
predict(m.fuel, newdata=data.frame(Tax = 20, Dlic = 865, Income = 25, logMiles = 10 ),
        interval="confidence", 
        level=.98)

# Matrix algebra in R
X = model.matrix(Fuel ~ ., fuel2001s)
head(X)
dim(X)
n = dim(X)[1]
p1 = dim(X)[2]
Y = fuel2001s$Fuel
H = X%*%solve(t(X)%*% X)%*%t(X)
# %*% used to multiply matrices in R.  Solve() is command to find the inverse.

Bhat = solve(t(X)%*% X)%*%t(X)%*%Y
Bhat

Yhat = H%*%Y
ehat = Y - Yhat


Id = diag(n)
RSS = t(Y)%*%(Id-H)%*%Y
RSS
one = rep(1,n)
J = one%*%t(one)
dim(J)
SSreg = t(Y)%*%(H - 1/n*J)%*%Y
SYY = t(Y)%*%(Id - 1/n*J)%*%Y
c(SYY, SSreg+RSS)

R_2 = SSreg/SYY

R_2

s_hat = sqrt(RSS/(n - p1))
s_hat * sqrt(diag(solve(t(X)%*%X)))

summary(m.fuel)


