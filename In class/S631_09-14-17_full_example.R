##############################################################
##############################################################
# A full example using the data: Hwtw (weight and height data)
# It produces the following output:
# 1. Produces the `lm()` output as well 
#    as the scatterplot and the regression line
# 2. Finds OLS estimates (betahat0, betahat1, and sigmahat2)
#    using both formulas and extracting them from the "lm()" function
# 3. Finds `se(betahat1)` using both formulas and "lm()" commands
# 4. Find confidence intervals for beta1, using both formulas
#    and the function `confint()`.
# 5. Perform a Hypothesis test for H_0: beta_1 = 0
#    using both formulas and the summary of the 'lm()' function
# 6. Find the prediction interval for ystar, given xstar, using both formulas
#    and the function `predict()`.
# 7. Find the confidence interval for E(ystar|X=xstar), using both formulas
#    and the function `predict()`.

#########################
library(alr4)
summary(Htwt)

# 1. Produces the `lm()` output as well 
#    as the scatterplot and the regression line
m1 = lm(wt ~ ht, data=Htwt)
summary(m1)
with(Htwt, plot(ht, wt))
abline(m1)

# 2. Finds OLS estimates (betahat0, betahat1, and sigmahat2)
#    using both formulas and extracting them from the "lm()" function
x = Htwt$ht; y = Htwt$wt
xbar = mean(x)
ybar = mean(y)
SXX = sum((x-xbar)^2)
SYY = sum((y-ybar)^2)
SXY = sum(x*y - xbar*ybar)

bh1 = SXY/SXX
bh0 = ybar - bh1*xbar
c(bh0, bh1)

# using `lm()`
coef(m1)

RSS = SYY-bh1^2*SXX
RSS
n = length(y)
n

sh = sqrt(RSS/(n-2))
sh

# using `lm()`
sigma(m1)


# 3. Finds `se(betahat1)` using both formulas and "lm()" commands
se.bh1 = sh/sqrt(SXX)
se.bh1

# using `lm()`
summary(m1)$coef[2,2]

# 4. Find confidence intervals for beta1, using both formulas
#    and the function `confint()`.
CI = .95
alpha = 1-CI
c(bh1 - abs(qt(alpha/2,n-2))*se.bh1, bh1 + abs(qt(alpha/2,n-2))*se.bh1)

# using `confint()`
confint(m1, level = CI)[2,]



# 5. Perform a Hypothesis test for
# H_0 : beta_1 = 0
# using both formulas and the summary of the 'lm()' function
t = (bh1 - 0)/se.bh1
t
p_value = 2*(1-pt(t,n-2))
p_value

# using `lm()`
summary(m1)$coef[2,]

# 6. Find the prediction interval for ystar, given xstar, using both formulas
#    and the function `predict()`.
xstar = runif(n = 1, min = min(x), max = max(x))
xstar
sepred = sh*sqrt(1+1/n+(xstar - mean(x))^2/SXX) 
sepred
ystar = bh0 + bh1*xstar
c(ystar - abs(qt(alpha/2,n-2))*sepred, ystar + abs(qt(alpha/2,n-2))*sepred)

# Prediction interval using the function "predict()"
predict(m1, newdata = data.frame(ht = xstar), interval = "prediction")

# 7. Find the confidence interval for E(ystar|X=xstar), using both formulas
#    and the function `predict()`.
sefit = sh*sqrt(1/n+(xstar - mean(x))^2/SXX) 
c(ystar - abs(qt(alpha/2,n-2))*sefit, ystar + abs(qt(alpha/2,n-2))*sefit)

# Confidence interval using the function "predict()"
predict(m1, newdata = data.frame(ht = xstar), interval = "confidence")
