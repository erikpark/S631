# Simulation
set.seed(1031)
n.means = 1000
y.means = rchisq(n.means,2)
y.means = sort(y.means)
beta0 = -3
beta1 = .08
x = (y.means-beta0)/beta1

n.values = 1000
y.vec = rep(0,n.values*length(y.means))
for (i in 1:length(y.means)){
  ei = rnorm(n.values, mean = 0, sd = 1)
  yi = beta0 + beta1*x[i] + ei
  y.vec[(n.values*(i-1)+1):(n.values*i)] = yi
}

x.vec = rep(x, each = n.values)
samp1 = sample(1:(n.means*n.values),100)
plot(x.vec[samp1],y.vec[samp1]) # Plot of 100 points sampled from the random simulation of 1000 points.
# Plot looks generally linear, which is good!  Also, as the x and y values increase, see that the amount of variance decreases (the range decreases).
# Means that our assumption of homoscedasticity probably doesn't fit here.

library(alr4)
scatterplotMatrix(~x.vec[samp1]+y.vec[samp1])
# Green line is OLS regression line.  Red line is loess method line.  Used here to compare how well our linear regression fits compared to it.
# Two lines don't match up well, so likely that we are violating some of the assumptions in our model.


# Example 1

require(alr4)
str(UN11) # shows structure of data
un11 = UN11[,c("ppgdp", "fertility", "lifeExpF")]
summary(un11)
head(un11)
scatterplotMatrix(~ ppgdp + fertility + lifeExpF, data = un11)
scatterplotMatrix(~ ppgdp + fertility + lifeExpF,
                  smooth=FALSE, diagonal="none", data = un11)


scatterplotMatrix(~ log(ppgdp) + fertility + lifeExpF,
                  smooth=FALSE, data = un11) # After adding in a log transformation for ppgdp, really improves the fit of the model and evens out the distribution.
scatterplotMatrix(~ log(ppgdp) + fertility + lifeExpF,
                  smooth=FALSE, diagonal="none", data = un11)


m1=lm(lifeExpF ~ log(ppgdp),un11)
m1
summary(m1)
m2=lm(lifeExpF ~ fertility, un11)
m2
summary(m2)

m.full <- lm(lifeExpF ~ log(ppgdp) + fertility, data=un11)
m.full
summary(m.full) # Together, the two predictors explain more than they do seperately, but it isn't additive.  There is a relationship between fertility and ppgdp, so there is overlap between what they explain.
# This relationship between the two can be seen in the scatterplot matrix where we see a non-zero, linear relationship between the two.

oldpar = par(mfrow=c(1, 2))
plot(lifeExpF ~ log(ppgdp),un11)
abline(m1)
plot(lifeExpF ~ fertility,un11)
abline(m2)
par(oldpar) # Plots show relationship (correlation) between life expentency and ppgdp.

m_2on1 <- lm(fertility ~ log(ppgdp), un11)
summary(m_2on1) # From R^2 we see that 52% of changes in fertility are explained by changes in ppgdp.
plot(fertility ~ log(ppgdp), un11)
abline(m_2on1)

oldpar = par(mfrow=c(1, 2))
m_resid <- lm(resid(m1) ~ resid(m_2on1))
plot(resid(m1) ~ resid(m_2on1))
abline(m_resid, col="Red")
plot(lifeExpF ~ fertility,un11)
abline(m2, col="Red")
par(oldpar)
# Plot comparing the residuals between the two variables. Residuals from 2on1 are all the info not explained from regression of x2 on x1. Y axis is all information on y not explained from the linear regression.
# So, here on left plot we see that Y axis is the variation seen in response not explained by first regressor (log(ppgdp)). X axis is part of new regressor (fertility) not explained by the first one (ppgdp)

m.full <- lm(lifeExpF ~ log(ppgdp) + fertility, data=un11)
avPlots(m.full, id.n=0) # equivalent to finding the residuals, running the regression of the residuals, and looking at the effects.

coef(m2)
coef(m.full) # Can interpret log(ppgdp) and fertiltiy alone, by saying that they are the changes you see in Y by increasing one unit of them, keeping all others constant. (Y increases by 2.42 for every unit increase in log(ppgdp))
summary(m.full)

# Example 2

head(fuel2001)
help(fuel2001)
str(fuel2001)
summary(fuel2001)

fuel2001 <- transform(fuel2001,
                      Dlic=1000 * Drivers/Pop,
                      Fuel=1000 * FuelC/Pop,
                      Income = Income,
                      logMiles = log(Miles))
scatterplotMatrix(~ Tax+Dlic+Income+logMiles+Fuel,
                  data=fuel2001,
                  diagonal="none",
                  smoother=FALSE)

f <- fuel2001[,c(7, 8, 3, 10, 9)]
f1 <- fuel2001[,c(7, 8, 3)]
summary(f1)
apply(f, 2, sd)
round(cor(f), 3)

m.fuel = lm(Fuel ~ Tax + Dlic + Income + log(Miles) , data = fuel2001)
summary (m.fuel)
avPlots(m.fuel)



## Example 3: Berkeley Guidance Study


hist(BGSgirls$BMI18)

scatterplotMatrix(~ HT2 + WT2 + WT9 + HT9 + ST9 + BMI18, BGSgirls,
                  smooth=FALSE, subset = BMI18 < 35)

print(cor(BGSgirls[, c("HT2", "WT2", "HT9", "WT9", "ST9", "BMI18")]), digits=3)
sel <- BGSgirls$BMI18< 35 
print(
  with(BGSgirls[sel ,],
       cor(BMI18, cbind(HT2, WT2, HT9, WT9, ST9))))

scatterplotMatrix(~BMI18 + WT9 + ST9, BGSgirls,
                  smooth=FALSE, diagonal="none")


r1 <- residuals(lm(BMI18 ~ WT9, BGSgirls))
r2 <- residuals(lm(ST9 ~ WT9, BGSgirls))
m.res <- lm(r1 ~ r2)
plot(r1 ~ r2,
     xlab=expression(paste(hat(e), " from ST9 on WT9")),
     ylab=expression(paste(hat(e), " from BMI18 on WT9")))
grid(col="gray", lty="solid")
abline(m.res)

m3=lm(BMI18~WT9+ST9,data=BGSgirls)
avPlots(m3, id.n=0)

m4=lm(formula = BMI18 ~ HT2 + WT2 + HT9 + WT9 + ST9, data = BGSgirls)
summary(m4)


