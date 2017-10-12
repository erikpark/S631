# Let's first do a simulation to study the linear regression.

beta0 = 1 #assigns value to beta 1
beta1 = .5 
sigma_2 = 100

# Now let's create a sample
n = 20 # sample size


set.seed(631) # set.seed is used to introduce a random seed
x = sample(50:100, n, replace = TRUE)
x[n] = 120 # a vector of values for the predictor

# the object x is a vector in R, it contains "n" number of values.
# Above command changes the nth value for 120, where here the nth value is 20 as defined above.
# Square bracketes around n used to select specific value from a vector


# e is a vector of error terms
e = rnorm(n, mean = 0, sd = sqrt(sigma_2))
# now we can generate the sample of "n" responses.

y = beta0+beta1*x+e

# So, now have worked backwards to generate predictor and response data (x and y) from beta0,1 and sigma_2

plot(y ~ x)
abline(beta0, beta1) # Solid line showing "true relationship" 
abline(lm(y ~ x), lty = 2) # Dashed line showing line of best fit generated from 20 x and y, sample values.


SXX = sum(x^2)-n*mean(x)^2
SXX


# the object "sim" represents the number of replicants in the simulation
sim = 10000

# the object "rep" creates a vector of repititions
bh1.vec = rep(0,sim)
bh0.vec = rep(0,sim)

# for loop used here to create several sims.  Here from 1 to the number given by "sim" (10000).
for (i in 1:sim){
  e = rnorm(n, mean = 0, sd = sqrt(sigma_2))
  y = beta0+beta1*x+e # only thing changing here is the errors "e", but y will still change.
  m1 = lm(y ~ x)
  bh1.vec[i] = coef(m1)[2]
  bh0.vec[i] = coef(m1)[1]
  #abline(lm(y ~ x), lty = 2, col = i)
}
mean(bh0.vec)
sd(bh0.vec)
norm.bh0 = rnorm(n = sim,mean(bh0.vec),sd(bh0.vec))

mean(bh1.vec) # so, gets really close to actual b0 and b1
sd(bh1.vec)
norm.bh1 = rnorm(n = sim,mean(bh1.vec),sd(bh1.vec))

qqplot(norm.bh1,bh1.vec)






require(alr4)
summary(Forbes)
attach(Forbes)

SXX = sum((bp-mean(bp))^2)
SXY = sum((bp-mean(bp))*(lpres-mean(lpres)))
SYY = sum((lpres-mean(lpres))^2)
betahat1 = SXY/SXX
betahat0 = as.numeric(mean(lpres) - betahat1 * mean(bp))
print(c(betahat0 = betahat0, betahat1 = betahat1),
      digits = 4)


RSS = SYY - SXY^2/SXX
sigmahat2 = RSS/15
sigmahat = sqrt(sigmahat2)
c(RSS = RSS, sigmahat2 = sigmahat2, sigmahat = sigmahat)


# The function lm is the standard function for linear regression (simple and multiple)
m1 = lm(lpres~bp,Forbes)

# "attributes" is a function that shows you the structure of your object
attributes(m1)
m1$coefficients

# "summary" is a function which gives a summary of the object. Perhaps the most useful function for this class.
summary(m1)
attributes(summary(m1))
summary(m1)$sigma
sigmaHat(m1)
RSS = m1$df.residual * sigmaHat(m1)^2
RSS



betahat=as.numeric(coef(m1))
ses <- as.numeric(summary(m1)$coef[,2])
(tval <- qt(1-.05/2, lm_Forbes$df))
data.frame(Est = betahat,lower=betahat - tval * ses,
           upper=betahat + tval * ses)
confint(m1, level=.95)



## Simulation: Confidence Intervals for the Intercept

require(plotrix)


ci_betas <- function(e=rnorm(100) , x=1:length(e), beta0=18, beta1=4, CI=.95) {
  y=beta0+beta1*x+e
  m=lm(y~x)
  c(coef(m),t(confint(m, level = CI)))
}
colorbeta0 <- function(bounds, marker) { if (marker < bounds[3]) "Red"  else if (marker > bounds[4]) "Orange" else "Black" }

# Sample Size
n <- 40

# Parameters for linear model
beta0=18
beta1=4
e_mu=0 #the mean of the error term
e_sd=3 #the standard deviation of the error term
x=1:n #an arbitrary vector of predictors of size n

# Simulation values
n.reps = 100  # The Number of Replicates
ybar =beta0+beta1*mean(x)  
set.seed(138)
e <- replicate(n.reps, rnorm(n, mean = 0, sd = 3)) 

cibeta <- apply(e, 2, ci_betas)
zbeta0 <- apply(cibeta, 2, colorbeta0, beta0)

#plot for the CIs for beta0
plotCI(x = 1:n.reps, y = cibeta[1,], sfrac=.003, pch=1, cex=.5,
       li = cibeta[3, ], ui = cibeta[4, ], 
       col = zbeta0, lwd = .1, xlab="Iterations", ylab="Beta0") 
abline(h = beta0, col="blue", lwd = .5, lty=2) 


## Simulation: Confidence Intervals for the Slope


colorbeta1 <- function(bounds, marker) { if (marker < bounds[5]) 
  "Red"  else if (marker > bounds[6]) "Orange" else "Black" }
zbeta1 <- apply(cibeta, 2, colorbeta1, beta1)

#plot for the CIs for beta1
plotCI(x = 1:n.reps, y = cibeta[2,], sfrac=.003, pch=1, cex=.5,
       li = cibeta[5, ], ui = cibeta[6, ], 
       col = zbeta1, lwd = .1, xlab="Iterations", ylab="Beta1") 
abline(h = beta1, col="blue", lwd = .5, lty=2) 










