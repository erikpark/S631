library(alr4)

jevons <- jevons

m1 <- lm(Weight ~ Age, jevons, weight = 1/(SD^2/n))
summary(m1)

m2 <- lm(Weight ~ Age, jevons, weight = (n/SD^2))
summary(m2)


# So, the appropriate weight is $n/\sigma^2$ 
