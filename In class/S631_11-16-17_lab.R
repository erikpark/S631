library(alr4)

scatterplot(BrainWt ~ BodyWt, brains, id.n=3, boxplots=FALSE, smooth=FALSE, reg=FALSE)

summary(brains)


oldpar = par(mfrow=c(2, 2))
with(brains,plot(BodyWt,BrainWt))
with(brains,plot(1/BodyWt , 1/BrainWt))
with(brains,plot(sqrt(BodyWt) , sqrt(BrainWt)))
with(brains,plot(BodyWt^(1/3) , BrainWt^(1/3)))
par=oldpar

# Using the log (empirical) rule
scatterplot(log(BrainWt) ~ log(BodyWt), brains, id.n=3, 
            boxplots=FALSE, smooth=FALSE, reg=FALSE)

# Find lambdahat that minimizes RSS(lambda) from mean function in  8.4 (see textbook)
with(ufcwc, invTranPlot(Dbh, Height))

# confidence interval
with(ufcwc, invTranEstimate(Dbh,Height))

m1 = lm(Height ~ Dbh, ufcwc)
summary(m1)
m2 = lm(Height ~ log(Dbh), ufcwc)
summary(m2)




head(basicPower(brains$BrainWt, 0))
head(brains$BrainWt)
head(log(brains$BrainWt))
head(bcPower(brains$BrainWt, 0, jacobian.adjusted=FALSE))
head(bcPower(brains$BrainWt, 0, jacobian.adjusted=TRUE))
head(yjPower(brains$BrainWt, 0, jacobian.adjusted=FALSE))

plot(bcPower(brains$BrainWt, 0, jacobian.adjusted=FALSE), 
     brains$BodyWt)

# Example using Box and Cox method: 
# Highway data: Automobile accident rate (response) and 
# highway information

var1 = c("rate","len","adt","trks","slim","shld","sigs")
summary(Highway[,var1])
scatterplotMatrix(Highway[,var1],smoother = F, reg.line = F, diagonal = "none")

# Make values positive (to be able to use Box-Cox method)
Highway$sigs1 = with(Highway, (sigs * len + 1)/len)

# Using the Box-Cox method in relevant variables
bc1 = powerTransform(cbind(len, adt, trks, shld, sigs1) ~ 1, Highway)
summary(bc1)
testTransform(bc1, c(0,0,0,1,0))

X.mat = with(Highway,cbind(rate,"log.len"=log(len),"log.adt"=log(adt),
                           "log.trks"=log(trks),slim,shld,"log.sigs1"=log(sigs1) ))
scatterplotMatrix(X.mat,smoother = F, reg.line = F, diagonal = "none")

bc2 = powerTransform(cbind(len, adt, trks, sigs1) ~ htype, Highway)
summary(bc2)

m2 = lm(rate ~ log(len) + log(adt) + log(trks) 
         + slim+ shld + log(sigs1) , Highway)
p2=inverseResponsePlot(m2)
p2
boxCox(m2)

summary(powerTransform(m2))

summary(m2)

summary(powerTransform(cbind(len, adt, trks, shld, sigs1) ~ 1,
                       data=Highway, family="yjPower"))

