rm(list=ls())
require(alr4)
options(digits=6)

# Berkeley
# Data frame BGSgirls

help(BGSgirls)
str(BGSgirls)


pairs(BGSgirls)
pairs(BGSgirls[,c("BMI18", "WT2", "WT9", "WT18")])
pairs(BGSgirls[,c("BMI18", "HT2", "HT9", "HT18")])


scatterplotMatrix(~BMI18+WT2+WT9+WT18, data=BGSgirls,
                  reg.line=lm,
                  smoother=F,
                  diagonal="none")



scatterplotMatrix(~WT2+WT9+WT18+BMI18,
                  data=BGSgirls,
                  reg.line=lm,
                  smoother=F,
                  diagonal="none")

scatterplotMatrix(~HT2+HT9+HT18+BMI18,
                  data=BGSgirls,
                  reg.line=lm,
                  smoother=F,
                  diagonal="none")

# Since BMI18 = WT18/(HT18/100)^2


m1=lm(BMI18 ~ WT18,data=BGSgirls)
summary(m1)

plot(m1, which=1, add.smooth = F)

plot(Effect("WT18", m1))
# effect plot for the linear model, shaded areas show 95% CI, line is linear model line.

summary(BGSgirls$WT18)

# Let's use WT18 and HT18

scatterplotMatrix(~ WT18 + HT18 + BMI18, data=BGSgirls, smoother = F)
m2 = lm(BMI18 ~ WT18 + HT18, data=BGSgirls)
summary(m2)


# then log BMI = log (WT18) - 2 (log (HT18) + 2log (100)


scatterplotMatrix(~ log(WT18) + log(HT18) + log(BMI18), data=BGSgirls, smoother = F)
m3 = lm(log(BMI18) ~ log(WT18) + log(HT18), data=BGSgirls)
summary(m3)


m4=lm(BMI18 ~ WT2+WT9+WT18,data=BGSgirls)

plot(BMI18 ~ WT2, data = BGSgirls)
abline(lm(BMI18 ~ WT2, data = BGSgirls))

avPlots(m4,terms="WT2")
summary(m4)

plot(m4, which = 1, add.smooth = F)

m5=lm(BMI18 ~ WT2+WT9+WT18+HT2,data=BGSgirls)
summary(m5)




BGSgirls$DW9 <- BGSgirls$WT9-BGSgirls$WT2
BGSgirls$DW18 <- BGSgirls$WT18-BGSgirls$WT9
BGSgirls$DW218 <- BGSgirls$WT18-BGSgirls$WT2

m1 <- lm(BMI18 ~ WT2 + WT9 + WT18 , BGSgirls)
plot(Effect("WT18", m1))
m2 <- lm(BMI18 ~ WT2 + DW9 + DW18 , BGSgirls)
m3 <- lm(BMI18 ~ WT2 + WT9 + WT18 + DW9 + DW18, BGSgirls)
summary(m3)
# DW9 and DW18 are perfect linear combinations of our other variables, so they have no use in the model, are not full rank, and can not be used.


compareCoefs(m1, m2,  se=TRUE)


compareCoefs(m1, m2, m3, se=FALSE)


m3$rank
m3$qr$pivot[1:m3$qr$rank]

# Multicollinearity

XtX=matrix(c(1,0,0,1),2,2)
XtX
solve(XtX)
XtX1=matrix(c(1,.99,.99,1),2,2)
XtX1
solve(XtX1)
XtX2=matrix(c(1,1,1,1),2,2)
XtX2
solve(XtX2)


## Example 2: Minnesota water use data

help("MinnWater")

pairs(~year+muniPrecip+log(muniPop)+log(muniUse),data=MinnWater)


m1 <- lm(log(muniUse) ~ year , MinnWater)
m2 <- lm(log(muniUse) ~ year+muniPrecip , MinnWater)
m3 <- lm(log(muniUse) ~ year+muniPrecip+log(muniPop) , MinnWater)

compareCoefs(m1, m2, se=FALSE)

compareCoefs(m1, m2, m3, se=FALSE)

MinnWater$logmuniPop=log(MinnWater$muniPop)
cor(MinnWater[,c("year","muniPrecip","logmuniPop")])


#VIF

m.aux = lm(year~muniPrecip+log(muniPop) , MinnWater)
r1 = summary(m.aux)$r.sq
1/(1-r1)

vif(m3)

# Condition index

X = model.matrix(~ year + muniPrecip + logmuniPop, data = MinnWater)
X.aux = model.matrix(~ -1 + year + muniPrecip + logmuniPop, data = MinnWater)
Y=log(MinnWater$muniUse)


f.center = function(x){
  (x-mean(x))
}
Xc1 = apply(X.aux,2,f.center)
Yc1 = f.center(Y)
solve(t(Xc1)%*%Xc1)%*%t(Xc1)%*%Yc1


lambda = eigen(t(Xc1)%*%Xc1)$values
V = eigen(t(Xc1)%*%Xc1)$vectors
V%*%diag(lambda)%*%t(V)
t(Xc1)%*%Xc1
sqrt(max(lambda)/min(lambda))



Xc=scale(X.aux,scale=FALSE)
Yc=scale(Y,scale=FALSE)
beta.vec=solve(t(X)%*%X)%*%t(X)%*%Y
betac.vec=solve(t(Xc)%*%Xc)%*%t(Xc)%*%Yc



summary(m3)$coef
beta.vec
betac.vec


#######################################


fuel2001$Dlic <- 1000*fuel2001$Drivers/fuel2001$Pop
fuel2001$Fuel <- 1000*fuel2001$FuelC/fuel2001$Pop
fuel2001$Income <- fuel2001$Income/1000
m.fuel1 <- lm(formula = Fuel ~ Tax + Dlic + Income +
              log(Miles),data = fuel2001)
plot(Effect("Miles", m.fuel1))
plot(Effect("Miles", m.fuel1),
     transform.x=list(Miles=c(trans=log, inverse=exp)),
     ticks.x=list(at=round(exp(7:13))), grid=TRUE, rotx=45)


#############################

m9 = lm(log(fertility)~ log(ppgdp)+lifeExpF, UN11)
m9

confint(m9)
exp(confint(m9))[3,]-1
