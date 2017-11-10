## Example: Fuel Data

library(alr4)
fuel2001 <- transform(fuel2001, Dlic=1000*Drivers/Pop,
                      Fuel=1000*FuelC/Pop, Income=Income/1000)
names(fuel2001)


## ANOVA

X = model.matrix(Fuel ~ Tax + Dlic + Income + log(Miles), data = fuel2001)
Y = fuel2001$Fuel
H = X%*%solve(t(X)%*%X)%*%t(X)
n = dim(X)[1]
p1 = dim(X)[2]
RSS_F = t(Y)%*%(diag(n)-H)%*%Y
df2 = n - p1


X1 = model.matrix(Fuel ~ Tax + Dlic, data = fuel2001)
df1 = dim(X)[2]-dim(X1)[2]
H_R = X1%*%solve(t(X1)%*%X1)%*%t(X1)
RSS_R = t(Y)%*%(diag(n)-H_R)%*%Y
SSreg = RSS_R - RSS_F
c(SSreg,RSS_F)

Fstat = (SSreg/df1)/(RSS_F/df2)
Fstat

## Linear Models

m0 <- lm(Fuel ~ 1, fuel2001)
m1 <- update(m0, ~ Tax)
m2 <- update(m1, ~ . + Dlic)
m3 <- update(m2, ~ . + Income + log(Miles))


anova(m2,m3)

anova(m0, m3)
summary(m3)
anova(m1, m2)
anova(m2, m3)
anova(m1,m2,m3)
anova(m3)
Anova(m3)


## Example: United Nations data (UN11)

#* Observational study
#* $n=199$ localities (mainly countries)
#* The variable `group` is a factor (categorical variable) with $d=3$ levels (categories)
#* The variable `lifeExpF` will be used as a response 

m1=lm(lifeExpF~group+log(ppgdp)+log(ppgdp):group,data=UN11)
Anova(m1)
plot(Effect(c("group", "ppgdp"), m1, default.levels=100),
     rug=FALSE, grid=TRUE, multiline=TRUE)

### Type I 
#* Sequential analysis of variance

## UN Data: Type I Anova
summary(m3)
anova(m1)
## UN Data: Type II Anova
Anova(m1)

## UN Data: Type III Anova

Anova(m1,type = "III")

## Example: Textile Experiment: Worsted Yarn (Box and Cox, 1964)

#* Strenght of wool (number of cycles until the specimen fails, `cycles`) as a function of
#* Lenght of test specimen (`len`), amplitude of loading cycle (`amp`), and load put on the specimen (`load`)

var_nam = c("len","amp","load")
Wool[,var_nam] = lapply(Wool[,var_nam],factor) 

# recall to change variable type to factors 

contrasts(Wool$len)=contrasts(Wool$amp)=contrasts(Wool$load)="contr.treatment"
m2=lm(log(cycles)~len*amp+len*load+amp*load,data=Wool)
anova(m2)
Anova(m2)

#See An R Companion to Applied Regression 4.4.4
contrasts(Wool$len)=contrasts(Wool$amp)=contrasts(Wool$load)="contr.sum"
m2.adj<-update(m2)
Anova(m2.adj)
Anova(m2.adj,type="III")

m2a=lm(log(cycles)~load+len*amp,data=Wool)
Anova(m2a)
plot(allEffects(m2a), grid=TRUE, multiline=TRUE)

## Minnesota Farm Sales

MinnLand$year=factor(MinnLand$year)
m4=lm(log(acrePrice)~year+region+year:region,data=MinnLand)
Anova(m4)

plot(
  allEffects(m4, transformation=list(link=log, inverse=exp)),
  grid=TRUE, rotx=45, ylab="Acre Price", rug=FALSE)

plot(allEffects(m4, transformation=list(link=log, inverse=exp)),
     grid=TRUE, multiline=TRUE, ylab="Acre Price", rug=FALSE)

## Example UN Data
    
m.un = lm(lifeExpF~group+log(ppgdp)+log(ppgdp):group,data=UN11)
summary(m.un)
Anova(m.un)

# 1. Determine which regressors should be included in the model. Explain.
# 2. Test the if there is a significant difference between groupother and groupafrica
# 3. Test if the difference between main effect other and africa is 10 and
# the difference of the slope of regression line is -0.3


#To test difference in life expectancy between groups `other` and `africa`

ht1 = linearHypothesis(m.un, c(0, 1, -1, 0, 0 ,0))
ht1

# Possible alternatives
linearHypothesis(m.un, "groupother=groupafrica")
linearHypothesis(m.un, "groupother-groupafrica")
linearHypothesis(m.un, "1*groupother - 1*groupafrica = 0")

    
L = matrix(c(0, 1, -1, 0, 0, 0,
             0, 0, 0, 0, 1, -1), 
           byrow=TRUE, nrow=2)
ht2 = linearHypothesis(m.un, L)
ht2

c.vector = c(10, -0.3)
ht3 = linearHypothesis(m.un, hypothesis.matrix = L, rhs = c.vector)
ht3
