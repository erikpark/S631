# S631 12-05-17

require(alr4)
n <- dim(UN11)[1]; levs <- pf(99*c(.01, .03, .05, .07), 2, n)
with(UN11, dataEllipse(pctUrban, log(ppgdp),
                       levels=levs-1/n, xlim=c(-20, 130), ylim=c(3, 14),id.n=4))
# with(UN11, identify(pctUrban, log(ppgdp), rownames(UN11)))
m1=lm(lifeExpF~log(ppgdp)+pctUrban,UN11)
hii=hatvalues(m1)
hii[c(102,125,165,182)]
hii[hii>.035]


## Example: caution data
m2=lm(y~x1+x2,caution)
e_hat=residuals(m2,"pearson"); y_hat=fitted(m2)
plot(y_hat,e_hat)
plot(caution)


## Fuel consumption Data

fuel2001 <- transform(fuel2001,
                      Dlic=1000 * Drivers/Pop,
                      Fuel=1000 * FuelC/Pop,
                      Income = Income/1000)
fuel1 <- lm(formula = Fuel ~ Tax + Dlic + Income + log(Miles),
            data = fuel2001)
rp=residualPlots(fuel1, id.n=2)
hii=hatvalues(fuel1)
hii[hii>.3]

## Testing For Curvature

# Suppose a plot of $\hat e$ against $U$

rp
2*(pt(-1.446,dim(fuel2001)[1]-2))
2*(pnorm(-1.446))

yhat=fitted(fuel1)
summary(lm(Fuel ~ Tax + Dlic + Income + log(Miles)+I(yhat^2),fuel2001))

m3= lm(fertility ~ log(ppgdp)+pctUrban,UN11)
rp3=residualPlots(m3)

m4= lm(fertility ~ log(ppgdp)+pctUrban + log(ppgdp):pctUrban,UN11)
rp4=residualPlots(m4)

rp3
rp4

## Non-constant Variance


## Outlier Test and Significance Levels

#The Bonferroni inequality is used for multiple testing

m2 <- lm(fertility ~ log(ppgdp) + pctUrban, UN11)
outlierTest(m2)


## Example: Forbes

mf <- lm(pres ~ bp, data=Forbes)
plot(pres ~ bp, data=Forbes, xlab="Boiling Point (deg. F)",
     ylab="Pressure (in Hg)")

outlierTest(mf)


## Influence Analysis

m1 <- lm(log(fertility) ~ log(ppgdp) + lifeExpF, UN11)
betahat.not.i <- influence(m1)$coefficients
panel.fun <- function(x, y, ...){
  points(x, y, ...)
  dataEllipse(x, y, plot.points=FALSE, levels=c(.90))
  showLabels(x, y, labels=rownames(UN11),
             id.method="mahal", id.n=4)}

pairs(betahat.not.i, panel=panel.fun)
summary(m1)


## Cook's Distance

cd1=cooks.distance(m1)
cd1[1:10]

influenceIndexPlot(m1)

## Normality Assumption


qqPlot(m2, id.n=3)
shapiro.test(rstandard(m2))
