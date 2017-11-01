library(alr4)


# 1. The data in the filetwinsgive the IQ scores of identical twins, one raised in 
# a foster home,IQf, and the other raised by birth parents,IQb. The data were published 
# by Burt (1966), and their authenticity has been questioned. For purposes of this 
# groupwork, the twin pairs can be divided into three social classesC, low, middle, or 
# high, coded in the data file 1, 2, and 3, respectively, according to the social class 
# of the birth parents. TreatIQf as the response andIQb as the predictor, with C as a factor.

twins <- twins


# 1. a) Use a scatterplot to explain the relationship of the response with the predictors.

# Using the scatterplot supplied in the pdf of the assignment, we saw that there is a definite
# effect of IQb on IQf (all the lines increase as IQb increases), so basically the regressor IQb seems
# to have a significant effect on the response IQf.  However, the intercepts of the three lines,
# which represent the three levels of the factor C, seem to be quite similar and maybe only slightly different.
# This suggests that the factor regressor, C, may not be very useful in explaining the variation seen
# in the response.  Finally, there definitely seems to be no need for an interaction between the two
# regressors, as the slopes of the different lines are the same - the lines are parallel.  This
# means that there is no change in the effect of the continuous regressor on the response as we move between
# factor levels.

# 1. b) Use the appropriate test to determine the most adequate model

m1 <- lm(IQf ~ IQb + C + IQb:C, data = twins)

summary(m1)

m2 <- lm(IQf ~ IQb + C, data = twins)

m3 <- lm(IQf ~ IQb, data = twins)

# model 1 is the full model, models 2 and 3 are subsequently reduced models.

anova(m2, m1)

# From this first F-test, we see that we aren't able to reject the null hypothesis (that the reduced model with only IQb and C main effects is sufficient), 
# specifically here that means that the coefficient of the interaction term is equal to zero. 
# So, seems like the interaction has no additional effect on explaining the variation seen in the response.

anova(m3,m2)

# Here, we also see that we can't reject the null hypothesis, this time that the regression of IQf ~ IQb is sufficient.
# The addition of C to the model doesn't add any significant explanatory power to the model, so we can't conclude that a model including both 
# IQb and C as regressors is better than one with just IQb.

# Can also do this in one step with Type II Anova, as shown in assignment handout.
Anova(m1)
# Can drop interaction and C from the model as only IQb is significant, when we read from bottom to top.
# Addition of interaction to model containing main effects not significant, and addition of C to model already containing IQb also not significant.


avPlots(m1)

# A way to visualize the importance of the different regressors and interactions. Thanks Sriram!

# 1. c) Interpret the results of the chosen model.

summary(m3)

# So, we see that for every unit increase in IQb, IQf increases by 0.901 units.  So, for every increase of 1 point on the IQ tests of 
# individuals raised by their birth parents, we see an increase of 0.9 points on the IQ tests of individuals raised by foster parents.
# So, kids raised by foster parents have very slighly lower IQ tests, but it isn't a big difference.


# 2. Testing for lack-of-fit (Data file:MinnLand) Refer to the Minnesota farm sales data

farm <- MinnLand

# 2. a) Fit the regression modellog(acrePrice) ~ year via ols, where year is not a factor, but treated as a 
# continuous predictor. What does this model say about the change in price per acre over time? 
# Call this model A.

mA <- lm(log(acrePrice) ~ year, data = farm)

summary(mA)

scatterplot(log(acrePrice) ~ year, data = farm, smooth=FALSE, boxplots=FALSE)

# So with this model with year as a continuous variable, we see that an increase of one year leads to
# an increase in the log transformed response, log(price per acre), by 0.1 units.  This corresponds to
# a (e^beta - 1) * 100 percent change in price per acre, which is a 10.57% increase.
(exp(.1005) - 1) *100

# 2. b) Fit the regression model via log(acrePrice) ~ 1+fyear via ols, where fyear is a factor with as many levels 
# as there are years in the data, including the intercept in the model. What does this model say about 
# the change in price per acre over time?

MinnLand$fyear <- factor(MinnLand$year)

mB <- lm(log(acrePrice) ~ 1 + fyear, MinnLand)

summary(mB)

scatterplot(log(acrePrice) ~ fyear, data = MinnLand, smooth=FALSE, boxplots=FALSE)


# Here this model with year as a factor shows us that there is a general increase in price per acre over time, as we saw in the other model,
# but that the increase is not constant over time.  Some years we see no real increase (2002 to 2003) while some years show larger or smaller increases,
# or even decreases (2010 to 2011).  So this new model gives us some more specific year-to-year information over the 
# year as a continuous predictor model.


# 2. c) Show that model A is a special case of model B, and so a hypothesis test of H0: model A versus HA: model B is reasonable


# So as we discussed in our group, model A is a special reduced case of model B as it provides more generalized, less percise information.
# Model B provides us with a different coefficient for the change in log(price per acre) for each year, while model A provides us with only
# one coefficient: the general, average change in log(price per acre) over time, where the change each year is basically averaged and the same.


# 2. d) A question of interest is whether or not model A provides an adequate description of the change in
# log(acrePrice) over time. The hypothesis test from part c addresses this question, and it can be called a
# lack-of-fit test for model A. Perform the test and summarize results.

anova(mA, mB)


# So in this F-test we can see that we have a p-value near zero, and so we are able to reject the null-hypothesis that the reduced model with year as a continuous variable
# adaquately explains the change seen in the response variable.  So, we need to consider year as a categorical variable when we consider the changes
# seen in log(price per acre) over time.

