#------------------------------------------------------------------------------------------------------------------------
#Problem 2: Linear regression
# (a) Compute the correlation coefficient between mpg and all other features in the dataset.
# What are the two features most strongly correlated with mpg ?
# (Hint: a strong correlation can be either positive or negative, use abs(x) to obtain the
#  absolute value of a number x.)
library(MASS)
mtcars

corr.cyl= cor(mtcars$mpg,mtcars$cyl)
corr.disp= cor(mtcars$mpg, mtcars$disp)
corr.hp= cor(mtcars$mpg, mtcars$hp)
corr.drat= cor(mtcars$mpg, mtcars$drat)
corr.wt= cor(mtcars$mpg, mtcars$wt)
corr.qsec= cor(mtcars$mpg, mtcars$qsec)
corr.vs= cor(mtcars$mpg, mtcars$vs)
corr.am= cor(mtcars$mpg, mtcars$am)
corr.gear= cor(mtcars$mpg, mtcars$gear)
corr.carb= cor(mtcars$mpg, mtcars$carb)
print(corr.cyl)
print(corr.disp)
print(corr.hp)
print(corr.drat)
print(corr.wt)
print(corr.qsec)
print(corr.vs)
print(corr.am)
print(corr.gear)
print(corr.carb)
sort(abs(cor(mtcars)[1,]), decreasing=TRUE)[2:11] #Ignore the correlation between the mpg to mpg.

summary(aov(mpg~. , data = mtcars))

#------------------------------------------------------------------------------------------------------------------------
# (b) Fit two simple linear regression models: model 1 using the strongest feature from (a) and
# model 2 using the second strongest feature from (a). Report the linear regression
# formula (i.e., report the line equation) and the value of R^2 from the two models. If you
# had to choose between these two models, which one would you choose and why?


model1= lm(mpg~wt, data=mtcars)
model2= lm(mpg~cyl, data=mtcars)
summary(model1)
summary(model2)
# The regression equation for model 1 is mpg = 37.2851 -5.3445*wt. The value of R^2 is:
# Multiple R-squared:  0.7528,	Adjusted R-squared:  0.7446
# The regression equation for model 2 is mpg = 37.8846 -2.8758*cyl. The value of R^2 is:
# Multiple R-squared:  0.7262,	Adjusted R-squared:  0.7171


par(mfrow=c(1,2))
# plot(model2)
plot(mpg~wt,mtcars)
abline(model1,col="blue")
plot(mpg~cyl, mtcars)
abline(model2,col="red")

par(mfrow=c(2,2))
plot(model1)
plot(model2)


m1 = lm(mpg~cyl, data=mtcars)
summary(m1)
m2 = lm(mpg~wt, data=mtcars)
summary(m2)
anova(m1, m2)

#------------------------------------------------------------------------------------------------------------------------
  # (c) Fit a multiple linear regression model with all features. Which features are significant in
  # this model? What is the value of R^2 in this model?
par(mfrow=c(2,2))
mModel=lm(mpg~., data=mtcars)
plot(mModel)
summary(mModel)
anova(model1, model2, mModel)

#------------------------------------------------------------------------------------------------------------------------
# (d) Using stepAIC , identify the best subset of features. Fit a multiple linear regression model
# using the best subset of features. Write down the regression formula and R^2 for this
# model. Are any of the features from (a) included in this model? Do they have the same
# coefficients as they had in model 1 or model 2 from (b)? If the coefficient values have
# changed, explain why.
library("MASS")
fit1 <- lm(mpg~1,data=mtcars)
foo1 <- stepAIC(fit1,direction="both",
               scope=list(upper=mModel,lower=fit1))

# mpg = 38.75179 -3.16697(wt) -0.94162(cyl) -0.01804(hp)
# Multiple R-squared:  0.8431,	Adjusted R-squared:  0.8263
# wt and wt + cyl are included as part of consideration, but it shows that the weight to be the sigificant value out of all.
# The coefficient value will be different since we know that our models contain outliers and values that show significant changes on the variants.
# So, inorder to create a best fit model, stepwise will search between the scope arguments and outputs the best model according to AIC.

f0 = lm(mpg~1, data=mtcars)
summary(f0)
foo = stepAIC(f0, direction = "both", scope=list(upper = f3, lower = f0))
foo$anova
formula(foo)
summary(foo)


#------------------------------------------------------------------------------------------------------------------------
# 3.
# (b) Construct a plot of hp (xaxis) and wt (yaxis),with different colors for automatic and
# manual transmission. From the plot, do you think automatic and manual transmission
# can be distinguished by weight and horsepower?
# am Transmission (0 = automatic, 1 = manual)
par(mfrow=c(1,1))
plot(wt~hp, data = mtcars, col = ifelse(mtcars$am==0,"blue","red"), pch=16 ,main = "wt vs hp")
legend("topright", inset=0.01, title="Transmission", c("Automatic","Manual"), fill=c("blue","red"))

# (c) Fit a logistic regression model with wt as the only feature. Using this model, explain
# whether heavier cars are more likely or less likely to have manual transmission.
# If weight increases by 1000 lbs, what is the change in odds of a car having manual transmission?

# am Transmission (0 = automatic(blue), 1 = manual(red))
# plot(am~wt, data = mtcars, col = ifelse(mtcars$am==0,"blue","red"), pch=16 ,main = "am vs hp")
attach(mtcars)
logis = glm(am~wt, family = "binomial")
summary(logis)
par(mfrow=c(2,2))
plot(logis, col = ifelse(mtcars$am==0,"blue","red"), pch=16)
# legend("topright", inset=0.05, title="Transmission", c("Automatic","Manual"), fill=c("blue","red"))
exp(predict(logis, data.frame(wt=2), type="link") - predict(logis, data.frame(wt=1), type="link") )
# Blue indicates automatic cars and red indicates manunal cars. Heavier cars are LESS likely to have manual transmission according to the logis plots.
# Both automatic and manual tranmission cars have few outliers which seem to create larger standard deviation for the Scale-Location plot. But overall,
# given data sets are fairly distinct in terms of the ratio between the wt and the transmission. When the weight is increased by 1000lbs,
# the odds that an increased 1000lbs is approximately 0.1788183 times as likely to have manual transmission as an automatic transmission.

# (d) Fit a logistic regression model with hp as the only feature. Using this model, explain
# whether cars with higher horsepower are more likely or less likely to have manual transmission. 
# If horsepower increases by 100, what is the change in odds of a car having manual transmission?

# am Transmission (0 = automatic(blue), 1 = manual(red))
# plot(am~hp, data = mtcars, col = ifelse(mtcars$am==0,"blue","red"), pch=16 ,main = "am vs hp")
logis2 = glm(am~hp, family = "binomial")
summary(logis2)
par(mfrow=c(2,2))
plot(logis2, col = ifelse(mtcars$am==0,"blue","red"), pch=16)
exp(predict(logis2, data.frame(hp=200), type="link") - predict(logis2, data.frame(hp=100), type="link") )
detach(mtcars)

# On average, automatic cars have higher horsepower.
# When the horsepower is increased by 100hp, the odds that an increased 100hp is approximately 0.4440971 times as likely
# to have a manual transmission as an automatic transmission

# (e) If you had to choose between these two models, which one would you choose and why?
# am VS wt or am VS hp

# Compare both models
par(mfrow=c(2,4))
plot(logis, col = ifelse(mtcars$am==0,"blue","red"), pch=16)
plot(logis2, col = ifelse(mtcars$am==0,"blue","red"), pch=16)

