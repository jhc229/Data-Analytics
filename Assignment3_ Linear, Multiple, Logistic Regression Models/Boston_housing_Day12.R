library(MASS)
help("Boston")
data.h <- Boston

#let's explore a bit
str(data.h)
summary(data.h)
x <- data.h[,1:13]
y <- data.h[,14]

# linearity: scatterplots and correlations
par(mfrow=c(1,3))
for (j in 1:3){
  plot(x[,j],y,xlab=names(x)[j],ylab="median home price",
       main="x-y scatterplot")
}

# log transform
x.t <- x
x.t[,1] <- log(x[,1])

par(mfrow=c(1,3))
for (j in 4:6){
  plot(x.t[,j],y,xlab=names(x)[j],ylab="median home price",
       main="x-y scatterplot")
}

a <- cor(x.t,y)
round(a,2)

# simple lin reg model
fit1 <- lm(y~x.t$lstat)
fit1
summary(fit1)

fit2 <- lm( y ~ x.t$crim )
fit2
summary(fit2)

fit3 <- lm( y ~ x.t$lstat + x.t$crim + x.t$rm )
fit3
summary(fit3)

cor( x.t$crim, x.t$lstat )

##### continuing from Day 11 #####
anova(fit1, fit3) #fit1 or fit3?
anova(fit2, fit3) #fit2 or fit3?

##### interactions #####
fit3.5 <- lm( y ~ x.t$lstat*x.t$rm)
fit3.5
summary(fit3.5)

#let us put all features in the model
data.ht <- cbind(y, x.t)
fit4 <- lm( y ~ . , data.ht) # notice the syntax
summary(fit4)

# prediction using model 4
new.df <- data.frame(colMeans(x.t))
predict(fit4, new.df)


# model selection
fit0 <- lm(y~1,data = data.ht)
foo <- stepAIC(fit0, direction = "both",scope=list(upper=fit4,lower=fit0))
foo$anova
formula(foo)

fit5 <- lm(formula(foo), data=data.ht)
fit5
summary(fit5)

##### diagnostics #####

# 0. x-y plots (done earlier)

# 1. Are residuals normally distributed?

res <- fit5$residuals
par(mfrow=c(1,1))
qqnorm(res) #quantile-quantile plot
qqline(res) #adds a line going through first and third quartile

# 2. Are residuals unrelated to x?

par(mfrow=c(2,2))
plot(x.t$lstat, res); abline(0,0)
plot(x.t$rm, res); abline(0,0)
plot(x.t$ptratio, res); abline(0,0)
plot(x.t$crim, res); abline(0,0)

# 3. Are residuals unrelated to fitted y values?

par(mfrow=c(1,1))
plot(predict(fit5), res); abline(0,0)

# 4. Leverage (influential points)
lev <- hat(model.matrix(fit5))
plot(lev)

# 5. Cook's distance (influential points)
cook <- cooks.distance(fit5)
plot(cook)

# 6. Plot of residuals against Leverage (influential points)
lev <- hat(model.matrix(fit5))
plot(lev, res)
