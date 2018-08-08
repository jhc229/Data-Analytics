library(MASS)
data.h <- Boston
help("Boston")

# explore a bit
names(data.h)
str(data.h)
summary(data.h)
y <- data.h[,14]
x <- data.h[,1:13]

# linearity of relation: plots and correlation (go through x's in groups of 3)
par(mfrow=c(1,3))
for (j in 1:3){
  plot(x[,j],y,xlab=names(data.h)[j],ylab="median value",main="x-y scatterplot")
  }
round(cor(x,y),2)

# transformation
x.t <- x
x.t[,1] <- log(x[,1])

par(mfrow=c(1,3))
for (j in 1:3){
  plot(x.t[,j],y,xlab=names(data.h)[j],ylab="median value",main="x-y scatterplot")
}
round(cor(x.t,y),2)

# let's sort the features from strongest to weakest correlation with y
a <- cor(x.t,y)
a[order(-abs(a))]
names(x)[order(-abs(a))]

# Let's look at the x-y plots for the top six
par(mfrow=c(2,3))
for (j in order(-abs(a))[1:6]){
  plot(x.t[,j],y,xlab=names(data.h)[j],ylab="median value",main="x-y scatterplot")
}


# 1,2,6 look strong candidates

# simple linear regression
fit1 <- lm( y ~ x.t$lstat )
fit1
summary(fit1) # this gives us results for t-test, R^2, and anova. explain each term
par(mfrow=c(1,1))
plot( x.t$lstat, y )
abline(fit1, col="blue")

# multiple regression: let's use the 3 features 1,2,6
names(x)[order(-abs(a))][c(1,2,6)]
fit2 <- lm( y ~ x.t$lstat + x.t$rm + x.t$crim)
fit2
summary(fit2) # this gives us results for t-test, R^2, and anova
# check the p-value for crim!
round(cor(cbind(x.t$lstat, x.t$rm, x.t$crim )), 2)
crim.fit <- lm( x.t$crim ~ x.t$lstat )
summary(crim.fit)
# crime can be exaplined by lstat, so in a model with lstat crime is no longer uselful
# BUT: in a model without lstat, crime is useful!
fit3 <- lm( y ~ x.t$rm + x.t$crim )
summary(fit3)

# so fit2 or fit3?
anova(fit3,fit2) # answer: fit3 is better

#let us put all features in the model
data.ht <- cbind(y,x.t)
fit4 <- lm( y ~ . , data.ht) # notice the syntax
summary(fit4)

# model selection
fit <- lm( y ~ 1 , data = data.ht)
foo <- stepAIC(fit0,direction = "both",scope=list(upper=fit4,lower=fit0))
foo$anova
formula(foo)

fit5 <- lm( formula(foo) , data=data.ht)
fit5
summary(fit5)
