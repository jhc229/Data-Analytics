#------------------------------------------------------------------------------------------------------------------------
# (a) Load the wine dataset from the rattle package in R, and describe the dataset in your own words, in 2-3 lines.
#     Consider Type to be the response variable, and all other variables as features.
require(MASS) # library and require load and attach add-on packages.

install.packages("rattle")
library(rattle)

# The wine datset contains three types of wine in the 178 samples. The three types of wine are labeled in
# numeric 1~3 in column 1(response variable) and rest of the 13 columns(features) represent the wine results
# of chemical analyses. All data will be represented in numeric values.


#------------------------------------------------------------------------------------------------------------------------
# (b) Perform classification using LDA (linear discriminant analysis) and report the
# classification error rate.

data.wine=wine 
col.wine=ifelse(data.wine$Type=="1","red", ifelse(data.wine$Type=="2","blue","green"))
# par(mfrow=c(1,2))
# plot(data.wine$Sepal.Length,data.wine$Sepal.Width,pch=16,col=col.iris) 
# plot(data.wine$Petal.Length,data.wine$Petal.Width,pch=16,col=col.iris)

r1 = lda(formula = Type~., data = wine)
r1
r1$counts

r1.prop = r1$svd^2/sum(r1$svd^2) # same as proportion of trace
plda= predict(r1,wine)
table(wine$Type, plda$class)
mean(wine$Type!=plda$class, CV=TRUE) # classification error rate

lda.CV = lda(Type ~., data = wine.data, CV=TRUE)
mean(wine.data$Type != lda.CV$class) # cv error rate

plot(r1)


# dset = data.frame(type = wine[,"Type"],
#                     r1 = plda$x)
# 
# p1 <- ggplot(dset) + geom_point(aes(r1.LD1, r1.LD2, colour = type, shape = type), size = 2.5) + 
#   labs(x = paste("LD1 (", percent(plda.lda[1]), ")", sep=""),
#        y = paste("LD2 (", percent(plda.lda[2]), ")", sep=""))
# 

#------------------------------------------------------------------------------------------------------------------------
# (c) Perform classification using QDA (quadratic discriminant analysis) and report the
# classification error rate.

r2 = qda(formula =Type~. ,data = wine)
r2
pqda=predict(r2,wine)
table(wine$Type, pqda$class)
mean(wine$Type!=pqda$class, CV=TRUE) # classification error rate

qda.CV = qda(Type ~ .,data=wine.data, CV=TRUE)
mean(wine.data$Type != qda.CV$class) # cv error rate

plot(r2)

#------------------------------------------------------------------------------------------------------------------------
# (d) Perform classification using SVM (support vector machines) and report the classification
# error rate.

install.packages("e1071")
library(e1071)
r3 = svm(Type~., data = data.wine)
r3
psvm = predict(r3, data = data.wine)
psvm
mean(data.wine$Type!=psvm) # classification error rate

svm.modelCV = svm(Type ~., data=wine.data, CV = TRUE)
svm.predCV = predict(svm.modelCV, data = wine.data)
mean(wine.data$Type!=svm.predCV) # cv error rate

plot(r3)

#------------------------------------------------------------------------------------------------------------------------
# (e) Rank the classification methods in your order of preference for this dataset, and justify
# your preference.
# install.packages('klaR')
# library(klaR)
# partimat(Type~.,data=data.wine,method="lda")


# SVM is the superior model, as it has both a 0 classification error rate and a 
# 0 cv error rate.
# Checking the CV error rates is a great way to see how well a model predicts and 
# helps give insight into which model may be best.
