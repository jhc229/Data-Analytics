
#------------------------------------------------------------------------------------------------------------------------
#Problem 3: Iris data

#Getting the data from the online resource
url = "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
data_iris <- read.csv(url, sep=",", stringsAsFactors=FALSE)

#Setting up the data frame
data.iris = data.frame(data_iris)
dim(data.iris)
str(data.iris)
matrix.iris=data.matrix(data_iris)

#Setting the column names of the Reg. Summary matrix
names(data.iris)[1] = "Sepal length"
names(data.iris)[2] = "Sepal width"
names(data.iris)[3] = "Petal length"
names(data.iris)[4] = "Petal width"
names(data.iris)[5] = "Class"

#Sets up the for loop to fill the matrix
Reg.names = unique(data.iris$Class)
Reg.summary = matrix(NA,nrow=length(Reg.names),ncol=9)
for (i in 1:length(Reg.names)){
  #Sets up the subset of data
  foo = subset(data.iris,Class==Reg.names[i],select=c("Sepal length","Sepal width","Petal length","Petal width"))
  
  #Calculates the mean and standard deviations 
  Reg.summary[i,1] = nrow(foo)
  Reg.summary[i,2] = mean(foo[,1],na.rm=TRUE)
  Reg.summary[i,3] = sd(foo[,1], na.rm = TRUE)
  Reg.summary[i,4] = mean(foo[,2],na.rm=TRUE)
  Reg.summary[i,5] = sd(foo[,2], na.rm = TRUE)
  Reg.summary[i,6] = mean(foo[,3],na.rm=TRUE)
  Reg.summary[i,7] = sd(foo[,3], na.rm = TRUE)
  Reg.summary[i,8] = mean(foo[,4],na.rm=TRUE)
  Reg.summary[i,9] = sd(foo[,4], na.rm = TRUE)
}

rownames(Reg.summary) = Reg.names
colnames(Reg.summary) = c("Occurences","Mean Sepal length","SD Sepal length","Mean Sepal width","SD Sepal width","Mean Petal length","SD Petal length","Mean Petal width","SD Petal width")

Reg.summary = round(Reg.summary, digits=2)

save(data.iris,Reg.summary,file="iris.RData")

#------------------------------------------------------------------------------------------------------------------------
# (20 points) Consider the Iris data set from assignment 1 problem 3. Construct the
# following plots in R.
plot(data.iris$`Petal length`, data.iris$`Petal width`, type="p", main = "petal length vs width", xlab="length", ylab="width", pch =21, col=c("red","green3","blue")[unclass(iris$Species)])
plot(data.iris$`Sepal length`, data.iris$`Sepal width`, type="p", main = "Sepal length vs width", xlab="length", ylab="width", pch =21, col=c("red","green3","blue")[unclass(iris$Species)])

pairs(data.iris[1:4], main = "Iris Data", pch = 21, bg = c("red", "green", "blue")[unclass(iris$Species)])


#------------------------------------------------------------------------------------------------------------------------
# 2. (20 points) Consider the babynames data from assignment 1 problems 4,5
# (a) Create a subset of the data with female babies named "Mary" from 1880-2014.
# install.packages("babynames")
library("babynames")

#Setting up the data frame
data.baby=data.frame(babynames)

mary = subset(data.baby, data.baby$sex=="F" & data.baby$year>=1880 & data.baby$year<=2014 & data.baby$name=="Mary")

# (b) Create a subset of the data with female babies named "Sophia" from 1880 2014.

sophia = subset(data.baby, data.baby$sex=="F" & data.baby$year>=1880 & data.baby$year<=2014 & data.baby$name=="Sophia")

# (c) Construct a plot of the proportion of female babies named "Mary" from 1880 2014. On
# the same plot, add/overlay a plot of the proportion of female babies named "Sophia" from
# 1880 2014.
plot(x=1880:2014,y=mary$prop,col="red",ylab="Prop",xlab="Year")
points(x=1880:2014,y=sophia$prop,col="blue")
legend("topright", inset=0.05, title="Number of participants", c("Sophia","Mary"), fill=c("red","blue"))

#------------------------------------------------------------------------------------------------------------------------
# 3. Run the following code segments in R, describe what is wrong and how to correct it.
# Report only the R code with correction, do not report output.

# (a) (10 points)
# a = rnorm(100)
# b = sqrt(a)
# print(b)
# ANSWER: b=sqrt(as.complex(a)). Since sqrt does not return a complex number when we take the sqrt of negative numbers from a.
# Then it would produce a wanring message saying NaNs produced.

# (b) (10 points)
# a = c("1","2","3","4","5")
# b = mean(a)
# print(b)

# ANSWER:  b = mean(as.numeric(a)). You can not apply mathematical functions on string variables.

#------------------------------------------------------------------------------------------------------------------------
# 4. (20 points) Load the anscombe dataset in R. (Hint: data.anscombe = anscombe)
# (a) Fit linear regression of (i) y1 on x1 (ii) y2 on x2 (iii) y3 on x3 and (iv) y4 on x4. Write
# down the four fitted regression lines.

data.anscombe = anscombe
fit1 = lm(data.anscombe$y1~ data.anscombe$x1) # lm( Y ~ X1 + X2 X3)
fit2 = lm(y2~x2, data=data.anscombe)
fit3 = lm(y3~x3, data=data.anscombe)
fit4 = lm(y4~x4, data=data.anscombe)
plot(y1~x1, data=anscombe)
points(y2~x2, data=anscombe)
points(y3~x3, data=anscombe)
points(y4~x4, data=anscombe)
par(mfrow=c(2,2))
for (j in 1:4){
  # plot(x[,j],y,xlab=names(data.h)[j],ylab="median value",main="x-y scatterplot")
  plot(data.anscombe[,j], data.anscombe[,4+j] ,main="x-y scatterplot")
  fit = lm(data.anscombe[,4+j]~ data.anscombe[,j], data=data.anscombe)
  abline(fit,col="blue")
}
round(cor(data.anscombe[,1],data.anscombe[,5]),2)

# (c) Use your judgement and describe the discrepancy between the plots and the regression lines
