# Problem 1: Pct. of life at VT
num=(2016-2012)*12+(9-2)
den=(2016-1994)*12+(9-3)
pct=100*(num/den)

#-----------------------------------------------------------------------------------------------------------------------
# Problem 2: 1000th Fibonacci
x=numeric(1000)
y=numeric(999)

x[1]<-1;
x[2]<-1;

y[1]=x[2]/x[1];

for (i in 3:1000){
  x[i]<-x[i-1]+x[i-2]; #Fibonacci Equation
  y[i-1]=x[i]/x[i-1]; # Growth Equation
}

plot(y,xlab="X Interval",ylab='Fibonacci Growth Ratio',pch=16, col='blue')


#------------------------------------------------------------------------------------------------------------------------
#Problem 3: Iris data
# 3. From UC Irvine's machine learning depository, consider the Iris data set at
# http://archive.ics.uci.edu/ml/datasets/Iris.

# (b) Import the data in R, and convert the data set into a data frame. Define column names
# for the data frame using the "Attribute Information" from the above webpage.

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


#---------------------------------------------------------------------------------------------------------------------------------
# 4. Install the R package "babynames". Load the babynames data and answer
# the following questions. Report R code and answers.
install.packages("babynames")
library("babynames")

sub = subset(babynames, n < 6)
sub2 = subset(sub, !duplicated(sub[,3]))
sub2


# Using the %in% notation, I can subset the values of variable year that are
# equal to the given years for each gender.
subYearMale = babynames[babynames$year %in% c(1900, 1925, 1950, 1975, 2000), ] 
subYearFemale = babynames[babynames$year %in% c(2010, 2011, 2012, 2013, 2014), ]

subMale = subset(subYearMale, sex == "M") # Separate the data set into male and female
subFemale = subset(subYearFemale, sex == "F")

# Find the max n for each year then match with the original data to creat a new subset.
popMale = rbind(subMale[subMale$n == max(subMale[subMale$year==1900, ][,4]), ],
                subMale[subMale$n == max(subMale[subMale$year==1925, ][,4]), ],
                subMale[subMale$n == max(subMale[subMale$year==1950, ][,4]), ],
                subMale[subMale$n == max(subMale[subMale$year==1975, ][,4]), ],
                subMale[subMale$n == max(subMale[subMale$year==2000, ][,4]), ])

popFemale = rbind(subFemale[subFemale$n == max(subFemale[subFemale$year==2010, ][,4]), ],
                  subFemale[subFemale$n == max(subFemale[subFemale$year==2011, ][,4]), ],
                  subFemale[subFemale$n == max(subFemale[subFemale$year==2012, ][,4]), ],
                  subFemale[subFemale$n == max(subFemale[subFemale$year==2013, ][,4]), ],
                  subFemale[subFemale$n == max(subFemale[subFemale$year==2014, ][,4]), ])
popMale
popFemale



#--------------------------------------------------------------------------------------------------------------------
# Problem 5

install.packages("babynames")
library("babynames")


popMaleAll = subset(babynames, sex == "M")  # Contains only male applicants
popFemaleAll = subset(babynames, sex == "F") # Contains only female applicants

popMaleAll2 = popMaleAll[order(popMaleAll$n, decreasing = TRUE), c(1:5)] # Sort n in descreasing order
popFemaleAll2 = popFemaleAll[order(popFemaleAll$n, decreasing =TRUE), c(1:5)]

# !duplicated(popMaleAll[,3])
# Remove duplicated names and sort n in order which has to be done
# first so that we have correct output from most popular to least popular names
popMaleAll2 = subset(popMaleAll2, !duplicated(popMaleAll2[,3])) 
popFemaleAll2 = subset(popFemaleAll2, !duplicated(popFemaleAll2[,3]))

popMaleAll2[1:10,]
popFemaleAll2[1:10,]
