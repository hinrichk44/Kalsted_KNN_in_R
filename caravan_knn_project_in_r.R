#For this project we will be working with K-Nearest Neighbors 

#We do not have a csv file for this project
#Instead we will be using the Caravan dataset that is part of the ISLR package
#Let's install the ISLR package first
#install.packages('ISLR')
#Then we will import the ISLR library
library(ISLR)

#Here we will create the Caravan dataframe
#There are 5822 observations of 86 variables
caravans = Caravan


#Here's a quick look at the dataset
#So it looks like the dependent variable will be Purchase (Y/N)
#The 5822 observations are 5822 customers
head(caravans)


#Here is a different way to look at the dataset
str(caravans)

#Here's a quick summary of the results of the Purchase column
#Overwhelmingly 5474 customers did NOT purchase a vehicle. Just 348 customers did purchase a vehicle
summary(caravans$Purchase)


#Here we're doing a quick check if there are any null values in the dataset
#The outcome is FALSE, so there are none. Clean dataset in terms of missing values
any(is.na(caravans))



#Here we will import the dplyr library to help us remove some unwanted columns
library(dplyr)

#Here are the columns we will remove
#They are basically all zeros, they don't provide much value
col_remove = c("PZEILPL", "PVRAAUT", "AVRAAUT", "AZEILPL")
#Here we are doing the actual removal of the columns
caravans = caravans %>%
  select(-one_of(col_remove))


#Here we are splitting the dataset into the Training set and Test set
#First we import the caTools library that allows us to split the data
library(caTools)

#We are setting the seed so we get consistent results
set.seed(123)

#Officially splitting the data. 80% split
split = sample.split(caravans$Purchase, SplitRatio = 0.80)
training_set = subset(caravans, split == TRUE)
test_set = subset(caravans, split == FALSE)


#Scaling matters A LOT in KNN Classification
#Scaling allows KNN classifiers to truly measure the distance variables have to the dependent variable
#Here we are applying Feature Scaling to our split dataset
#We are selecting all of the columns except the last one, hence -86
training_set[-82] = scale(training_set[-82])
test_set[-82] = scale(test_set[-82])


#Here we are fitting K-NN to the Training set and predicting the Test set results
#First we import the class library
library(class)

#we will use the knn function to create our predictions
#We are once again selecting all columns except the last one, hence -82
#cl argument is "factor of true classifications or training set" This is where we include the DEPENDENT variable
#k argument is the number of neighbors 
y_pred = knn(train = training_set[, -82],
             test = test_set[, -82],
             cl = training_set[, 82],
             k = 5,
             prob = TRUE)


#Here we are checking for the missclassification error
#The error is low at .06
mean(test_set$Purchase != y_pred)


#Although the Elbow Method is more attributable to K-Means Clustering, we can use it with KNN to find the optimal amount of neighbors
#We will set up a for-loop to help use find the optimal amount of neighbors
predicted.purchase <- NULL
error.rate <- NULL
#We will set a range of neighbors from 1 to 20
for (i in 1:20){
  set.seed(101)
  predicted.purchase <- knn(training_set[, -82], test_set[, -82], training_set$Purchase, k = i)
  error.rate[i] <- mean(test_set$Purchase != predicted.purchase)
}
#Here we are printing the error rates from each k-neighbors
#It looks like k = 9 is the optimal amount
print(error.rate)

#Let's plot the elbow just as an extra look at the amount of neighbors
#First we will import the ggplot2 library
library(ggplot2)
#We will make a simple dataframe with neighbor values from 1-20 and the error rates associated with them
k.values <- 1:20
error.df <- data.frame(error.rate, k.values)

#Now let's actually plot the dataframe
#It does confirm that the optimal amount of neighbors is 9
ggplot(error.df, aes(k.values, error.rate)) + geom_point() + geom_line(lty = 'dotted', color = 'red')
