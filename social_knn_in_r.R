#For this project, we will be focusing on K-Nearest Neighbors or K-NN in R
#We are going to use K-NN CLASSIFICATION to predict the effectiveness of social network ads


#Here we are importing the dataset and selecting the relevant columns for analysis
social = read.csv('Social_Network_Ads.csv')
social = social[3:5]

#Here we are encoding the target feature as factor
#We are doing this because of classification. Target variable must be 1-0
social$Purchased = factor(social$Purchased, levels = c(0, 1))

#Here we are splitting the dataset into the Training set and Test set
#First we import the caTools library that allows us to split the data
library(caTools)

#We are setting the seed so we get consistent results
set.seed(123)

#Officially splitting the data. 75% split
split = sample.split(social$Purchased, SplitRatio = 0.75)
training_set = subset(social, split == TRUE)
test_set = subset(social, split == FALSE)

#Here we are applying Feature Scaling to our split dataset
#We are selecting all of the columns except the last one, hence -3
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

#Here we are fitting K-NN to the Training set and predicting the Test set results
#First we import the class library
library(class)

#we will use the knn function to create our predictions
#We are once again selecting all columns except the last one, hence -3
#cl argument is "factor of true classifications or training set" This is where we include the DEPENDENT variable
#k argument is the number of neighbors 
y_pred = knn(train = training_set[, -3],
             test = test_set[, -3],
             cl = training_set[, 3],
             k = 5,
             prob = TRUE)



#Here we are making the Confusion Matrix
cm = table(test_set[, 3], y_pred)







#Here we are visualizing the Training set results
#We are creating prediction boundaries and then the actual observed data on top
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))






#Here we are visualizing the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))