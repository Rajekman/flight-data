#Change 'delayed' feature from an integer to a factor
flights_with_dist$delayed <- factor(flights_with_dist$delayed)

#Remove 'flight number' & ' cancelled' features as they seem irrelevant to the decision tree analysis
flight_tree <- flights_with_dist[-9]
flight_tree <- flight_tree[-15]

#Check proportion of delayed flights on the full data
#Roughly 23% delayed
prop.table(table(flight_tree$delayed))

#Since the data is ordered by month, we created a random sample for our training and test data
set.seed(1122)
flight_sample <- sample(1311826, 1180644)

#Split the data into training and test data sets
flight_train <- flight_tree[flight_sample, ]
flight_test  <- flight_tree[-flight_sample, ]

#Double check the proportion of delayed flights
prop.table(table(flight_train$delayed))
prop.table(table(flight_test$delayed))

#Building our first decision tree

library(C50)
flight_model <- C5.0(flight_train[-16], flight_train$delayed)

#Display simple facts about the tree
flight_model

#Display detailed information about the tree
summary(flight_model)

#Evaluating model performance on our test data
#Create a factor vector of predictions on test data
flight_pred <- predict(flight_model, flight_test)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(flight_test$delayed, flight_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual delayed', 'predicted delayed'))


