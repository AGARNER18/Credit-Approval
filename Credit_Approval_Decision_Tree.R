# Amber Garner
# Nov. 2, 2016
# Decision Tree on credit approval data

#*********Setup*************************************** 

install.packages("party")
library(party)

# Load credit approval dataset
credit <- read.csv("CreditApproval.csv", header = T, sep = ",")

#**********DATA PREPROCESSING****

# structure and summary of data
summary(credit)
str(credit)

# remove missing values
credit <- na.omit(credit)

# verify that missing values are gone
summary(credit)

#*********MODEL**************

# divide into training (70%) and test (30%) sets
set.seed(1234)
ind <- sample(2, nrow(credit), replace = TRUE, prob = c(0.7, 0.3))
train.data <- credit[ind == 1, ]
test.data <- credit[ind == 2, ]

# build decision tree with ctree
myFormula <- class~.
model <- ctree(myFormula, data=train.data)

# print credit decision tree model
print(model)

# view nodes starting at the 2nd node
nodes(model, 2)

# plot decision tree model
plot(model)

# plot simple decision tree model
plot(model, type="simple")


#********ACCURACY****************

# build confusion matrix on training data
table(predict(model), train.data$class)

# Calcualte classification accuracy
train_accuracy <-(201+194)/(nrow(train.data))
train_accuracy 

# Calculate classification error rate
train_error <- (55+12)/(nrow(train.data))
train_error

# table of probabilities
prop.table(table(predict(model), train.data$class))

# confusion matrix on test data
testPred <- predict(model, newdata = test.data)
table (testPred, test.data$class)

# Calcualte classification accuracy
test_accuracy <-(94+86)/(nrow(test.data))
test_accuracy 

# Calculate classification error rate
test_error <- (17+7)/(nrow(test.data))
test_error

#*******THE END**************
