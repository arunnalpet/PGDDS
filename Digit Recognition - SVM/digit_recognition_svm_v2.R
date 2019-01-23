# Assignment: Digit Recognition using SVM
# Objective: Develop SVM classification model to identify the digits based on pixel values.

# Load required libraries
library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(caTools)

# Source the data. Please set this according to your data set location.
setwd('C:/Project/IIITB/4. Predictive Analysis II/Module 3 - Digit Recognition SVM Assignment')

# test and train data doesnt have headers, let R generate one instead.
# Test data is given seperately. So using train data for model building and cross validation.
digit_data <- read.csv('mnist_train.csv', header = FALSE)
test_data <- read.csv('mnist_test.csv', header = FALSE)

#####################
# Understand the data
#####################

dim(digit_data)
# It has 60k rows and 785 observations!

str(digit_data)
# All are integers.
# The first column v1 is the dependent variable
# All the columns hold pixel data. Plotting graphs for such data, doesnt yield anything useful.

# Check for missing values
sapply(digit_data, function(x) sum(is.na(x)))
sum(is.na(digit_data))
# There are no NAs, so the data is clean

# Remove the columns that has only zeros.
sapply(digit_data, function(x) sum(x))
# Check how many columns have zeros.
emptyCols = colnames(digit_data)[colSums(digit_data)==0]
length(emptyCols)
# 67 rows have only 0s.

# Check the same for test data.
emptyCols = colnames(test_data)[colSums(test_data)==0]
length(emptyCols)
# 116 rows have only 0s.

# Since both train and test data have different set of columns with 0s, will leave it as it is.
# Will not remove empty columns.
# digit_data <- digit_data[,-which(names(digit_data) %in% emptyCols)]
# test_data <- test_data[,-which(names(test_data) %in% emptyCols)]

##################
# Prepare the data
##################

# Convert the target class to factor
digit_data$V1 <- factor(digit_data$V1)
test_data$V1 <- factor(test_data$V1)

# Check how many samples we have for each digit.
digit_group <- group_by(digit_data,V1)
count(digit_group)

# This is large data set. Consider 15% of train data..
# If 15% is larger and taking time, reduce this further and then proceed.
set.seed(100)
indices = sample(1:nrow(digit_data), 0.15*nrow(digit_data))
train = digit_data[indices,]

# Verifying the count of random samples, make sure we have captured enough samples of each digit.
count(group_by(train,V1))

################
# Model Building
################

######Linear Model######  
# Test out the simple linear model first.
# Scaling is not required here, as all the columns represent pixel data.
Model_linear <- ksvm(V1~ ., data = train, scale = FALSE, kernel = "vanilladot")

# Evaluate model against the seperate test data.
Eval_linear<- predict(Model_linear, test_data)

# Check the accuracy
confusionMatrix(Eval_linear,test_data$V1)
# We get accuracy of about 91%
# Also notice that 9&4, 9&7, 8&3 are the major ones that are not classified properly.

###### RBF Kernel ###### 
Model_RBF <- ksvm(V1~ ., data = train, scale = FALSE, kernel = "rbfdot")
# Check out the RBF model built.
Model_RBF
# It has very very low sigma value. 
# sigma = 1.64178908877282e-07
# C = 1
# That means, kernel is leveraging small amount of non-linearity 

Eval_RBF<- predict(Model_RBF, test_data)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test_data$V1)
# Accuracy has improved to 95%
# Classification has improved significantly over linear model.
# We can just stick to linear model, or now vary the c parameter to get better accuracy.


####### Hyperparameter and cross-validation ########

# I am running this on i7 processor and took about 40 minutes for this to finish. 
# If it is very slow, reduce the number of folds here and proceed.
# enable verbose option, we can see what is going on currently.
trainControl <- trainControl(method="cv", number=5, verboseIter = TRUE)

# Define the evaluation metric
metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)

# Commenting the below two hyperparameter variation trials, as it consumes lot of time to test all of these.
# grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2) )
# This yielded very low accuracy, hence not considering it.

# grid <- expand.grid(.sigma=c(0,0.0000001,0.000001), .C=c(0.1,0.5,1,2) )
# Getting about 92% accuracy, can be made better.

# Matching the sigma here with the one we found in RBF evaluation.
grid <- expand.grid(.sigma=c(1.6e-07,1.64e-07,1e-07), .C=c(1:4) )

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(V1~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)
# Selecting tuning parameters
# Fitting sigma = 1.64e-07, C = 4 on full training set
# Got accuracy of 96.31% with these tuned hyperparameters.

print(fit.svm)

plot(fit.svm)


# Use the best of the cross-validated tuning parameters and try it on test data.
Eval_CV<- predict(fit.svm, test_data)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_CV,test_data$V1)
# Accuracy of 96.31% over the test data.

############
# Conclusion
############

# Step1: SVM model was built with simple linear kernel, yielded about 91% Accuracy.
# Step2: SVM model was built with RBF kernel, with default tuning parameters. yielded about 95% Accuracy.
# Step3: Since RBF yielded better results, performed Cross Validation using RBF by tuning hyperparameters.
    # Test Run 1:
        # Tuning Parameters: CV=2, sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2)
        # Results: Very low Accuracy.
        # Interpretation: Model is not good. Need to tune hyperparameters.
    # Test Run 2:
        # Tuning Parameters: CV=5, sigma=c(0,0.0000001,0.000001), .C=c(0.1,0.5,1,2)
        # Results: Accuracy of 92%
        # Interpretation: Model is doing good prediction after significantly reducing the value of sigma.
                          # Need to tune more to get better Accuracy.
    # Test Run 3:
        # Tuning Parameters: .sigma=c(1.6e-07,1.64e-07,1e-07), .C=c(1:4)
        # Results: Good accuracy with 96.31%
        # Interpretation: final tuned parameters are: sigma = 1.64e-07, C = 4
        # Ran the model against the test data and got 96.31% Accuracy, which is good. Hence stopping further tuning.

# 'fit.svm' is the final model.
