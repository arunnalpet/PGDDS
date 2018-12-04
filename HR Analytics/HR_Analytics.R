# Load libraries
library(ggplot2)
library(MASS)
library(caTools)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(ROCR)
library(dplyr)
library(lubridate)

# Set directory
# Please modify this according to your path
setwd("C:/Project/IIITB/3. Predictive Analysis 1/Module5 - CaseStudy/PA-I_Case_Study_HR_Analytics/Remodel-2")

# Load the all Data files
# Assuming files will be present in the current directory when executing this program.
genData <- read.csv("general_data.csv")
empSurveyData <- read.csv("employee_survey_data.csv")
managerSurveyData <- read.csv("manager_survey_data.csv")

# intime & outtime data
intime <- read.csv("in_time.csv")
outtime <- read.csv("out_time.csv")

# Exploratory Analysis
str(genData)              # 4410 Observations
str(empSurveyData)        # 4410 Observations    
str(managerSurveyData)    # 4410 Observations

# Check if employee ID is unique across all three data sets
length(unique(genData$EmployeeID))
length(unique(empSurveyData$EmployeeID))
length(unique(managerSurveyData$EmployeeID))

# Check if all 4410 unique employees are same across data set
identical(sort(genData$EmployeeID),sort(empSurveyData$EmployeeID))
identical(sort(genData$EmployeeID),sort(managerSurveyData$EmployeeID))

# Merging data
empData <- merge(genData, empSurveyData, by = "EmployeeID")
empData <- merge(empData, managerSurveyData, by = "EmployeeID")

########
# Parse intime and outtime to extract average working hours of each employee.
########
intime_cols <- names(intime)
outtime_cols <- names(outtime)

length(intime_cols) #262
length(outtime_cols) #262

#Check if they are identical.
identical(sort(intime_cols),sort(outtime_cols))
# TRUE

# Remove first column, EmployeeID
EmpID <- intime[,1]
intime <- intime[,-1]
outtime <- outtime[,-1]

# Convert everything to date format
intime[]<-lapply(intime,ymd_hms)
outtime[] <- lapply (outtime, ymd_hms)

# Difference between dfs
tdiff <- outtime - intime[colnames(outtime)]
tdiff[] <- lapply(tdiff, as.numeric)

# Bind employeeID
tdiff <- cbind(EmpID,tdiff)

# Find average time spent in office by each employee
avgHours <- data.frame(EmployeeID=tdiff[,1], avgWorkingHours=round(rowMeans(tdiff[,-1], na.rm = TRUE),1))

# Add in-time and out-tme data to master
empData <- merge(empData,avgHours, by="EmployeeID")

# MasterFile
View(empData)


########## 
## Exploratory Data Analysis
### EDA - Categorical Variables 
 plot_grid(ggplot(empData,aes(x=JobRole,fill= factor(Attrition))) + geom_bar(position = "fill"))
## Job role with "Research Director" has higher Attrition
 plot_grid(ggplot(empData,aes(x=MaritalStatus,fill= factor(Attrition))) + geom_bar(position = "fill"))
 ## Marital Status "Single" has higher Attrition
 plot_grid(ggplot(empData,aes(x=BusinessTravel,fill= factor(Attrition))) + geom_bar(position = "fill"))
 ##Travellers who travel frequently has higher Attrition
 plot_grid(ggplot(empData,aes(x=Department,fill= factor(Attrition))) + geom_bar(position = "fill"))
 ##Department  "human resources" has higher Attrition
 plot_grid(ggplot(empData,aes(x=Education,fill= factor(Attrition))) + geom_bar(position = "fill"))
 ##Education Field- HR has left the company more.
 plot_grid(ggplot(empData,aes(x=EducationField,fill= factor(Attrition))) + geom_bar(position = "fill"))
 ##Education Field- HR has left the company more.
 plot_grid(ggplot(empData,aes(x=JobLevel,fill= factor(Attrition))) + geom_bar(position = "fill"))
 ##JobLevel 2 has higher Attrition
 plot_grid(ggplot(empData,aes(x=EnvironmentSatisfaction,fill= factor(Attrition))) + geom_bar(position = "fill"))
 ##Environment Satisfaction attrition decreases with increase in index
 plot_grid(ggplot(empData,aes(x=JobSatisfaction,fill= factor(Attrition))) + geom_bar(position = "fill"))
 ##Job Satisfaction -attrition decreases with increase in index
 plot_grid(ggplot(empData,aes(x=WorkLifeBalance,fill= factor(Attrition))) + geom_bar(position = "fill"))
 ## Work Life Balance Attrition decreases with increase in Count Index
 plot_grid(ggplot(empData,aes(x=JobInvolvement,fill= factor(Attrition))) + geom_bar(position = "fill"))
 ## Job Level 2 has highest Attrition
 plot_grid(ggplot(empData,aes(x=PerformanceRating,fill= factor(Attrition))) + geom_bar(position = "fill"))
 ##Not much noticeable attrition can be measured with Performance Rating

 ### Numerical Variables
  plot_grid(ggplot(empData,aes(Age))+ geom_histogram(binwidth = 10),ggplot(empData,aes(x="Reference", y = Age))+ geom_boxplot(width=0.1))
  plot_grid(ggplot(empData,aes(DistanceFromHome))+ geom_histogram(binwidth = 10),ggplot(empData,aes(x="Refernce", y = DistanceFromHome))+ geom_boxplot(width=0.1))
  plot_grid(ggplot(empData,aes(MonthlyIncome))+ geom_histogram(binwidth = 10),ggplot(empData,aes(x="Reference", y = MonthlyIncome))+ geom_boxplot(width=0.1))
  plot_grid(ggplot(empData,aes(NumCompaniesWorked))+ geom_histogram(binwidth = 10),ggplot(empData,aes(x="Reference", y = NumCompaniesWorked))+ geom_boxplot(width=0.1))
  plot_grid(ggplot(empData,aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),ggplot(empData,aes(x="Reference", y = PercentSalaryHike))+ geom_boxplot(width=0.5))
  plot_grid(ggplot(empData,aes(YearsAtCompany))+ geom_histogram(binwidth = 10),ggplot(empData,aes(x="Reference", y = YearsAtCompany))+ geom_boxplot(width=0.2))
  plot_grid(ggplot(empData,aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),ggplot(empData,aes(x="Reference", y = YearsSinceLastPromotion))+ geom_boxplot(width=0.2))
  plot_grid(ggplot(empData,aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),ggplot(empData,aes(x="Reference", y = TotalWorkingYears))+ geom_boxplot(width=0.2))
##Results
 ####30-45 Years Age Employees are higher in Company
 ####Employees staying near to Company are more in number
 ##### Salary Less than 60K are more in the company
 ####Employees who worked in less than 3 companies are more
 ###Less than 15% hike are more in the company
 ###5 to 15 Total Working years Experience are more in the company

  
 

##########

##########
# DATA PREPARATION
##########

summary(empData)

# We can remove columns which does not vary at all(constant) throughout.
unique(empData$EmployeeCount)
unique(empData$Over18)
unique(empData$StandardHours)
empData <- empData[,-which(names(empData) %in% c("EmployeeCount","Over18","StandardHours"))]

# Missing Value Imputation
# Check NAs
sum(is.na(empData)) # 111 NAs

summary(empData)

# [MissingValue]: GenData
# Total 4410 Observations
# TotalWorkingYears = 9 NAs
# NumCompaniesWorked = 19 NAs
# EnvironmentSatisfaction = 25 NAs
# JobSatisfaction = 20 NAs
# WorkLifeBalance = 38 NAs
# Replacing NAs with Mean numbers for each column. 
# As the missing NAs are very less, its better to replace with mean than deleting the rows.
# Also round off to integer
empData$TotalWorkingYears[is.na(empData$TotalWorkingYears)] <- round(mean(empData$TotalWorkingYears,na.rm = TRUE),0)
empData$NumCompaniesWorked[is.na(empData$NumCompaniesWorked)] <- round(mean(empData$NumCompaniesWorked, na.rm = TRUE),0)

empData$EnvironmentSatisfaction[is.na(empData$EnvironmentSatisfaction)] <- round(mean(empData$EnvironmentSatisfaction, na.rm = TRUE),0)
empData$JobSatisfaction[is.na(empData$JobSatisfaction)] <- round(mean(empData$JobSatisfaction, na.rm = TRUE),0)
empData$WorkLifeBalance[is.na(empData$WorkLifeBalance)] <- round(mean(empData$WorkLifeBalance, na.rm = TRUE),0)

# Check NA again
sum(is.na(empData)) # 0 now

###########
# Normalizing, conversion and dummy variable creation
###########

# Convert dependent variable Attrition to binary
empData$Attrition <- ifelse(empData$Attrition=="Yes",1,0)

# Scale continuous variables
# Age
# Distance from home
# Monthly Income
# NumCompaniesWorked
# Percent Salary Hike
# Total Working Years
# Training times last year
# Years at company
# Years since promotion
# YearsWithCurrentManager
# avgWorkingHours
vars <- c("Age", "DistanceFromHome", "MonthlyIncome",
			"NumCompaniesWorked", "PercentSalaryHike", "TotalWorkingYears",
			"TrainingTimesLastYear", "YearsAtCompany",
			"YearsSinceLastPromotion", "YearsWithCurrManager","avgWorkingHours")
empData[,which(names(empData) %in% vars)] <- data.frame(lapply(empData[,which(names(empData) %in% vars)], function(x) scale(x)))

# Considering Ordinal variables.
# All these variables get better with higher levels, hence considering it as regular integers and scaling them.
# Education:			1-5
# JobLevel: 			1-5
# StockOptionLevel: 	0-3
# EnvironmentSatisfaction: 1-4
# JobSatisfaction		1-4
# WorkLifeBalance		1-4
# JobInvolvement		1-4
# PerformanceRating		1-4
vars <- c("Education","JobLevel","StockOptionLevel",
          "EnvironmentSatisfaction","JobSatisfaction",
          "WorkLifeBalance","JobInvolvement","PerformanceRating")
empData[,which(names(empData) %in% vars)] <- data.frame(lapply(empData[,which(names(empData) %in% vars)], function(x) scale(x)))



# Creating dummy variables for categorical variables
# Categorical Variables LIST
# BusinessTravel:      3-levels
# Department:          3-levels
# Education Field:     5-levels
# Gender:              2-levels
# JobRole:             9-levels
# MaritalStatus        3-levels
vars <- c("BusinessTravel","Department",
          "EducationField", "Gender",
          "JobRole","MaritalStatus")

empData_cat <- empData[,which(names(empData) %in% vars)]

# Remove these columns and replace with corresponding dummies.
empData <- empData[,-which(names(empData) %in% vars)]

# Create Dummies
dummies <- data.frame(sapply(empData_cat, function(x) data.frame(model.matrix(~x-1,data =empData_cat))[,-1]))

# Scale Dummies as well
# dummies <- data.frame(lapply(dummies,function(x) scale(x)))

# Everything is scaled now. Merge the dfs
final_data <- cbind(empData,dummies)

# Remove Employee ID
final_data <- final_data[,-1]

# Check the attrition rate %

attritionRate <- sum(final_data$Attrition)/nrow(final_data)
attritionRate

#############
# Splitting Data to train and test
#############

set.seed(100)
indices = sample.split(final_data$Attrition, SplitRatio = 0.7)

train = final_data[indices,]

test = final_data[!(indices),]

# Check data distribution between test and train
sum(train$Attrition)/nrow(train)  # 0.1613
sum(test$Attrition)/nrow(test)    # 0.1609

###########
# Logistic Regression
###########

# 1:[model]: 
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)

# 2:[model]: Implement stepAIC
# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
sort(vif(model_2))

# BusinessTravel.xTravel_Rarely  & BusinessTravel.xTravel_Frequently have High VIFs.
# Check correlation
cor(train$BusinessTravel.xTravel_Frequently,train$BusinessTravel.xTravel_Rarely)
# 0.75
# Eliminate train$BusinessTravel.xTravel_Rarely which has low p-value comparitively.

# 3:[model]: Remove BusinessTravel.xTravel_Rarely
model_3 <- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + avgWorkingHours + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales + EducationField.xMarketing + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle, 
               family = "binomial", data = train)
summary(model_3)
sort(vif(model_3))
# Examine: Development and Sales department
cor(train$Department.xResearch...Development,train$Department.xSales)
# -0.90
# Very high negative corelation. Remove Department.Sales


# 4:[model]: Remove Department.Sales
model_4 <- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + avgWorkingHours + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 EducationField.xMarketing + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle, 
               family = "binomial", data = train)
summary(model_4)
sort(vif(model_4))
# Examine: Age, TotalWorkingYears & YearsAtCompany
cor(train$TotalWorkingYears, train$Age)             #0.68
cor(train$TotalWorkingYears, train$YearsAtCompany)  #0.61
# Eliminate TotalWorkingYears


# 5:[model]: Remove TotalWorkingYears
model_5 <- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
                 TrainingTimesLastYear + YearsAtCompany + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + avgWorkingHours + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 EducationField.xMarketing + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle, 
               family = "binomial", data = train)
summary(model_5)
sort(vif(model_5))
# Examine: YearsAtCompany & YearsWithCurrManager
cor(train$YearsAtCompany,train$YearsWithCurrManager)  #0.77
# Eliminate: YearsAtCompany as it has higher p-value


# 6:[model]: Remove YearsAtCompany
# [LATER]: Try switching with YearsWithCurrentManager.
model_6 <- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
                 TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + avgWorkingHours + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 EducationField.xMarketing + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle, 
               family = "binomial", data = train)
summary(model_6)
sort(vif(model_6))
# Examine: MaritalStatus Single/Married
cor(train$MaritalStatus.xMarried,train$MaritalStatus.xSingle) # -0.63
# Eliminate "Married" as it has higher p-value


# 7:[model]: Remove "Married"
model_7 <- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
                 TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + avgWorkingHours + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 EducationField.xMarketing + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle, 
               family = "binomial", data = train)
summary(model_7)
sort(vif(model_7))
# All the VIFs are below 2
# So multi collenearity has been handled.
# Start eliminating variables with higher p-values.

# 8:[model]: Remove variable one by one, based on the high p-value in each iteration.
# After multiple such iterations, arrived at the following variables.
# Removing those variables in one step here just for brevity.
# "Scientist", "EducationField Degree", "EducationFieldOther", "Development", "Marketing"
model_8 <- glm(formula = Attrition ~ Age + Education + NumCompaniesWorked + 
                 TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + avgWorkingHours + BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director +
                 JobRole.xSales.Executive + MaritalStatus.xSingle, 
               family = "binomial", data = train)
summary(model_8)
sort(vif(model_8))
# All pvalues are either 2 or 3 stars

# Evaluate once


# 9:[model]: Remove variables with low p-value: 
# Again, after doing repeated iterations, arrived at the following variables.
# "Research Director", "Sales Executive", "Education", "WorkLifeBal"
model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + avgWorkingHours + BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, 
               family = "binomial", data = train)
summary(model_9)
sort(vif(model_9))
# All pvalues are 3 stars now, that means all these variables are highly significant.
# Hence stopping here.

# Evaluate

###########
# Evaluating the model_9
###########


# Check the discrimination power
# Final model
hr_model <- model_9

test_pred = predict(hr_model, type = "response", newdata = test[,-2])
summary(test_pred)
# Min/Max Predicted values
# 0.002364/0.867925

###########
# Choose cutoff
###########

# Choose cutoff of 30% initially.

test_pred_attrition <- factor(ifelse(test_pred >= 0.30, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
test_actual_attrition_bin <- ifelse(test_actual_attrition=="Yes",1,0)
  
table(test_actual_attrition,test_pred_attrition)

###########
# Get details from confusion matrix
###########

test_confusion <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_confusion

# Results
# Accuracy:		0.8382
# Sensitivity:	0.48357
# Specificity:	0.90631

#######
# Finding Optimal cutoff values
#######

get_cutoff <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc,cutoff))) 
  
  # Return the cutoff, sensitivity, specificity and accuracy values
  colnames(out) <- c("sensitivity", "specificity", "accuracy","cutoff")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

# Sequence from 0.01 to 0.86
s = seq(.01,.86,length=100)

# Initialize the matrix
OUT = matrix(0,100,4)


for(i in 1:100)
{
  OUT[i,] = get_cutoff(s[i])
} 

# Plot the values to get optimal cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
# model_9 : 0.164545

# Choosing cut-off of 16%
test_pred_attrition <- factor(ifelse(test_pred >= 0.16, "Yes", "No"))
test_confusion <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_confusion
# Accuracy      : 0.7264
# Sensitivity   : 0.7653
# Specificity   : 0.7189


# Increase the cut-off to be more specific and see who will quit.
# Looking at the OUT variable to pick cut off produces high specificity.
# CutOFF: 0.22464646
test_pred_attrition <- factor(ifelse(test_pred >= 0.22, "Yes", "No"))
test_confusion <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_confusion

###############
# KS Statistic
###############

# Function to find out KS max value for different cutoffs
ks_lift_gain <- function(cutoff,fun_predicted,fun_actual){
# for which cutoff?
test_pred_attrition <- factor(ifelse(fun_predicted >= cutoff, "Yes", "No"))
test_cutoff_attrition <- ifelse(test_pred_attrition=="Yes",1,0)
test_actual_attrition_bin <- ifelse(fun_actual=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition_bin)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

  return(max(ks_table_test))
# for cut_off: 0.4570

}

# Set test_pred (as probabilities)
# Set test_actual_attrition (as Yes/No)

ks_lift_gain(0.30,test_pred,test_actual_attrition) # 0.39
ks_lift_gain(0.16,test_pred,test_actual_attrition) # 0.48
ks_lift_gain(0.22,test_pred,test_actual_attrition) # 0.46


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}


# Find cutoff attrition value for 22%
test_pred_attrition_2 <- factor(ifelse(test_pred >= 0.22, "Yes", "No"))
test_cutoff_attrition_2 <- ifelse(test_pred_attrition_2=="Yes",1,0)

# Set test_actual_attrtion: 1/0
# test_cutoff_attrition_2: 1/0
Attrition_decile = lift(test_actual_attrition_bin, test_cutoff_attrition_2, groups = 10)
Attrition_decile

# Plot the Gain Chart
ggplot(Attrition_decile,aes(bucket,Gain)) + geom_point() + geom_line()
ggplot(Attrition_decile,aes(bucket,Cumlift)) + geom_point() + geom_line()

########
# Summary
########

# Final Model: Model_9
# Min/max predicted value: 0.002364/0.867925
# 30% Cutoff:
#   Accuracy:		0.8382
#   Sensitivity:	0.48357
#   Specificity:	0.90631
#   KS-Max:			39%
# Optimal Cutoff [16%]:
#   Accuracy      : 0.7264
#   Sensitivity   : 0.7653
#   Specificity   : 0.7189
#   KS-Max:		  :	48%
# High Specificity cutoff [22%]:
#   Accuracy:		0.8
#   Sensitivity:	0.6291
#   Specificity:	0.8279
#   KS-Max:			46%
#   Gain:			70% (by 4th Decile)
#   Lift: 			1.74 (by 4th Decile)

#################################
# CONCLUSION
##################################
# THRESHOLD: 22%
#     - Considering the threshold/cutoff of 22%
# SPECIFICITY: 0.8279
#     - Model is built to be little highly specific, to target and identify the employees who are very much likely to quit.
# LIFT: 1.74
#     - This model, outperforms the normal model by 1.74 times (LIFT)
# GAIN: 70% (by 4th Decile)
#     - If we target the top 40% of this sorted employee list, 70% of them will be most likely to quit.
# KS-Max: 46%
#     - In the sorted list of "attrtion probablity", most of the probable candidates are on the top.
#     - While the employees who wouldnt quit will be at the bottom.
# 

