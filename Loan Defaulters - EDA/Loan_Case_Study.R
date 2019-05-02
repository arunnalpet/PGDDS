##########################################
#   Assignment : Loan Case Study
##########################################

#Set working directory & install required packages
#setwd("<working directory where data file is present")
#install.packages("lubridate")
#install.packages("dplyr")

#Load required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)

##########################################
# First Step :  Data Sourcing & Cleaning
##########################################

#1. Load data from csv file
loan_df <- read.csv("loan.csv")

#2. Glance through the data
glimpse(loan_df) 
# Observation : a lot of columns has NA values.

#3. Let run summary
summary(loan_df)
# Observation:
# 1. A number of columns are having large NA values.
# 2. Lets get the number of NA values and remove those columns

# First, lets get total number of records
n_row <- nrow(loan_df)                #39717 Records are present
n_col <- ncol(loan_df)                #111 columns

#Check for number of NA values in each column
colSums(is.na(loan_df))

#Remove the columns which has only NA values. 
#Code below removes all the columns with all NA values.
loan_df <- loan_df[,colSums(is.na(loan_df))<n_row]

#Number of Columns
n_col <- ncol(loan_df)                #57 columns now

#Observations
#1. It leaves me with 57 columns [ 54 columns are removed from dataframe]

#Lets look at columns again using summary function
summary(loan_df)

# Following Observations from summary function. It will help us to trim down the data

#################################################################
# Based on the observations from summary function, following columns can be removed to trim the dataframe further
################################################################

#1. 'tax_liens'(A tax lien is a legal claim by a government entity against a noncompliant taxpayer's assets.)
#    It has just 0 & NA values. 
#2. 'delinq_amnt':  has all Zeros. Since the feature value is not changing. It will have zero impact on the result
#3. 'chargeoff_within_12_mths' (Number of charge-offs within 12 months) : has NA's & 0 values. Will not have any impact on result
#4  'acc_now_delinq': has 0 for all values.
#5. 'application_type': has "INDIVIDUAL" for all record.
#6. 'policy_code': single policy code '1'. No Impact   
#7. 'collections_12_mths_ex_med' (Number of collections in 12 months excluding medical collections): Only NA & 0 values
#8. 'next_pymnt_d'(Next scheduled payment date): 97% of cells are emply. Next payment will have no impact on someone defaulting.
#9  'initial_list_status': has 'f' value for all loans
#10 'mths_since_last_record':  has 36931 ( ~93 % )
#11 'mths_since_last_delinq': has 25682 NA's (~65%) 
#12.'pymnt_plan': has value 'n' for all records
#13.'url' : It is just a link to loan details for a particular borrower. No impact on someone defaulting
#14.'desc': loan description. No impact

#IMP:Please note that in case of feature selection, if a particular feature has low variance ( meaning the value doesn't change a lot), that feauture can be removed from analysis. 
#    In cases where we have features, whose value doesn't change (variance = 0), those type of features can be removed

col_to_be_deleted <- c('tax_liens','delinq_amnt','chargeoff_within_12_mths','policy_code', 'collections_12_mths_ex_med','acc_now_delinq','application_type','next_pymnt_d','pymnt_plan','url','desc','mths_since_last_delinq','mths_since_last_record','initial_list_status' )
length(col_to_be_deleted)             #14 more columns deleted
loan_df <- select(loan_df,-col_to_be_deleted)

#Number of Columns
n_col <- ncol(loan_df)                #43 columns now
#We have reduced the number of columns from 111 to 43 now.

# There are other columns which are related to total_rec_int, total_rec_late_fee, total_rec_prncp which are not relevant.
# Because these values are related to recovery amount which is after someone has defaulted and will not help us predict anything
# Though we are not deleting those columns, we will not be analyzing those columns

#member_id: Check for uniqueness
sum(duplicated(loan_df$member_id))    #0 duplicate values

#Remove % from interest rate
loan_df$int_rate <- as.numeric(str_replace_all(loan_df$int_rate,"%",""))
loan_df$revol_util <- as.numeric(str_replace_all(loan_df$revol_util,"%",""))

#############################
# Derived Metrics 
#############################

# As per Upgrad 
# In "loan_status" column, the value of "Charged-off" =: Applicant has not paid the instalments in due time for a long period of time, 
#             i.e. he/she has defaulted on the loan 
# Created a new column "defaulted" 
# -> 1 : customer has defaulted loan
# -> 0 : Customer has not defaulted loan

loan_df$defaulted <- ifelse(loan_df$loan_status == "Charged Off",1,0)


########################################################
# Univariate & Bivariate Analysis
########################################################

##################################
#Basis of analysis
##################################

#1. loan_amnt : Histogram to find is there any range on which people default
#2. funded_amnt: same as above
#3. term : Which tenure loans are defaulted the most
#4. int_rate : Is there any impact of interest rate on loan defaulting
#5. grade & sub_grade: Analyze grade & sub grade
#6. emp_title : To many unique variables. It can be dropped. 
#7. home_ownership: important to analyze
#8. annual_inc : Annual Income
#9. verification_status: (If the income was verified): IMP to anlalyze
#10. issue_d : month on which loan was funded
#11. purpose : Important feature to anlayze
#12. zip_code : Since it contains 'xx' at the end and doesn't give full information
#13. addr_state: from which state major defaults are happening
#14. dti : Debt to Income ration: Important to analyze this.
#15. delinq_2yrs: number of 30+ days past-due incidences of delinquency
#16. earliest_cr_line (month the borrower's earliest reported credit line was opened): Is it significant ?
#17. open_acc : Number of open credit lines: Again not sure if it is really significant
#18. pub_rec: Number of derogatory public records : Is there any correlation here ?
#19. pub_rec_bankruptcies: number of public record bankruptcies
#20. emp_length (Employment length in years. Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years.)

# For column, emp_length (Employment length in years. 
# Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years.)
# we will normalize the data and make 10+ as 10.

#Analyze only the defaulted rows:
defaulted <- loan_df[loan_df$loan_status=="Charged Off",]

# Total Records : 39717
# Defaults : 5627 
# Percentage Default : 14.16 %

#1 Analyze loan_amnt range for which it is defaulted.
unique(defaulted$loan_status)
ggplot(defaulted, aes(loan_amnt)) + geom_histogram(binwidth = 1000,color="red")
ggplot(defaulted, aes(loan_amnt)) + geom_histogram(binwidth = 2500,color="green")
ggplot(defaulted, aes(loan_amnt)) + geom_histogram(binwidth = 5000,color="green")
#Observation: Most defaults are with loan value from 5000 - 15000

#Check for outliers
boxplot(loan_df$loan_amnt)
#Observation: Outliers present

#3. term : Which tenure loans are defaulted the most
unique(defaulted$term)
summary(defaulted$term)
ggplot(defaulted, aes(term)) + geom_bar(color="red")
#36 months  : 3227
#60 months  : 2400 
#Observation: More defaults in loan with tenure  '36 months'

#4. int_rate : Is there any impact of interest rate on loan defaulting
# Clean up % symbol and convert to numeric
defaulted$int_rate <- as.numeric(str_replace_all(defaulted$int_rate,"%",""))
ggplot(defaulted, aes(int_rate)) + geom_histogram(binwidth = 5,color="red")
# Observation: We can see that interest rates between 10-15% Interest rate are majority defaulters.

#5. grade & sub_grade
summary(defaulted$grade)
summary(defaulted$sub_grade)
ggplot(defaulted, aes(x=reorder(grade, -table(grade)[grade]))) + geom_bar()
#Observation: We can see that grades of categories "B", "C", "D" are max defaulters.
#   A    B    C    D    E    F    G 
# 602 1425 1347 1118  715  319  101 

#6. emp_title : # To many unique values .. Not relevant for analysis

#7. home_ownership: imporvtant to analyze
summary(defaulted$home_ownership)
ggplot(defaulted, aes(x=reorder(home_ownership, -table(home_ownership)[home_ownership]))) + geom_bar()
# Observation : 'RENT' & 'MORTGAGE' are the mostly defaulters.
# MORTGAGE     NONE    OTHER      OWN     RENT 
# 2327        0       18      443     2839 

#8. annual_inc : Annual Income 
summary(loan_df$annual_inc)
#MEdian: 59000
#Max: 6000000

boxplot(loan_df$annual_inc)
#Outliers present here

#find 90 percentile and 95 percentile.
quantile(loan_df$annual_inc,probs=seq(0.90,1,0.01))
#90%       91%       92%       93%       94%       95%       96%       97%       98%       99%      100% 
#116000.0  120000.0  123000.0  128371.8  135000.0  142000.0  150000.0  165757.9  187000.0  234999.4 6000000.0 

#loan_df[which(loan_df$annual_inc > 142000)]

#loan_df$annual_inc <- loan_df[loan_df$annual_inc < 142000]

ggplot (defaulted, aes(annual_inc)) + geom_histogram(binwidth = 10000)
ggplot (defaulted, aes(y=annual_inc)) + geom_boxplot()


#9. verification_status: (If the income was verified)
summary(defaulted$verification_status)
ggplot(defaulted, aes(x=reorder(verification_status, -table(verification_status)[verification_status]))) + geom_bar()
# Verified = verified + sourceVerified (1434+2051 = 3485)
# Not Verified = 2142
# Observation:  No real inference. Hence, dropped for anaysis

#10. Purpose of loan
summary(defaulted$purpose)

#ggplot : Ordering wrt to purpose
ggplot(defaulted, aes(x=reorder(purpose, -table(purpose)[purpose]))) + geom_bar()
#Observation: We can see that 'debt consolidation' is the largest factor in cases where people have defaulted
#debt_consolidation	2767
#other	633
#credit_card	542
#small_business	475

# issue_d: 
# Convert to Date Field
defaulted$issue_date <- paste("01-",defaulted$issue_d, sep="")
defaulted$issue_date <- as.Date(defaulted$issue_date,format = "%d-%b-%y")
# Observation:  No real inference. Hence, dropped for anaysis


#12. zip_code : Since it contains 'xx' at the end and doesn't give full information
# No useful information
# This is mapped properly to addr_state. So no need to analyze this.

#13. addr_state: from which state major defaults are happening
ggplot(defaulted,aes(x=reorder(addr_state, -table(addr_state)[addr_state]))) + geom_bar()
#CA	1125
#FL	504
#NY	495
#TX	316

#Compare with all records. To check for proportion of the defaults
ggplot(loan_df,aes(addr_state, fill=loan_status)) + geom_bar(position = "fill")
# NE State: NEVADA seems to default more than 50% of times. After anlyzing, we found that there are only 3 records. 
# So, this anamoly can be ignored.

#14. dti : DEbt to Income ration. Important metric signifying the credit worthiness of borrower
summary(loan_df[loan_df$defaulted==1,]$dti)    #Summary for defaulted loans
summary(loan_df[loan_df$defaulted==0,]$dti)    #Summary for non-defaulted loans
#Average DTI
#Defaulted         : 14.00
#Non Defaulated    : 13.20

#Observation : It seems people with higher DTI default more. However, the difference in mean is not significant enough.
#To ascertain, we will do hypothesis testing using Excel ( results in ppt )
# Result: There is significant difference in mean of defaulted vs non-defaulted
# So, Null hypothesis can be rejected.

#15. delinq_2yrs: number of 30+ days past-due incidences of delinquency
# Even though most the loan defaults are happening for delquency value of 0,1,2
# But if we compare it to overall status, there is no differentiator here
ggplot(defaulted, aes(delinq_2yrs)) + geom_bar()
# close to 5000 records have 0 'delinq_2yrs' record.
# Not a useful column to anlayze. Dropping from analysis


#16. earliest_cr_line (month the borrower's earliest reported credit line was opened): Is it significant ?
# Observation :No significance of this column on loan defaults

#17. open_acc : Number of open credit lines: Again not sure if it is really significant
#Observation : No significance

#18. pub_rec: Number of derogatory public records : 
#Seems that there is correlation here .. Higher chance of defaulting if you have 1 or more public derogatory remarks
ggplot(defaulted, aes(pub_rec)) + geom_bar()
#Observation : Not much here. Most of them have zero public records.

# lets see the proportion
ggplot(loan_df, aes(pub_rec, fill=loan_status)) + geom_bar(position = "fill")
# Observation : For pub_rec 1, There is higher proportion of people who defaults compared for people with 0 public record

#19. pub_rec_bankruptcies: number of public record bankruptcies
#Same here as above ... higher percentage seen in excel
ggplot(defaulted, aes(pub_rec_bankruptcies)) + geom_bar()
ggplot(loan_df, aes(pub_rec_bankruptcies, fill=loan_status)) + geom_bar(position = "fill")
#Observation : The proportion charts shows that for people with 2 pub_rec_bankruptcies, there is higher possibility for them to default.

#20 emp_length:
ggplot(defaulted,aes(x=reorder(emp_length, -table(emp_length)[emp_length]))) + geom_bar()
#Observation :  Most defaults in <1 year & 10+ years

ggplot(loan_df, aes(emp_length, fill=loan_status)) + geom_bar(position = "fill")
#Observation : However, proportion shows somewhat similar distribution


###############################################
# Bivariate Analysis 
################################################

#1.  Correlation
# Lets take continous Variables for creating a corelation matrix.
# we have are annual_inc , loan_amnt , funded_amnt , dti
cont_var_name= c("loan_amnt", "int_rate", "dti", "annual_inc")
loan_df_subset <- subset(loan_df, select = cont_var_name)
cor(loan_df_subset)

            #loan_amnt   int_rate         dti  annual_inc
#loan_amnt  1.00000000 0.30941527  0.06643935  0.27114855
#int_rate   0.30941527 1.00000000  0.11116168  0.05318516
#dti        0.06643935 0.11116168  1.00000000 -0.12273191
#annual_inc 0.27114855 0.05318516 -0.12273191  1.00000000

#Observation: No correlation betwwen "loan_amnt", "int_rate", "dti", "annual_inc" variables.


#Assumption about Grades
# low   ---> high
# A -> B -> C-> D -> E -> G

#2. DTI & Grade
ggplot(loan_df) + geom_boxplot(aes(x=grade,y=dti))
ggplot(loan_df) + geom_boxplot(aes(x=sub_grade,y=dti))

#Observation: There seems to relationship between grade/sub-grade & dti. 
#             People with higher dti has higher grades. Hence, higher risk of loan defaulting


#3. Grade & Interest Rate
ggplot(loan_df) + geom_boxplot(aes(x=grade,y=int_rate))
ggplot(loan_df) + geom_boxplot(aes(x=sub_grade,y=int_rate))

#Observation: There is direct relation between grade (subgrade) & interest rate
#             Higher the grade or subgrade, higher the interest rate ( as shown in the plot)

#4. Grade & loan amount
ggplot(loan_df) + geom_boxplot(aes(x=grade,y=loan_amnt))
ggplot(loan_df) + geom_boxplot(aes(x=sub_grade,y=loan_amnt))

#Observation : As shown in box plot, Higher grades has loans with higher amount.
#             Same behavious is seen with sub-grade also

############################################
# Hypothesis Testing ( done using excel )
#############################################
#1. dti   ( Refer ppt for details)
#2. interest rate (Refer ppt for details)


#################################
#  END OF CASE STUDY
#################################


