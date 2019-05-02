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
head(loan_df) 
# Observation : a lot of columns has NA values.

#3. Let Summary
summary(loan_df)
# Observation:
# 1. A number of columns are having large NA values.
# 2. Lets get the number of NA values and remove those columns

# First, lets get total number of records
n_row <- nrow(loan_df)                #39717 Records are present

#Check for number of NA values in each column
colSums(is.na(loan_df))

#Remove the columns which has only NA values.
loan_df <- loan_df[,colSums(is.na(loan_df))<n_row]

#Observations: 
#1. It leaves me with 57 columnss [ out of 111 columns]

#Lets look at columns again using summary function
summary(loan_df)
# Observations

# Following columns can be removed to clean up the data.
#1. tax_liens(A tax lien is a legal claim by a government entity against a noncompliant taxpayer's assets.)
#   It has just 0 & NA values. 
#2. delinq_amnt:  has all Zeros. Since the feature value is not changing. It will have zero impact on the result
#3. chargeoff_within_12_mths (Number of charge-offs within 12 months) : has NA's & 0 values. Will not have any impact on result
#4  acc_now_delinq: has 0 for all values.
#5. application_type: has "INDIVIDUAL" for all record.
#6. policy_code: single policy code '1'. No Impact   
#7. collections_12_mths_ex_med (Number of collections in 12 months excluding medical collections): Only NA & 0 values
#8. next_pymnt_d(Next scheduled payment date): 97% of cells are emply. Next payment will have no impact on someone defaulting.
#9  initial_list_status: has 'f' value for all loans
#10 mths_since_last_record :  has 36931 ( ~93 % )
#11 mths_since_last_delinq: has 25682 NA's (~65%)   ??? Not sure
#12. pymnt_plan: has value 'n' for all records
#13. url : It is just a link to loan details for a particular borrower. No impact on someone defaulting
#14. desc: loan description. No impact

col_to_be_deleted <- c('tax_liens','delinq_amnt','chargeoff_within_12_mths','policy_code', 'collections_12_mths_ex_med','acc_now_delinq','application_type','next_pymnt_d','pymnt_plan','url','desc','mths_since_last_delinq','mths_since_last_record','initial_list_status' )
length(col_to_be_deleted)
loan_df <- select(loan_df,-col_to_be_deleted)


# There are other columns which are related to total_rec_int, total_rec_late_fee, total_rec_prncp which may not be relevant.
# We are not removing them from dataframe 

#member_id should be unique
sum(duplicated(loan_df$member_id))    #0 duplicate values

# As per Upgrad 
# In "loan_status" column, the value of "Charged-off" =: Applicant has not paid the instalments in due time for a long period of time, 
#             i.e. he/she has defaulted on the loan 
# Created a new column "defaulted" 
# -> 1 : customer has defaulted loan
# -> 0 : Customer has not defaulted loan

loan_df$defaulted <- ifelse(loan_df$loan_status == "Charged Off",1,0)

# For column, emp_length (Employment length in years. Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years.)
# we will normalize the data and make 10+ as 10.


########################################################
# Univariate & Bivariate Analysis
########################################################

#Important columns to look for 
#1. loan_amnt : Histogram to find is there any range on which people default
#2. funded_amnt: same as above
#3. term : Which tenure loans are defaulted the most
#4. int_rate : Is there any impact of interest rate on loan defaulting
#5. grade & sub_grade
#6. emp_title : Not sure if it is important column to analze .. any comments ??
#7. home_ownership: imporvtant to analyze
#8. annual_inc : Annual Income
#9. verification_status: (If the income was verified): IMP to anlalyze
#10. issue_d : month on which loan was funded
#11. purpose : Important feature to anlayze
#12. zip_code : Not sure since it contains 'xx' at the end and doesn't give full information
#13. addr_state: from which state major defaults are happening
#14. dti : Dipankar, Can u explain what dti mean ?
#15. delinq_2yrs: number of 30+ days past-due incidences of delinquency
#16. earliest_cr_line (month the borrower's earliest reported credit line was opened): Is it significant ?
#17. open_acc : Number of open credit lines: Again not sure if it is really significant
#18. pub_rec: Number of derogatory public records : Is there any correlation here ?
#19. pub_rec_bankruptcies: number of public record bankruptcies


##################################
# Derived metrics
#################################
#Does a month in 'issue_d' has any correlation with someone defaulting ?
#derive month & plot it

write.csv(file = "loan_clean.csv",x = loan_df)


#Analyze only the defaulted rows:
defaulted <- loan_df[loan_df$loan_status=="Charged Off",]
# Have 5627 of 39717 rows of defaulters to analyze and conclude

#1 Analyze loan_amnt range for which it is defaulted.
unique(defaulted$loan_status)
ggplot(defaulted, aes(loan_amnt)) + geom_histogram(binwidth = 1000)
ggplot(defaulted, aes(loan_amnt)) + geom_histogram(binwidth = 2500)


#3. term : Which tenure loans are defaulted the most
unique(defaulted$term)
summary(defaulted$term)
# We can see that 36 months 

#4. int_rate : Is there any impact of interest rate on loan defaulting
# Clean up % symbol and convert to numeric
defaulted$int_rate <- as.numeric(str_replace_all(defaulted$int_rate,"%",""))
ggplot(defaulted, aes(int_rate)) + geom_histogram(binwidth = 5)
# We can see that interest rates between 10-15% Interest rate are majority defaulters.

#5. grade & sub_grade
summary(defaulted$grade)
# We can see that grades of categories "B", "C", "D" are max defaulters.

#6. emp_title : Not sure if it is important column to analze .. any comments ??
# Need to do it properly
# Handle NA here
sort(count(group_by(defaulted, emp_title))$n,decreasing = TRUE)
#NAs are 484. Only few are repeated. rest are unique. So it doesnt impact much.


#7. home_ownership: imporvtant to analyze
summary(defaulted$home_ownership)
# We can see that mostly Mortgage and RENT people are the mostly defaulters.

#8. annual_inc : Annual Income
summary(defaulted$annual_inc)
# ggplot (defaulted, aes(annual_inc)) + geom_histogram()
# ggplot (defaulted, aes(annual_inc)) + geom_boxplot()
boxplot(defaulted$annual_inc)
# Important to note 75 percentile is below 20000
# find 90 percentile and 95 percentile.


#9. verification_status: (If the income was verified): IMP to anlalyze
summary(defaulted$verification_status)
# Not much varying result

