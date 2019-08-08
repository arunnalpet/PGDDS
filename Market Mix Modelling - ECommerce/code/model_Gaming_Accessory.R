library(MASS)
library(DAAG) # for Cross Validation
library(scales) # for rescale to scale negative values to positive.
library(Hmisc) # ?? used for
library(memisc) # for mtable to display all the results as table.
library(DataCombine)


# install.packages('DataCombine')

###########################################################################
###########################################################################
###                                                                     ###
###                            MODEL BUILDING                           ###
###                          LINEAR REGRESSION                          ###
###                                                                     ###
###########################################################################
###########################################################################

# Performing Model building, to identify the relationship of KPIs with the gmv
# Since we are only trying to predict the top KPIs, we dont need to split data to test and train.

##----------------------------------------------------------------

# set directory
setwd('C:/Project/IIITB/7. Capstone Project/Final')

# Load data
sales_all <- read.csv("dataset/sales_all.csv")
sales_all <- sales_all[,-1]

##################################################################
##             Model Building for Audio Accessories             ##
##################################################################

sales_gam_acc <- sales_all[which(sales_all$product_analytic_sub_category == "GamingAccessory"),]

# Consider only derived KPIs and marketing levers, including spl days, nps and media investment.
temp <- sales_gam_acc
sales_gam_acc <- sales_gam_acc[,-c(1,2,4:10,12,13)]


# Scale all data (had no effect)
# Include no. of units
# temp <- data.frame(lapply(sales_gam_acc[,c(2:13)],function(x) scale(x)))
# sales_gam_acc <- cbind(sales_gam_acc[,1], temp)
colnames(sales_gam_acc)[1] <- "total_gmv"

model_1 <- lm(total_gmv~., data = sales_gam_acc)
summary(model_1)
# Adjusted R-squared: 0.6009

step<-stepAIC(model_1,direction = "both")
step

model_2 <- lm(formula = total_gmv ~ avg_discount + TV + Digital + Sponsorship + 
                Content.Marketing + Affiliates + SEM + Radio, data = sales_gam_acc)

summary(model_2)
# Adjusted R-squared: 0.6093

sort(vif(model_2))

# SEM has high VIF=114, and high p-value=0.04. Remove it.
model_3 <- lm(formula = total_gmv ~ avg_discount + TV + Digital + Sponsorship + 
                Content.Marketing + Affiliates + Radio, data = sales_gam_acc)

summary(model_3)
# Adjusted R-squared: 0.5891

sort(vif(model_3))


# avg_discount has less significance. Remove it.
model_4 <- lm(formula = total_gmv ~ TV + Digital + Sponsorship + 
                Content.Marketing + Affiliates + Radio, data = sales_gam_acc)

summary(model_4)
# Adjusted R-squared: 0.5699

sort(vif(model_4))

model_lm <- model_4

# Test
predict_1 <- predict(model_4, sales_gam_acc[,-which(colnames(sales_gam_acc) == "total_gmv")])


cor(predict_1,sales_gam_acc$total_gmv)
# 0.7871045
cor(predict_1,sales_gam_acc$total_gmv)^2
# 0.6195334

# Adjusted R2 of model4 = 0.5699
# R2 from prediction = 0.6195

# Plot actual and predicted
plot(sales_gam_acc$total_gmv, main='Game Accessory', xlab = 'week', ylab = 'PredictGMV')
lines(sales_gam_acc$total_gmv)
lines(predict_1, col='red', lwd=2)

# TEMP
# Cross validation
cv.lm(data = sales_gam_acc, form.lm = model_lm, m=2, seed=33, plotit = FALSE)

##################################################################
##                          Conclusion:                         ##
##                       Audio Accessories                      ##
##################################################################

# All are significant so far.
# TV, Digital, Sponsorhip, Content Marketing, Affiliates and Radio are the ones influencing gmv.

############################################################################
############################################################################
###                                                                      ###
###                            MODEL BUILDING                            ###
###                         MULTIPLICATIVE MODEL                         ###
###                                                                      ###
############################################################################
############################################################################

# Get natural log values of dependent and independent variables.
# Log of 0 is not defined, hence replace zero to negligable value (0.00001)

sales_gam_acc[sales_gam_acc == 0] <- 0.00001

sales_gam_acc_log <- log(sales_gam_acc)


model_1 <- lm(total_gmv~., data = sales_gam_acc_log)
summary(model_1)


step<-stepAIC(model_1,direction = "both")

step

model_2 <- lm(formula = total_gmv ~ avg_discount + TV + Content.Marketing + 
                Online.marketing + Affiliates, data = sales_gam_acc_log)

summary(model_2)
# Adjusted R-squared: 0.806

sort(vif(model_2))

# Remove Online.marketing with high VIF = 316.29
model_3 <- lm(formula = total_gmv ~ avg_discount + TV + Content.Marketing + 
                Affiliates, data = sales_gam_acc_log)

summary(model_3)
# Adjusted R-squared: 0.787

sort(vif(model_3))

# Remove Affiliates. Which is insignificant and has high VIF
model_4 <- lm(formula = total_gmv ~ avg_discount + TV + Content.Marketing, data = sales_gam_acc_log)

summary(model_4)
# Adjusted R-squared: 0.791

sort(vif(model_4))

# Remove Content Marketing which has high pvalue(0.144)
model_5 <- lm(formula = total_gmv ~ avg_discount + TV, data = sales_gam_acc_log)

summary(model_5)
# Adjusted R-squared: 0.786

sort(vif(model_5))


# Remove avg_discount which has high pvalue(0.25)
model_6 <- lm(formula = total_gmv ~ TV, data = sales_gam_acc_log)
summary(model_6)
# Adjusted R-Squared: 0.785
sort(vif(model_6))

model_ml <- model_6

# Cross validation
cv.lm(data = sales_gam_acc_log, form.lm = model_ml, m=5, seed=33, plotit = FALSE)


############################################################################
############################################################################
###                                                                      ###
###                            MODEL BUILDING                            ###
###                              KOYCK MODEL                             ###
###                                                                      ###
############################################################################
############################################################################

sales_gam_acc_koyck <- sales_gam_acc
# x <- sales_gam_acc['total_gmv']
# sales_lagged <- c(NA, x[1:(length(x)-1)])

sales_gam_acc_koyck <-slide(sales_gam_acc_koyck, Var="total_gmv", slideBy = -1, NewVar = "total_gmv_1")
sales_gam_acc_koyck <-slide(sales_gam_acc_koyck, Var="total_gmv", slideBy = -2, NewVar = "total_gmv_2")

# Impute NA values by replacing with moving average.
sales_gam_acc_koyck[1,'total_gmv_1'] <- 2195607
sales_gam_acc_koyck[1,'total_gmv_2'] <- 1105669
sales_gam_acc_koyck[2,'total_gmv_2'] <- 2195607


model_1 <- lm(total_gmv~., data = sales_gam_acc_koyck)
summary(model_1)
# Adjusted R2: 0.638

step<-stepAIC(model_1,direction = "both")

step

model_2 <- lm(formula = total_gmv ~ avg_discount + TV + Digital + Sponsorship + 
                Content.Marketing + Affiliates + SEM + Radio + NPS + total_gmv_1, 
              data = sales_gam_acc_koyck)
summary(model_2)
# Adjusted R2: 0.648

sort(vif(model_2))


# Remove NPS. It has high p value
model_3 <- lm(formula = total_gmv ~ avg_discount + TV + Digital + Sponsorship + 
                Content.Marketing + Affiliates + SEM + Radio + total_gmv_1, 
              data = sales_gam_acc_koyck)
summary(model_3)
# Adjusted R2: 0.63.4

sort(vif(model_3))


# Remove average_discount. It has high p value
model_4 <- lm(formula = total_gmv ~ TV + Digital + Sponsorship + 
                Content.Marketing + Affiliates + SEM + Radio + total_gmv_1, 
              data = sales_gam_acc_koyck)
summary(model_4)
# Adjusted R2: 0.626

sort(vif(model_4))

# Remove average_discount. It has high p value
model_4 <- lm(formula = total_gmv ~ TV + Digital + Sponsorship + 
                Content.Marketing + Affiliates + SEM + Radio + total_gmv_1, 
              data = sales_gam_acc_koyck)
summary(model_4)
# Adjusted R2: 0.626

sort(vif(model_4))


# Remove SEM. It has high VIF.
model_5 <- lm(formula = total_gmv ~ TV + Digital + Sponsorship + 
                Content.Marketing + Affiliates + Radio + total_gmv_1, 
              data = sales_gam_acc_koyck)
summary(model_5)
# Adjusted R2: 0.626

sort(vif(model_5))


# Remove total_gmv_1. It has high pvalue.
model_6 <- lm(formula = total_gmv ~ TV + Digital + Sponsorship + 
                Content.Marketing + Affiliates + Radio, 
              data = sales_gam_acc_koyck)
summary(model_6)
# Adjusted R2: 0.648

sort(vif(model_6))

model_ky <- model_6

# Cross Validation
cv.lm(data = sales_gam_acc_koyck, form.lm = model_ky, m=5, seed=33, plotit = FALSE)


###########################################################################
###########################################################################
###                                                                     ###
###                            MODEL BUILDING                           ###
###                        DISTRIBUTED LAG MODEL                        ###
###                                                                     ###
###########################################################################
###########################################################################

adstock_all <- read.csv('dataset/AdStock_All.csv')

sales_all <- read.csv("dataset/sales_all.csv")
sales_all <- sales_all[,-1]


#################################################################
##            Model Building for Camera Accessories            ##
#################################################################

sales_gam_acc_dlag <- sales_all[which(sales_all$product_analytic_sub_category == "GamingAccessory"),]

# Consider only derived KPIs and marketing levers, including spl days, nps and media investment.
sales_gam_acc_dlag <- sales_gam_acc_dlag[,-c(2,4:10,12,13)]

sales_gam_acc_dlag <- merge(x=sales_gam_acc_dlag, y=adstock_all, by.x = "week_num", by.y = "Week")

sales_gam_acc_dlag <- sales_gam_acc_dlag[,-c(1,4:12)]

sales_gam_acc_dlag <-slide(sales_gam_acc_dlag, Var="total_gmv", slideBy = -1, NewVar = "total_gmv_1")
sales_gam_acc_dlag <-slide(sales_gam_acc_dlag, Var="total_gmv", slideBy = -2, NewVar = "total_gmv_2")

# Impute NA values by replacing with moving average.
sales_gam_acc_dlag[1,'total_gmv_1'] <- 2195607
sales_gam_acc_dlag[1,'total_gmv_2'] <- 1105669
sales_gam_acc_dlag[2,'total_gmv_2'] <- 2195607


model_1 <- lm(total_gmv~., data = sales_gam_acc_dlag)
summary(model_1)

step<-stepAIC(model_1,direction = "both")
step

model_2 <- lm(formula = total_gmv ~ AdStock_TV + AdStock_Digital + AdStock_Sponsorship + 
                AdStock_ContentMarketing + AdStock_Affiliates + AdStock_SEM + 
                AdStock_Radio + total_gmv_1 + total_gmv_2, data = sales_gam_acc_dlag)
summary(model_2)
# R2 -> 0.643
sort(vif(model_2))
# All the variables are highly significant! 


model_2 <- lm(formula = total_gmv ~ AdStock_TV + AdStock_Digital + AdStock_Sponsorship + 
                AdStock_ContentMarketing + AdStock_Affiliates + AdStock_SEM + 
                AdStock_Radio + total_gmv_1 + total_gmv_2, data = sales_gam_acc_dlag)
summary(model_2)
# R2: 0.643

model_dlag <- model_2

# Cross Validation
cv.lm(data = sales_gam_acc_dlag, form.lm = model_dlag, m=5, seed=33, plotit = FALSE)


###########################################################################
###########################################################################
###                                                                     ###
###                            MODEL BUILDING                           ###
###                    DLAG AND MULTIPLICATIVE MODEL                    ###
###                                                                     ###
###########################################################################
###########################################################################


sales_gam_acc_dlag_mul <- sales_gam_acc_dlag
sales_gam_acc_dlag_mul[is.na(sales_gam_acc_dlag_mul)] <- 0.00001
sales_gam_acc_dlag_mul[sales_gam_acc_dlag_mul == 0] <- 0.00001

sales_gam_acc_dlag_mul <- log(sales_gam_acc_dlag_mul)


model_1 <- lm(total_gmv~., data = sales_gam_acc_dlag_mul)
summary(model_1)

step<-stepAIC(model_1,direction = "both")
step

model_2 <- lm(formula = total_gmv ~ avg_discount + AdStock_Digital + AdStock_Sponsorship + 
                AdStock_ContentMarketing + AdStock_OnlineMarketing + AdStock_Affiliates + 
                AdStock_SEM + AdStock_Radio + AdStock_Other + total_gmv_1 + 
                total_gmv_2, data = sales_gam_acc_dlag_mul)
summary(model_2)

sort(vif(model_2))
# R2: 0.895

# All are highly significant

model_2 <- lm(formula = total_gmv ~ AdStock_ContentMarketing + AdStock_OnlineMarketing + AdStock_Affiliates + 
                total_gmv_2, data = sales_gam_acc_dlag_mul)
summary(model_2)
sort(vif(model_2))
# R2: 81.9

model_dlag_mul <- model_2

###########################################################################
###########################################################################
###                                                                     ###
###                          MODEL COMPARISION                          ###
###                                                                     ###
###########################################################################
###########################################################################


FinalResults <- mtable("Linear" = model_lm,
                       "Multiplicative" = model_ml, 
                       "Koyck" = model_ky,
                       "DLag" = model_dlag,
                       "DLag + Mul" = model_dlag_mul, summary.stats=c("sigma","R-squared","F"))

show_html(FinalResults)
write_html(FinalResults, file="output/model_gam_acc_results.html")

###########################################################################
###########################################################################
###                                                                     ###
###                           MODEL SELECTION                           ###
###                                 AND                                 ###
###                         ELASTICITY ANALYSIS                         ###
###                                                                     ###
###########################################################################
###########################################################################

# Comparing the results from mtable output and choosing appropriate model.

# Dropping the following models becasue of low R2 and high error:
# Linear
# Multiplicative
# Koyck

# Choosing models based on good R2 values and choice of explanatory variables:
# Distributed Lag: 0.705
# Distributed Lag + Multiplicative: 0.832


# Function to calculate elasticiy of each variable in the model.
getElasticityVector <- function(modelX, modelData){
  # Initialize
  var_list <- list()
  edf <- data.frame(VarName = character(), Elasticity=double(), varCoeff=double(), stringsAsFactors = FALSE)
  
  for(i in 2:length(modelX$coefficients)){
    varname <- names(modelX$coefficients)[i]
    var_elasticity <- as.numeric(modelX$coefficients[varname]*mean(modelData[,varname])/mean(modelData$total_gmv))
    edf[nrow(edf)+1,] <- list(varname, var_elasticity, round(modelX$coefficients[i],2))
  }
  return(edf)
}


# Function to retrieve elasticity and plot elasticity chart
plot_elasticity <- function(modelX, modelData, chartTitle){
  edf <- getElasticityVector(modelX, modelData)
  
  # Plot it
  ggplot(edf, aes(x=reorder(VarName,Elasticity),y=Elasticity)) + 
    geom_bar(position="dodge",stat="identity", width = 0.8) + 
    geom_text(aes(label=varCoeff), color = 'Red') + ylab("Elasticity") + xlab("Variable Names") +
    coord_flip() + ggtitle(chartTitle) 
}


# dlag_var_list <- getElasticityVector(model_dlag, sales_hom_aud_dlag)
# dlag_mul_var_list <- getElasticityVector(model_dlag_mul, sales_hom_aud_dlag_mul)

# Call function to calculate and plot elasticity.
plot_elasticity(model_dlag, sales_gam_acc_dlag, "Elasticity for Gaming Accessory using Distributed Lag Model")
# AdStock_Affiliates
# AdStock_Digital
# AdStock_Sponsorship
# AdStock_Radio

plot_elasticity(model_dlag_mul, sales_gam_acc_dlag_mul, "Elasticity for Gaming Accessory using Distributed Lag Model + Multiplicative")
# AdStock Online Marketing

