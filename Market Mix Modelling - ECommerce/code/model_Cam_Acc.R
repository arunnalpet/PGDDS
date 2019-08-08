library(MASS)
library(DAAG) # for Cross Validation
library(scales) # for rescale to scale negative values to positive.
library(Hmisc)
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

# Where is the discount???

#################################################################
##            Model Building for Camera Accessories            ##
#################################################################

sales_cam_acc <- sales_all[which(sales_all$product_analytic_sub_category == "CameraAccessory"),]

# Consider only derived KPIs and marketing levers, including spl days, nps and media investment.
temp <- sales_cam_acc
sales_cam_acc <- sales_cam_acc[,-c(1,2,4:10,12,13)]

# 12 = avg_unitPrice.

# Scale all data (had no effect)
# temp <- data.frame(lapply(sales_cam_acc[,c(2:13)],function(x) scale(x)))
# sales_cam_acc <- cbind(sales_cam_acc[,1], temp)
colnames(sales_cam_acc)[1] <- "total_gmv"

model_1 <- lm(total_gmv~., data = sales_cam_acc)
summary(model_1)
# Adjusted R-squared: 0.5224

step<-stepAIC(model_1,direction = "both")

step

model_2 <- lm(formula = total_gmv ~ TV + Digital + Sponsorship + Content.Marketing + 
                Affiliates + SEM + Radio + spl_days, data = sales_cam_acc)

summary(model_2)
# Adjusted R-squared: 0.5562

sort(vif(model_2))

# VIF of digital marketing is too high = 115. Remove it.

model_3 <- lm(formula = total_gmv ~ TV + Sponsorship + Content.Marketing + 
                Affiliates + SEM + Radio + spl_days, data = sales_cam_acc)

summary(model_3)
# Adjusted R-squared: 0.501

sort(vif(model_3))


# VIF of content marketing is too high = 48. Remove it.
model_4 <- lm(formula = total_gmv ~ TV + Sponsorship + 
                Affiliates + SEM + Radio + spl_days, data = sales_cam_acc)

summary(model_4)
# Adjusted R-squared: 0.502

sort(vif(model_4))

# P-value of Radio is too high = 0.3. Remove it.
model_5 <- lm(formula = total_gmv ~ TV + Sponsorship + 
                Affiliates + SEM + spl_days, data = sales_cam_acc)

summary(model_5)
# Adjusted R-squared: 0.5037

sort(vif(model_5))


# P-Value of spl days is too high = 0.37. Remove it.
model_6 <- lm(formula = total_gmv ~ TV + Sponsorship + 
                Affiliates + SEM, data = sales_cam_acc)
summary(model_6)
# Adjusted R-squared: 0.5059

sort(vif(model_6))

model_lm <- model_6

# Test
predict_1 <- predict(model_6, sales_cam_acc[,-which(colnames(sales_cam_acc) == "total_gmv")])

cor(predict_1,sales_cam_acc$total_gmv)
# 0.7380205
cor(predict_1,sales_cam_acc$total_gmv)^2
# 0.5446743

# Adjusted R2 of model6 = 0.5059
# R2 from prediction = 0.5446743
# Difference is 0.0387743 which is low. That means, model is able to explain the data well.

# Plot actual and predicted
plot(sales_cam_acc$total_gmv, main='Camera Accessory', xlab = 'week', ylab = 'PredictGMV')
lines(sales_cam_acc$total_gmv)
lines(predict_1, col='red', lwd=2)


# Cross validation
cv.lm(data = sales_cam_acc, form.lm = model_lm, m=5, seed=33, plotit=FALSE)

##################################################################
##                          Conclusion:                         ##
##                      Camera Accessories                      ##
##################################################################

# All are significant so far.
# TV, Sponsorship, Affiliates and SEM are key factors influencing the revenue.


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

sales_cam_acc[sales_cam_acc == 0] <- 0.00001

sales_cam_acc_log <- log(sales_cam_acc)


model_1 <- lm(total_gmv~., data = sales_cam_acc_log)
summary(model_1)


step<-stepAIC(model_1,direction = "both")

step

model_2 <- lm(formula = total_gmv ~ Content.Marketing + Online.marketing + 
     Affiliates, data = sales_cam_acc_log)

summary(model_2)
# Adjusted R-squared: 0.6933

sort(vif(model_2))

# Try removing ContentMarketing/Affiliates and test.
model_3 <- lm(formula = total_gmv ~ Content.Marketing + Online.marketing, data = sales_cam_acc_log)

model_ml <- model_3

summary(model_3)
# Adjusted R-squared: 0.6933

sort(vif(model_3))
# R2-Reduced to 0.6477

# Cross validation
cv.lm(data = sales_cam_acc_log, form.lm = model_ml, m=5, seed=33, plotit=FALSE)

############################################################################
############################################################################
###                                                                      ###
###                            MODEL BUILDING                            ###
###                              KOYCK MODEL                             ###
###                                                                      ###
############################################################################
############################################################################

sales_cam_acc_koyck <- sales_cam_acc
# x <- sales_cam_acc['total_gmv']
# sales_lagged <- c(NA, x[1:(length(x)-1)])

sales_cam_acc_koyck <-slide(sales_cam_acc_koyck, Var="total_gmv", slideBy = -1, NewVar = "total_gmv_1")
sales_cam_acc_koyck <-slide(sales_cam_acc_koyck, Var="total_gmv", slideBy = -2, NewVar = "total_gmv_2")

# Impute NA values by replacing with moving average.
sales_cam_acc_koyck[1,'total_gmv_1'] <- 2195607
sales_cam_acc_koyck[1,'total_gmv_2'] <- 1105669
sales_cam_acc_koyck[2,'total_gmv_2'] <- 2195607

model_1 <- lm(total_gmv~., data = sales_cam_acc_koyck)
summary(model_1)


step<-stepAIC(model_1,direction = "both")

step

model_2 <- lm(formula = total_gmv ~ TV + Sponsorship + Content.Marketing + 
                Affiliates, data = sales_cam_acc_koyck)

model_ky <- model_2

summary(model_2)
# Adjusted R-squared: 0.6933

sort(vif(model_2))
# Final R2: 0.495

# Cross Validation
cv.lm(data = sales_cam_acc_koyck, form.lm = model_ky, m=5, seed=33, plotit=FALSE)


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

sales_cam_acc_dlag <- sales_all[which(sales_all$product_analytic_sub_category == "CameraAccessory"),]

# Consider only derived KPIs and marketing levers, including spl days, nps and media investment.
sales_cam_acc_dlag <- sales_cam_acc_dlag[,-c(2,4:10,12,13)]

sales_cam_acc_dlag <- merge(x=sales_cam_acc_dlag, y=adstock_all, by.x = "week_num", by.y = "Week")

sales_cam_acc_dlag <- sales_cam_acc_dlag[,-c(1,4:12)]

sales_cam_acc_dlag <-slide(sales_cam_acc_dlag, Var="total_gmv", slideBy = -1, NewVar = "total_gmv_1")
sales_cam_acc_dlag <-slide(sales_cam_acc_dlag, Var="total_gmv", slideBy = -2, NewVar = "total_gmv_2")
# sales_cam_acc_dlag <-slide(sales_cam_acc_dlag, Var="total_gmv", slideBy = -3)

# Impute NA values by replacing with moving average.
sales_cam_acc_dlag[1,'total_gmv_1'] <- 2195607
sales_cam_acc_dlag[1,'total_gmv_2'] <- 1105669
sales_cam_acc_dlag[2,'total_gmv_2'] <- 2195607


model_1 <- lm(total_gmv~., data = sales_cam_acc_dlag)
summary(model_1)

step<-stepAIC(model_1,direction = "both")
step

model_2 <- lm(formula = total_gmv ~ AdStock_TV + AdStock_Digital + AdStock_Sponsorship + 
                AdStock_Affiliates + AdStock_SEM + AdStock_Other + total_gmv_1 + 
                total_gmv_2, data = sales_cam_acc_dlag)
summary(model_2)
# R2 -> 0.462
sort(vif(model_2))


model_3 <- lm(formula = total_gmv ~ AdStock_TV + AdStock_Sponsorship + 
                AdStock_Affiliates + AdStock_SEM +  
                total_gmv_2, data = sales_cam_acc_dlag)

model_dlag <- model_3

summary(model_3)
sort(vif(model_3))
# R2: 0.409

# Cross Validation
# Replace NAs.
cv.lm(data = sales_cam_acc_dlag, form.lm = model_dlag, m=5, seed=33, plotit=FALSE)


###########################################################################
###########################################################################
###                                                                     ###
###                            MODEL BUILDING                           ###
###                    DLAG AND MULTIPLICATIVE MODEL                    ###
###                                                                     ###
###########################################################################
###########################################################################


sales_cam_acc_dlag_mul <- sales_cam_acc_dlag
sales_cam_acc_dlag_mul[is.na(sales_cam_acc_dlag_mul)] <- 0.00001
sales_cam_acc_dlag_mul[sales_cam_acc_dlag_mul == 0] <- 0.00001

sales_cam_acc_dlag_mul <- log(sales_cam_acc_dlag_mul)


model_1 <- lm(total_gmv~., data = sales_cam_acc_dlag_mul)
summary(model_1)

step<-stepAIC(model_1,direction = "both")
step

model_2 <- lm(formula = total_gmv ~ avg_discount + NPS + AdStock_TV + AdStock_Digital + 
     AdStock_Sponsorship + AdStock_ContentMarketing + AdStock_OnlineMarketing + 
     AdStock_Affiliates + AdStock_SEM + AdStock_Radio + AdStock_Other + 
     total_gmv_1 + total_gmv_2, data = sales_cam_acc_dlag_mul)
summary(model_2)

sort(vif(model_2))
# R2: 0.955


# OnlineMarketing is very significant, but has really high VIF. Reconsider this.
model_3 <- lm(formula = total_gmv ~ avg_discount + AdStock_Digital + 
                AdStock_ContentMarketing + AdStock_OnlineMarketing + 
                AdStock_Affiliates + AdStock_SEM + AdStock_Radio + 
                total_gmv_1 + total_gmv_2, data = sales_cam_acc_dlag_mul)
summary(model_3)

sort(vif(model_3))

model_dlag_mul <- model_3
# Stopping here as we have got 93.4 R2, and there is high inter-dependency 
# between Affiliates & OnlineMarketing.


predict_1 <- predict(model_3, sales_cam_acc_dlag_mul[,-which(colnames(sales_cam_acc_dlag_mul) == "total_gmv")])

cor(predict_1,sales_cam_acc_dlag_mul$total_gmv)
# 0.7380205
cor(predict_1,sales_cam_acc_dlag_mul$total_gmv)^2
# 0.5446743

# Adjusted R2 of model6 = 0.5059
# R2 from prediction = 0.5446743
# Difference is 0.0387743 which is low. That means, model is able to explain the data well.

# Plot actual and predicted
plot(sales_cam_acc_dlag_mul$total_gmv, main='Camera Accessory', xlab = 'week', ylab = 'PredictGMV')
lines(sales_cam_acc_dlag_mul$total_gmv)
lines(predict_1, col='red', lwd=2)


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
write_html(FinalResults, file="output/model_cam_acc_results.html")


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
# Multiplicative
# Koyck
# Distributed Lag Model
# Linear

# Choosing models based on good R2 values and choice of explanatory variables:
# Distributed Lag + Multiplicative: 0.946


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
plot_elasticity(model_dlag_mul, sales_cam_acc_dlag_mul, "Elasticity for Camera Accessory using Distributed Lag Model + Multiplicative")
# AdStock_OnlineMarketing
# AdStock_Digital

