library(MASS)
library(DAAG) # for Cross Validation
library(scales) # for rescale to scale negative values to positive.
library(Hmisc) # ?? used for
library(memisc) # for mtable to display all the results as table.
library(DataCombine)

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
##                 Model Building for Home Audio                ##
##################################################################

sales_hom_aud <- sales_all[which(sales_all$product_analytic_sub_category == "HomeAudio"),]

# Consider only derived KPIs and marketing levers, including spl days, nps and media investment.
temp <- sales_hom_aud
sales_hom_aud <- sales_hom_aud[,-c(1,2,4:10,12,13)]


# Scale all data (had no effect)
# temp <- data.frame(lapply(sales_hom_aud[,c(2:13)],function(x) scale(x)))
# sales_hom_aud <- cbind(sales_hom_aud[,1], temp)
colnames(sales_hom_aud)[1] <- "total_gmv"

model_1 <- lm(total_gmv~., data = sales_hom_aud)
summary(model_1)
# Adjusted R-squared: 0.4798

step<-stepAIC(model_1,direction = "both")
step

model_2 <- lm(formula = total_gmv ~ avg_discount + Digital + Sponsorship + 
                Online.marketing + Affiliates + SEM + Radio + Other + spl_days, 
              data = sales_hom_aud)
summary(model_2)
# Adjustd R-squared: 0.5124

sort(vif(model_2))

# Online marketing has high VIF. Eliminating it.
model_3 <- lm(formula = total_gmv ~ avg_discount + Digital + Sponsorship + 
                Affiliates + SEM + Radio + Other + spl_days, 
              data = sales_hom_aud)
summary(model_3)
# Adjustd R-squared: 0.4868

sort(vif(model_3))


# SEM has highest VIF. Remove it.
model_4 <- lm(formula = total_gmv ~ avg_discount + Digital + Sponsorship + 
                Affiliates + Radio + Other + spl_days, 
              data = sales_hom_aud)
summary(model_4)
# Adjustd R-squared: 0.4352

sort(vif(model_4))


# Radio has high VIF. Remove it.
model_5 <- lm(formula = total_gmv ~ avg_discount + Digital + Sponsorship + 
                Affiliates + Other + spl_days, 
              data = sales_hom_aud)
summary(model_5)
# Adjustd R-squared: 0.4476

sort(vif(model_5))


# Digital has least significance. Remove it.
model_6 <- lm(formula = total_gmv ~ avg_discount + Sponsorship + 
                Affiliates + Other + spl_days, 
              data = sales_hom_aud)
summary(model_6)
# Adjustd R-squared: 0.4564

sort(vif(model_6))


# Other category has low significance. Remove it.
model_7 <- lm(formula = total_gmv ~ avg_discount + Sponsorship + 
                Affiliates + spl_days, 
              data = sales_hom_aud)
summary(model_7)
# Adjustd R-squared: 0.4648

sort(vif(model_7))


# Affiliates has low significance. Remove it.
model_8 <- lm(formula = total_gmv ~ avg_discount + Sponsorship + 
                spl_days, 
              data = sales_hom_aud)
summary(model_8)
# Adjustd R-squared: 0.4534

sort(vif(model_8))

model_lm <- model_8

# Test
predict_1 <- predict(model_8, sales_hom_aud[,-which(colnames(sales_hom_aud) == "total_gmv")])


cor(predict_1,sales_hom_aud$total_gmv)
# 0.6977267
cor(predict_1,sales_hom_aud$total_gmv)^2
# 0.4868225

# Adjusted R2 of model6 = 0.4534
# R2 from prediction = 0.4868225
# Difference is 0.033 which is low. That means, model is able to explain the data well.

##################################################################
##                          Conclusion:                         ##
##                       Audio Accessories                      ##
##################################################################

# All are significant so far.
# AverageDiscount, Sponsorship and spl_days are key factors influencing the revenue.

# Cross validation
cv.lm(data = sales_hom_aud, form.lm = model_lm, m=5, seed=33, plotit = FALSE)

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

sales_hom_aud[sales_hom_aud == 0] <- 0.00001

sales_hom_aud_log <- log(sales_hom_aud)


model_1 <- lm(total_gmv~., data = sales_hom_aud_log)
summary(model_1)
# R2: 0.38

step<-stepAIC(model_1,direction = "both")

step

model_2 <- lm(formula = total_gmv ~ avg_discount + TV + Sponsorship + Online.marketing + 
                Affiliates, data = sales_hom_aud_log)

summary(model_2)
# Adjusted R-squared: 0.419

sort(vif(model_2))

# Remove TV, has high pvalue of 0.0892
model_3 <- lm(formula = total_gmv ~ avg_discount + Sponsorship + Online.marketing + 
                Affiliates, data = sales_hom_aud_log)

summary(model_3)
# Adjusted R-squared: 0.392

sort(vif(model_3))

# Remove Online.Marketing, has high pvalue.
model_4 <- lm(formula = total_gmv ~ avg_discount + Sponsorship + 
                Affiliates, data = sales_hom_aud_log)

summary(model_4)
# Adjusted R-squared: 0.378

sort(vif(model_4))

# Remove Sponsorship and affiliates. They have high pvalues.
model_5 <- lm(formula = total_gmv ~ avg_discount, data = sales_hom_aud_log)

summary(model_5)
# # R2: 0.313

model_ml <- model_5

# Cross validation
cv.lm(data = sales_hom_aud_log, form.lm = model_ml, m=5, seed=33, plotit = FALSE)


############################################################################
############################################################################
###                                                                      ###
###                            MODEL BUILDING                            ###
###                              KOYCK MODEL                             ###
###                                                                      ###
############################################################################
############################################################################

sales_hom_aud_koyck <- sales_hom_aud
# x <- sales_hom_aud['total_gmv']
# sales_lagged <- c(NA, x[1:(length(x)-1)])

sales_hom_aud_koyck <-slide(sales_hom_aud_koyck, Var="total_gmv", slideBy = -1, NewVar = "total_gmv_1")
sales_hom_aud_koyck <-slide(sales_hom_aud_koyck, Var="total_gmv", slideBy = -2, NewVar = "total_gmv_2")

# Impute NA values by replacing with moving average.
sales_hom_aud_koyck[1,'total_gmv_1'] <- 2195607
sales_hom_aud_koyck[1,'total_gmv_2'] <- 1105669
sales_hom_aud_koyck[2,'total_gmv_2'] <- 2195607


model_1 <- lm(total_gmv~., data = sales_hom_aud_koyck)
summary(model_1)
# Adjusted R2: 0.47

step<-stepAIC(model_1,direction = "both")

step

model_2 <- lm(formula = total_gmv ~ avg_discount + TV + Digital + Sponsorship + 
                Online.marketing + Affiliates + SEM + Radio + spl_days + 
                total_gmv_1, data = sales_hom_aud_koyck)
summary(model_2)
# Adjusted R2: 0.515

sort(vif(model_2))

# Remove Digital. Has high pValue and VIF
model_3 <- lm(formula = total_gmv ~ avg_discount + TV + Sponsorship + 
                Online.marketing + Affiliates + SEM + Radio + spl_days + 
                total_gmv_1, data = sales_hom_aud_koyck)
summary(model_3)
# Adjusted R2: 0.496

sort(vif(model_3))


# Remove SEM. Has high pvalue
model_4 <- lm(formula = total_gmv ~ avg_discount + TV + Sponsorship + 
                Online.marketing + Affiliates + Radio + spl_days + 
                total_gmv_1, data = sales_hom_aud_koyck)
summary(model_4)
# Adjusted R2: 0.479

sort(vif(model_4))

# Remove Total_gvm1
model_5 <- lm(formula = total_gmv ~ avg_discount + TV + Sponsorship + 
                Online.marketing + Affiliates + Radio + spl_days, data = sales_hom_aud_koyck)
summary(model_5)
# Adjusted R2: 0.486

sort(vif(model_5))

# Remove Affiliates. Has high VIF and p-Value
model_6 <- lm(formula = total_gmv ~ avg_discount + TV + Sponsorship + 
                Online.marketing + Radio + spl_days, data = sales_hom_aud_koyck)
summary(model_6)
# Adjusted R2: 0.455

sort(vif(model_6))


# Remove TV
model_7 <- lm(formula = total_gmv ~ avg_discount + Sponsorship + 
                Online.marketing + Radio + spl_days, data = sales_hom_aud_koyck)
summary(model_7)
# Adjusted R2: 0.46

sort(vif(model_7))

# Remove Radio
model_8 <- lm(formula = total_gmv ~ avg_discount + Sponsorship + 
                Online.marketing + spl_days, data = sales_hom_aud_koyck)
summary(model_8)
# Adjusted R2: 0.466

sort(vif(model_8))


# Remove Affiliates Has high pvalue
model_9 <- lm(formula = total_gmv ~ avg_discount + Sponsorship + 
                spl_days, data = sales_hom_aud_koyck)
summary(model_9)
# Adjusted R2: 0.453

sort(vif(model_9))


model_ky <- model_9

# Cross Validation
cv.lm(data = sales_hom_aud_koyck, form.lm = model_ky, m=5, seed=33, plotit = FALSE)


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

sales_hom_aud_dlag <- sales_all[which(sales_all$product_analytic_sub_category == "HomeAudio"),]

# Consider only derived KPIs and marketing levers, including spl days, nps and media investment.
sales_hom_aud_dlag <- sales_hom_aud_dlag[,-c(2,4:10,12,13)]

sales_hom_aud_dlag <- merge(x=sales_hom_aud_dlag, y=adstock_all, by.x = "week_num", by.y = "Week")

sales_hom_aud_dlag <- sales_hom_aud_dlag[,-c(1,4:12)]

sales_hom_aud_dlag <-slide(sales_hom_aud_dlag, Var="total_gmv", slideBy = -1, NewVar = "total_gmv_1")
sales_hom_aud_dlag <-slide(sales_hom_aud_dlag, Var="total_gmv", slideBy = -2, NewVar = "total_gmv_2")

# Impute NA values by replacing with moving average.
sales_hom_aud_dlag[1,'total_gmv_1'] <- 2195607
sales_hom_aud_dlag[1,'total_gmv_2'] <- 1105669
sales_hom_aud_dlag[2,'total_gmv_2'] <- 2195607


model_1 <- lm(total_gmv~., data = sales_hom_aud_dlag)
summary(model_1)
# R2: 0.53

step<-stepAIC(model_1,direction = "both")
step

model_2 <- lm(formula = total_gmv ~ avg_discount + spl_days + NPS + AdStock_TV + 
                AdStock_Digital + AdStock_Sponsorship + AdStock_OnlineMarketing + 
                AdStock_Affiliates + AdStock_SEM + AdStock_Other + total_gmv_1 + 
                total_gmv_2, data = sales_hom_aud_dlag)
summary(model_2)
# R2 -> 0.55
sort(vif(model_2))

# Remove OnlineMarketing. Has high pvalue and vif
model_3 <- lm(formula = total_gmv ~ avg_discount + spl_days + NPS + AdStock_TV + 
                AdStock_Digital + AdStock_Sponsorship + 
                AdStock_Affiliates + AdStock_SEM + AdStock_Other + total_gmv_1 + 
                total_gmv_2, data = sales_hom_aud_dlag)
summary(model_3)
# R2 -> 0.537
sort(vif(model_3))


# Remove Affiliates. Has high pvalue and vif
model_4 <- lm(formula = total_gmv ~ avg_discount + spl_days + NPS + AdStock_TV + 
                AdStock_Digital + AdStock_Sponsorship + 
                AdStock_SEM + AdStock_Other + total_gmv_1 + 
                total_gmv_2, data = sales_hom_aud_dlag)
summary(model_4)
# R2 -> 0.516
sort(vif(model_4))

# Remove total_gmv_2 Has high pvalue
model_5 <- lm(formula = total_gmv ~ avg_discount + spl_days + NPS + AdStock_TV + 
                AdStock_Digital + AdStock_Sponsorship + 
                AdStock_SEM + AdStock_Other + total_gmv_1, data = sales_hom_aud_dlag)
summary(model_5)
# R2 -> 0.515
sort(vif(model_5))

# Remove NPS Has high pvalue
model_6 <- lm(formula = total_gmv ~ avg_discount + spl_days + AdStock_TV + 
                AdStock_Digital + AdStock_Sponsorship + 
                AdStock_SEM + AdStock_Other + total_gmv_1, data = sales_hom_aud_dlag)
summary(model_6)
# R2 -> 0.512
sort(vif(model_6))

# Remove NPS Has high pvalue
model_7 <- lm(formula = total_gmv ~ avg_discount + AdStock_TV + 
                AdStock_Digital + AdStock_Sponsorship + 
                AdStock_SEM + AdStock_Other + total_gmv_1, data = sales_hom_aud_dlag)
summary(model_7)
# R2 -> 0.512
sort(vif(model_7))

# Remove total_gmv_1 Has high pvalue
model_8 <- lm(formula = total_gmv ~ avg_discount + AdStock_TV + 
                AdStock_Digital + AdStock_Sponsorship + 
                AdStock_SEM + AdStock_Other, data = sales_hom_aud_dlag)
summary(model_8)
# R2 -> 0.512
sort(vif(model_8))


# Remove total_gmv_1 Has high pvalue
model_9 <- lm(formula = total_gmv ~ avg_discount + AdStock_TV + 
                AdStock_Digital + AdStock_Sponsorship + 
                AdStock_SEM + AdStock_Other, data = sales_hom_aud_dlag)
summary(model_9)
# R2 -> 0.483
sort(vif(model_9))

# Remove adStock_Other Has high pvalue
model_10 <- lm(formula = total_gmv ~ avg_discount + AdStock_TV + 
                AdStock_Digital + AdStock_Sponsorship + 
                AdStock_SEM, data = sales_hom_aud_dlag)
summary(model_10)
# R2 -> 0.471
sort(vif(model_10))

# Remove AdStock_SEM Has high pvalue
model_11 <- lm(formula = total_gmv ~ avg_discount + AdStock_TV + 
                 AdStock_Digital + AdStock_Sponsorship, data = sales_hom_aud_dlag)
summary(model_11)
# R2 -> 0.471
sort(vif(model_11))


# Remove Adstock_digital Has high pvalue
model_12 <- lm(formula = total_gmv ~ avg_discount + AdStock_TV + 
                 AdStock_Sponsorship, data = sales_hom_aud_dlag)
summary(model_12)
# R2 -> 0.443
sort(vif(model_12))



# Remove Adstock_tv Has high pvalue
model_13 <- lm(formula = total_gmv ~ avg_discount +  
                AdStock_Sponsorship, data = sales_hom_aud_dlag)
summary(model_13)
# R2 -> 0.414
sort(vif(model_13))



model_dlag <- model_13

# Cross Validation
cv.lm(data = sales_hom_aud_dlag, form.lm = model_dlag, m=5, seed=33, plotit = FALSE)


###########################################################################
###########################################################################
###                                                                     ###
###                            MODEL BUILDING                           ###
###                    DLAG AND MULTIPLICATIVE MODEL                    ###
###                                                                     ###
###########################################################################
###########################################################################


sales_hom_aud_dlag_mul <- sales_hom_aud_dlag
sales_hom_aud_dlag_mul[is.na(sales_hom_aud_dlag_mul)] <- 0.00001
sales_hom_aud_dlag_mul[sales_hom_aud_dlag_mul == 0] <- 0.00001

sales_hom_aud_dlag_mul <- log(sales_hom_aud_dlag_mul)


model_1 <- lm(total_gmv~., data = sales_hom_aud_dlag_mul)
summary(model_1)
# R2: 0.892

step<-stepAIC(model_1,direction = "both")
step

model_2 <- lm(formula = total_gmv ~ avg_discount + AdStock_Digital + AdStock_Sponsorship + 
                AdStock_ContentMarketing + AdStock_OnlineMarketing + AdStock_Affiliates + 
                AdStock_SEM + AdStock_Radio + AdStock_Other + total_gmv_1 + 
                total_gmv_2, data = sales_hom_aud_dlag_mul)
summary(model_2)

sort(vif(model_2))
# R2: 0.895

# All are highly significant

# Remove higher VIF variables.

model_3 <- lm(formula = total_gmv ~ AdStock_ContentMarketing + AdStock_OnlineMarketing + AdStock_Affiliates + 
                AdStock_SEM + total_gmv_2, data = sales_hom_aud_dlag_mul)
summary(model_3)
sort(vif(model_3))
# R2: 84.2

model_dlag_mul <- model_3

# Cross Validation
cv.lm(data = sales_hom_aud_dlag, form.lm = model_dlag_mul, m=5, seed=33, plotit = FALSE)



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
write_html(FinalResults, file="output/model_hom_aud_results.html")



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

# Choosing models based on good R2 values and choice of explanatory variables:
    # Linear : 0.487
    # Distributed Lag + Multiplicative: 0.857


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
plot_elasticity(model_dlag_mul, sales_hom_aud_dlag_mul, "Elasticity for Home Audio using Distributed Lag Model + Multiplicative")
# AdStock_OnlineMarketing

plot_elasticity(model_lm, sales_hom_aud, "Elasticity for Home Audio using Linear Model")
# avg_discount & Sponsorship


