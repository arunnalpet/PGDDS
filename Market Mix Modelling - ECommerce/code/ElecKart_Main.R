###  This is the code to perform Market Mix Modelling for given sales data.    
###  Data Sources available:                                                   
###    - Item wise sales data.                                                 
###    - Media Investment Spends.                                              
###    - Special Sale Calander                                                 
###    - Monthly NPS score.                                                    
###   Note: Please reset the working directory according to your environment   


# Importing necessary packages
library(lubridate)
library(dplyr)
library(MASS)
library(car)
library(ggplot2)

# set directory
# setwd('C:/Project/IIITB/7. Capstone Project/dataset')
setwd('C:/Project/IIITB/7. Capstone Project/Final')
# IMPORTANT INSTRUCTIONS TO RUN THE CODE.
# The uploaded zip file should be unzipped and placed under above location.
# The above location should hence contain:
  # code    <Directory: Containing all R Code>
  # dataset <Contains input data. Also should place original csv files here while executing>
  # output  <Contains the generated results and intermediate files>

############################################################################
############################################################################
###                                                                      ###
###                              IMPORTANT                               ###
###                            CODE STRUCTURE                            ###
###                                                                      ###
############################################################################
############################################################################
# Working Directory
  # Set Working Directory appropriately 
  # Source files will be read from this directory.
  # Intermediate Files are present in this directory.
  # Final results are written to this directory.
  # The scripts expect the following files to be present in that Directory
    # ConsumerElectronics.csv (Original File. Should be copied while execution)
    # AdStock_All.csv (Intermediate File: Provided in the solution zip)
    # MediaInvestment.csv (Modified File: Converted to R readable format)
    # MonthlyNPSscore.csv (Modified File: Converted to R readable format)
    # SpecialSale.csv (Modified File: Converted to R readable format)


# Code execution flow: The following files needs to be executed in sequential order.
  # 1. ElecKart.R is the main file to start with.
      # ElecKart will perform Data Cleaning and EDA.
      # After cleaning ElecKart will be generating intermediate cleaned file 'sales_all.csv'
  # 2. Category wise model building file. Uses 'sales_all.csv' as input and produces corresponding HTML results file.
      # 2.1: model_Cam_Acc.R
      # 2.2: model_Gaming_Accessory.R
      # 2.3: model_hom_aud.R

# Model comparision HTML output files are written to Working Directory.
# {workingDirectory}/dataset/AdStockCalculation.csv contians manually calculated adStock values for each of marketing levers.
# ASSUMPTION: Decay time of 5 weeks and same decay rate is assumed for all marketing levers. (As explained in the AdStock video)


###########################################################################
###########################################################################
###                                                                     ###
###                              LOAD DATA                              ###
###                                                                     ###
###########################################################################
###########################################################################

# Sales Data
sales <- read.csv('dataset/ConsumerElectronics.csv')

# For convinience and since data was small, xls was manually converted to csv
# in R readable format.

# Media Investment Data
media <- read.csv('dataset/MediaInvestment.csv')
media[is.na(media)] <- 0


# Special Sale Calendar.
splSale <- read.csv('dataset/SpecialSale.csv')
splSale$Date<- mdy(splSale$Date, tz = "UTC")

# NPS Score
npsscore <- read.csv('dataset/MonthlyNPSscore.csv')
npsscore$Date<- mdy(npsscore$Date, tz = "UTC")

############################################################################
############################################################################
###                                                                      ###
###                           DATA EXPLORATION                           ###
###                                                                      ###
############################################################################
############################################################################


summary(sales)
str(sales)

# No. of records present.
nrow(sales)
# 16,48,824

# cust_id, pincode, OrderID and OrderItemID are represented in exponential format.
# Convert it to integers.
options(scipen=999)

# Check the data now
# View(sales)

# Rename FSN, order payment type column name
colnames(sales)[1] <- "fsn_id"
colnames(sales)[11] <- "payment_type"

# Analyze each column
# FSN_ID:
# Unique records of each SKU
length(unique(sales$fsn_id))
# 21,219 different products are available in ElecKart store.

# Order Date:
# This column contains the date format. We will not be dealing with time series forecasting.
# Hence will not include this model directly for modelling.
# But later, will need to derive other factors from this column.

# Order_id
# Unique records of order_id
length(unique(sales$order_id))
# 15,01,177

# order_item_id
# Unique records of order_item_id
length(unique(sales$order_item_id))
# 1480765

# GMV: Gross Merchandise Value or Revenue
# Observing the data for higher numbers of units, 
# We are assuming GMV mentioned here is for the total number of units specified against it.
summary(sales$gmv)
# There are Records having 0 GMV. Should be incorrect, hence delete it.
nrow(sales[which(sales$gmv == 0),])
# 1349 rows doesnt have GMV

# Units
sort(unique(sales$units))

# Order Payment Type:
unique(sales$payment_type)
# We have two types
# Prepaid, Cash on Delivery.

# SLA
sort(unique(sales$sla))
# Observe records which have SLA more than 1000 days.
sales[which(sales$sla>200),]
# 4 items have SLA more than 200, which seems faulty data.

sales[which(sales$sla>65),]
# 8 records have more than 65 days of SLA.
# We should reset these to 65 days.

# We can now see that customer ID and zip code is in negative. Lets explore it.
nrow(sales[which(sales$cust_id < 0),])
# 8,23,414 have negative cutomer id.

length(unique(sales[which(sales$cust_id < 0),"cust_id"]))
# 6,01,237 are unique customers which have negative value.
# This looks random
# Hence Leaving it as is.
# This is unique ID, hence leaving it as is, not considering for our analysis.

# Zip code is also negative.
# Not considering Zipcode for our analysis.

# Product MRP
summary(sales$product_mrp)
# Product ranges from 0 to 2,99,999
# Explore free products.
nrow(sales[which(sales$product_mrp == 0),])
# 5308 products have zero MRP but positive GMV. 
# Need to clean up/lookup MRP for that.
length(unique(sales[which(sales$product_mrp == 0), "fsn_id"]))
# 249 out of 21,219 products do not have MRP

# Product Procurement SLA
summary(sales$product_procurement_sla)
sort(unique(sales$product_procurement_sla))
nrow(sales[which(sales$product_procurement_sla>999),])
# 4745 rows
length(unique(sales[which(sales$product_procurement_sla>999),"fsn_id"]))
# 59 SKUs have product procurement slas greater than 999.

nrow(sales[which(sales$product_procurement_sla == -1),])
# 75986 records have negative procurement sla.
length(unique(sales[which(sales$product_procurement_sla == -1),"fsn_id"]))
# 540 SKUs have product procurement sla of -1


# Analyze Month
summary(sales$Month)
sort(unique(sales$Month))
# Month data is clean

# Analyze Year
unique(sales$Year)
# Data is clean.

# deliverycdays and deliverybdays
unique(sales$deliverybdays)
unique(sales$deliverycdays)

nrow(sales[which(sales$deliverybdays == "\\N"),])
# 1312972
nrow(sales[which(sales$deliverycdays == "\\N"),])
# 1312971
# Almost 80% of the data is filled with this value. Which has no interpretation.
# Hence ignoring these two columns for modelling.

# Look for NAs
sum(is.na(sales))
# 14,712

# Check which columns have NA values.
sapply(sales, function(x) length(which(is.na(x))))
# cust_id, pin-code ang gmv each of them have 4094 null values.



###########################################################################
###########################################################################
###                                                                     ###
###                               CLEANUP                               ###
###                                                                     ###
###########################################################################
###########################################################################


# Delete rows having 0 for these attributes.
# gmv
# product_mrp
# product_procurement_sla
sales <- sales[-which(sales$gmv == 0),]
sales <- sales[-which(sales$product_mrp == 0),]
sales <- sales[-which(sales$product_procurement_sla == 0),]

##----------------------------------------------------------------

# Delete rows having -1 for these attributes.
# product_procurement_sla
sales <- sales[-which(sales$product_procurement_sla == -1),]

##----------------------------------------------------------------

# Cap these attributes to trim the outliers.
# sla
# product_procurement_sla

# cap sla greater than 100 to 65
sales$sla[which(sales$sla > 100)] <- 65

# cap product_procurement_sla greater than 100 to 15
sales$product_procurement_sla[which(sales$product_procurement_sla > 100)] <- 15

##----------------------------------------------------------------

# Delete Rows having NA for the following attributes.
# gmv
# cust_id
sales <- na.omit(sales)


##----------------------------------------------------------------

# Convert to date format.
head(sales$order_date)
sales$order_date <- ymd_hms(sales$order_date)

##----------------------------------------------------------------

# Clean up data that does not lies between July 2015 to June 2016
nrow(sales[which(sales$order_date < "2015-07-01"),])
# 6
nrow(sales[which(sales$order_date > "2016-06-30"),])
# 4830
sales <- sales[-which(sales$order_date < "2015-07-01"),]
sales <- sales[-which(sales$order_date > "2016-06-30"),]

##----------------------------------------------------------------

# Filter data to include only three analytic sub category as mentioned in problem statement.
# CameraAccessory
# GamingAccessory
# HomeAudio

sales <- sales[which(sales$product_analytic_sub_category %in% c("CameraAccessory", "GamingAccessory", "HomeAudio")),]


############################################################################
############################################################################
###                                                                      ###
###                    DERIVE OTHER FACTORS FROM DATA                    ###
###                                                                      ###
############################################################################
############################################################################

# Derive week number.

# Since we are considering 1 years data, we will have 53 weeks.
# Consider 1-July-2015 as week1
# Week number is calculated as  (sales$order_date - 30-June-2015)/7. Modulo of this will be the week number.
start_date_ref <- as.POSIXct(ymd("2015-06-30", tz = "UTC"))
# as.numeric(sales[1,"order_date"] - as.POSIXct(ymd("2015-07-01", tz = "UTC")))
sales$week_num <- ceiling(as.numeric(sales$order_date - start_date_ref)/7)


##----------------------------------------------------------------

# dicount offered. ((MRP-(GMV/Units))/MRP)*100

sales$discount <- ((sales$product_mrp-(sales$gmv/sales$units))/sales$product_mrp)*100
sales$discount <- round(sales$discount,2)
sort(unique(ceiling(sales$discount)))
# Discount ranges from -195% to 99%
# negative value here indicates that the product was upsold. (as compared to mrp)

##----------------------------------------------------------------

# Unit price

sales$unitPrice <- sales$gmv/sales$units
summary(sales$unitPrice)


###########################################################################
###########################################################################
###                                                                     ###
###                              GENERATE                               ###
###                    WEEKLY AGGREGATED INFORMATION                    ###
###                                                                     ###
###########################################################################
###########################################################################


##################################################################
##                           Generate                           ##
##                     Weekly consumer data                     ##
##################################################################


# Aggregate by product_analytic_sub_category and week.
# sum(gmv)
# sum(units)
# Payment Type: Categorical value, convert to dummy.
# mean(sla)
# product mrp??? mean/sum?
# mean(product_procurement_sla)
# week number
# mean(discount)
# mean(unitPrices) mean/sum?

# Payment Type: Converting to binary format
sales$payment_cod <- ifelse(sales$payment_type=="COD",1,0)
sales$payment_prepaid <- ifelse(sales$payment_type=="Prepaid",1,0)

sales_per_week <- group_by(sales, week_num, product_analytic_sub_category)

sales_per_week <- summarise(sales_per_week, total_gmv = sum(gmv), total_units=sum(units),
          no_of_orders = n(),        
          total_payment_cod=sum(payment_cod), total_payment_prepaid = sum(payment_prepaid),
          avg_sla=mean(sla), avg_product_mrp=mean(product_mrp),
          avg_product_procurement_sla=mean(product_procurement_sla),
          avg_discount=mean(discount),
          avg_unitPrice = mean(unitPrice))

sales_per_week <- as.data.frame(sales_per_week)


##################################################################
##                           Generate                           ##
##                 Weekly Media Investment Data                 ##
##################################################################


# Create date to week mapping data frame
daytoweek<-data.frame(day = (seq(ymd('2015-07-01'),ymd('2016-06-30'), by = 'days')), daynum=seq(1,366))
daytoweek$week_num <- ceiling(daytoweek$daynum/7)
daytoweek$day <- ymd(daytoweek$day)
daytoweek$Month <- month(as.POSIXlt(daytoweek$day))

##----------------------------------------------------------------

# Calculate daily wise advertisement spend.


media_weekly <- daytoweek

temp <- merge(x=media_weekly, y=media, by = "Month")

for (media_type in c("Total.Investment","TV","Digital","Sponsorship",
                     "Content.Marketing","Online.marketing","Affiliates","SEM","Radio","Other"))
  temp[,media_type] <- temp[,media_type]/(as.numeric(days_in_month(temp$Month)))

media_weekly <- temp

##----------------------------------------------------------------

# Aggregate daily wise spend to week wise.
media_weekly <- as.data.frame(temp[,c(4,6:15)] %>% group_by(week_num) %>% summarise_all(list(sum)))


#################################################################
##                          Calculate                          ##
##                     Weekly Special Sale                     ##
#################################################################

# Check how many special sale are there in each week.
splSale$spl_days <- 1
splSale <- splSale[,-2]
splSale$week_num <- ceiling(as.numeric(splSale$Date - start_date_ref)/7)

splSale <- as.data.frame(splSale[,c(2,3)] %>% group_by(week_num) %>% summarise(spl_days=sum(spl_days)))

# splSale_weekly <- merge(x = daytoweek, y=splSale, on.x = "day", on.y = "Date")

temp <- data.frame(week_num=seq(1,53))
splSale_weekly <- merge(x=temp, y=splSale, on=week_num, all.x = TRUE)
splSale_weekly[is.na(splSale_weekly)] <- 0


#################################################################
##                          Generate                           ##
##                       Weekly NPS data                       ##
#################################################################

# npsscore$week_num <- ceiling(as.numeric(npsscore$Date - start_date_ref)/7)
npsscore$Month <- month(as.POSIXlt(npsscore$Date))

nps_weekly <- daytoweek

temp <- merge(x=nps_weekly, y=npsscore[,c(2,3)], by = "Month")

temp[,"NPS"] <- temp[,"NPS"]/(as.numeric(days_in_month(temp$Month)))

nps_weekly <- temp

##----------------------------------------------------------------

# Aggregate daily wise spend to week wise.
nps_weekly <- as.data.frame(temp[,c(4,5)] %>% group_by(week_num) %>% summarise(NPS=sum(NPS)))




############################################################################
############################################################################
###                                                                      ###
###                          MERGE ALL WEEKLY:                           ###
###              SALES, SPL HOLIDAYS, MEDIA INVESTMENT, NPS              ###
###                                                                      ###
############################################################################
############################################################################

sales_all <- merge(x=sales_per_week, y=media_weekly, on=week_num)
sales_all <- merge(x=sales_all, y=splSale_weekly, on=week_num)
sales_all <- merge(x=sales_all, y=nps_weekly, on=week_num)

# Remove redundant/unwanted columns not needed for model building.
# Total.Investments
# Week Number
# Analytics sub category.

##----------------------------------------------------------------

# Write the Merged data to a intermediate csv file
write.csv(sales_all, file = "dataset/sales_all.csv")

##----------------------------------------------------------------

## Analyze sub-categories
# GMV vs weekNumber + spl_days
# Vertical lines indicates sales on special days.
saledays <- sales_all$week[sales_all$spl_days > 1]

ggplot(sales_all, aes(x=week_num, y=total_gmv, group=product_analytic_sub_category)) + 
  geom_line(aes(color=product_analytic_sub_category), size=1) +
  geom_vline(xintercept = saledays, size = 0.5, linetype = 4) + 
  ggtitle("Category wise GMV and special sale indication")

##----------------------------------------------------------------

# Mkt spend vs category gmv
# Camera Accessories
camera_sales_all <- sales_all[which(sales_all['product_analytic_sub_category'] == "CameraAccessory"),]
ggplot(camera_sales_all, aes(x=week_num)) + 
  geom_line(aes(y=total_gmv/100000), color = 'black', size = 1.2) + 
  geom_line(aes(y=Total.Investment), color = 'Red', size = 0.8) + 
  ggtitle("Camera Accessory GMV vs Total Marketing Spend")

# Gaming Accessories
game_sales_all <- sales_all[which(sales_all['product_analytic_sub_category'] == "GamingAccessory"),]
ggplot(game_sales_all, aes(x=week_num)) + 
  geom_line(aes(y=total_gmv/100000), color = 'black', size = 1.2) + 
  geom_line(aes(y=Total.Investment), color = 'Red', size = 0.8) + 
  ggtitle("Gaming Accessory GMV vs Total Marketing Spend")

# Home Audio
home_sales_all <- sales_all[which(sales_all['product_analytic_sub_category'] == "HomeAudio"),]
ggplot(home_sales_all, aes(x=week_num)) + 
  geom_line(aes(y=total_gmv/100000), color = 'black', size = 1.2) + 
  geom_line(aes(y=Total.Investment), color = 'Red', size = 0.8) + 
  ggtitle("Home Audio GMV vs Total Marketing Spend")
