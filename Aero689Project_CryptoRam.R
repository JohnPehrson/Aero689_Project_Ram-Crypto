rm(list = ls())   #clear current memory
graphics.off()    #close all graphics windows
cat("\014")       #clear the console

library(tidyverse)
library(ggplot2)
source("toDateTime.R")
library(dplyr)


setwd("/Users/clark/Documents/GitHub/Aero689_Project_Ram-Crypto")

Crypto_factors_filename     = "DIM_CRYPTO_DATA_filt.csv"
GPU_factors_filename        = "DIM_GPU_PROD_filt.csv"
Merchant_factors_filename   = "DIM_MERCHANT_filt.csv"
Region_factors_filename     = "DIM_REGION_filt.csv"
Time_factors_filename       = "DIM_TIME_filt.csv"
GPU_price_filename          = "FACT_GPU_PRICE_filt.csv"
Crypto_price_filename       = "FACT_CRYPTO_RATE_filt.csv"

Factors.Crypto <- read.csv(Crypto_factors_filename, header=TRUE, stringsAsFactors=FALSE)
Factors.GPU <- read.csv(GPU_factors_filename, header=TRUE, stringsAsFactors=FALSE)
Factors.Merchant <- read.csv(Merchant_factors_filename, header=TRUE, stringsAsFactors=FALSE)
Factors.Region <- read.csv(Region_factors_filename, header=TRUE, stringsAsFactors=FALSE)
Factors.Time <- read.csv(Time_factors_filename, header=TRUE, stringsAsFactors=FALSE)
Price.GPU <- read.csv(GPU_price_filename, header=TRUE, stringsAsFactors=FALSE)
Price.Crypto <- read.csv(Crypto_price_filename, header=TRUE, stringsAsFactors=FALSE)



## Inner product of all GPUs and a single cryptocurrency
all_curr_ids = Factors.Crypto$Id
curr_id = all_curr_ids[6]
single_curr <- Price.Crypto[Price.Crypto$CodeId == curr_id,]
test = merge(Price.GPU, single_curr, by = "TimeId")

names(Factors.Merchant)[1] <- 'MerchantId'
corr_df = merge(test, Factors.Merchant, by = "MerchantId")
corr_df$RegionId <- factor(corr_df$RegionId,
                           levels = c(1, 3, 4, 9, 10, 11),
                           labels = c("AUD", "CAD", "EUR","NZD","GBP","USD"))
corr_df <- corr_df[,2:ncol(corr_df)]
corr_df$Merchant <- as.factor(corr_df$Merchant)   # Convert character column to factor
str(corr_df)



#Single ANOVA
  #get all cards of the same type
    all_card_ids = Factors.GPU$Id
    card_id = all_card_ids[4]
    single_card <- Price.GPU[Price.GPU$ProdId == card_id,]

  #get all prices of a single currency
    all_curr_ids = Factors.Crypto$Id
    curr_id = all_curr_ids[6]
    single_curr <- Price.Crypto[Price.Crypto$CodeId == curr_id,]

  #left-join the card with a single cryptocurrency
    corr_df = merge(single_card, single_curr, by = "TimeId")
      #what if I trim the data down to when crypto really skyrockets?
    corr_df = corr_df[corr_df$TimeId>20170000,]
    corr_df = toDateTime(corr_df)
  #make factors
    names(Factors.Merchant)[1] <- 'MerchantId'
    corr_df = merge(corr_df, Factors.Merchant, by = "MerchantId")
    corr_df$RegionId <- factor(corr_df$RegionId,
                       levels = c(1, 3, 4, 9, 10, 11),
                       labels = c("AUD", "CAD", "EUR","NZD","GBP","USD"))
    corr_df <- corr_df[,2:ncol(corr_df)]
    corr_df$Merchant <- as.factor(corr_df$Merchant)   # Convert character column to factor
    str(corr_df)
    
    

  #scatterplot each through time
    ggplot(corr_df,                    # Change colors of lines by group
           aes(x = TimeId,
               y = Price_USD,
               col = RegionId)) +
      geom_point()
  
    ggplot(corr_df,                    # Change colors of lines by group
           aes(x = TimeId,
               y = Open)) +
      geom_point()
  
  #scatterplot vs each other
  ggplot(corr_df,                    # Change colors of lines by group
         aes(x = Open,
             y = Price_USD,
             col = RegionId)) +
    geom_point()


  

 
  
  
  