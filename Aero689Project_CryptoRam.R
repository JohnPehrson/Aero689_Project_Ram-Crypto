rm(list = ls())   #clear current memory
graphics.off()    #close all graphics windows
cat("\014")       #clear the console

library(tidyverse)
library(ggplot2)
source("toDateTime.R")
library(dplyr)
library(ggpubr)
library(DHARMa)
library(lme4)
library(nlme)
library(lmerTest)
library(ggeffects)
library(ggResidpanel)
setwd("path/to/project/")




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
all_cards_one_crypto_df = merge(Price.GPU, single_curr, by = "TimeId")

names(Factors.Merchant)[1] <- 'MerchantId'
all_cards_one_crypto_df = merge(all_cards_one_crypto_df, Factors.Merchant, by = "MerchantId")
all_cards_one_crypto_df$RegionId <- factor(all_cards_one_crypto_df$RegionId,
                           levels = c(1, 3, 4, 9, 10, 11),
                           labels = c("AUD", "CAD", "EUR","NZD","GBP","USD"))
all_cards_one_crypto_df <- all_cards_one_crypto_df[,2:ncol(all_cards_one_crypto_df)]
all_cards_one_crypto_df$Merchant <- as.factor(all_cards_one_crypto_df$Merchant)   # Convert character column to factor
all_cards_one_crypto_df = toDateTime(all_cards_one_crypto_df)
str(all_cards_one_crypto_df)



  #get all cards of the same type
    all_card_ids = Factors.GPU$Id
    card_id = all_card_ids[5]
    single_card_df <- all_cards_one_crypto_df[all_cards_one_crypto_df$ProdId == card_id,]


  #scatterplot each through time
    ggplot(single_card_df,                    # Change colors of lines by group
           aes(x = TimeId,
               y = Price_USD,
               col = RegionId)) +
      geom_point()
  
    ggplot(single_card_df,                    # Change colors of lines by group
           aes(x = TimeId,
               y = Open)) +
      geom_point()
  
  #scatterplot vs each other
  ggplot(single_card_df,                    # Change colors of lines by group
         aes(x = Open,
             y = Price_USD,
             col = RegionId)) +
    geom_point()

  
  ggplot(single_card_df, aes(x = Open, y = Price_USD)) +
    geom_point() +
    stat_smooth(method = "lm") +
    facet_wrap(~ RegionId, ncol = 3)
  
  ## correlation test, pearson correlation and shapiro wilk
  ## Test for heteroscedasticity using a Spearman's rho test
    ## breusch-pagan test, brown-forsythe, hartley test (all for variance testing)
  
  
  
  
  

 
  
  
  