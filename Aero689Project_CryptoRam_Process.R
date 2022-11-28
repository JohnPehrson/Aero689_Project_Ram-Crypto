rm(list = ls())   #clear current memory
graphics.off()    #close all graphics windows
cat("\014")       #clear the console

setwd("~/GitHub/Aero689_Project_Ram-Crypto")
library(tidyverse)
library(ggplot2)
source("toDateTime.R")
library(dplyr)
library(lme4)
library(nlme)
library(lmerTest)
library(ggeffects)
library(emmeans)
library(ggResidpanel)
library(ggpubr)
library(pwr)
library(catdata)
library(MASS)
library(nlme)
library(DHARMa)
library(mgcv)
library(RVAideMemoire)
library(car)
library(EnvStats)
library(coin)
library(leaps)
library(caret)
library(psych)

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

## Initialize iterated values
gpu_count = nrow(Factors.GPU)
crypto_count = nrow(Factors.Crypto)

  #create data frame with 0 rows and 3 columns
  iter_df <- data.frame(matrix(ncol = 8, nrow = 0))

#provide column names
colnames(iter_df) <- c('Iteration', 'CardId', 'Card_Name', 'CryptoID', 'Crypto_Name','Normality_P_value',
                  'Regression_Coeff','P_value')

# i = 12
# j = 4

for (i in 1:gpu_count) {
  for (j in 1:crypto_count) {
      #get all cards of the same type
        all_card_ids = Factors.GPU$Id
        card_id = all_card_ids[i] 
        single_card_df <- Price.GPU[Price.GPU$ProdId == card_id,]
        card_info <- Factors.GPU[ Factors.GPU$Id==card_id,]
        card_plot_title = paste(card_info$Processor,card_info$Processor_Manufacturer,'by'
                                ,card_info$GPU_Manufacturer, sep = " ", collapse = NULL)
        
      #get all prices of a single currency
        all_curr_ids = Factors.Crypto$Id
        curr_id = all_curr_ids[j]
        single_curr_df <- Price.Crypto[Price.Crypto$CodeId == curr_id,]
        curr_info <- Factors.Crypto[ Factors.Crypto$Id==curr_id,]
        curr_plot_title = paste(curr_info$Currency_Name,'Cryptocurrency', sep = " ", collapse = NULL)
    
      #left-join the card with a single cryptocurrency
        corr_df = merge(single_card_df, single_curr_df, by = "TimeId")
          #what if I trim the data down to when crypto really skyrockets?
        corr_df = corr_df[corr_df$TimeId>20170500,]
    
      #make factors
        names(Factors.Merchant)[1] <- 'MerchantId'
        corr_df = merge(corr_df, Factors.Merchant, by = "MerchantId")
        corr_df$RegionId <- factor(corr_df$RegionId,
                           levels = c(1, 3, 4, 9, 10, 11),
                           labels = c("AUD", "CAD", "EUR","NZD","GBP","USD"))
        corr_df <- corr_df[,2:ncol(corr_df)]
        corr_df$Merchant <- as.factor(corr_df$Merchant)   # Convert character column to factor
        corr_df = toDateTime(corr_df)
        # str(corr_df)
        corr_df <- corr_df[,-c(7,8,9)]  #get rid of columns with extra cryptocurrency stuff

      #Model
      m1.a.lme4 <- lmer(Price_USD~Open + (1|Merchant), data = corr_df, REML = TRUE)
      p_norm = shapiro.test(resid(m1.a.lme4))$p.value
      
            
            #get the regression coefficient
            reg_coeff <- summary(m1.a.lme4)$coefficients[2,1]
            p_val <- summary(m1.a.lme4)$coefficients[2,5]
            
            iter_df[nrow(iter_df) + 1,] = c(nrow(iter_df) + 1,i,card_plot_title,j,
                                            curr_plot_title,p_norm,reg_coeff,p_val)
            #c('Iteration', 'CardId', 'Card_Name', 'CryptoID', 'Crypto_Name','Normal P'
            #'Regression_Coeff','P_value')
            

  }
}

        
        
        
        