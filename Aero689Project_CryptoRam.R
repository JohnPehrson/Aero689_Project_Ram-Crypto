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

  #get all cards of the same type
    all_card_ids = Factors.GPU$Id
    card_id = all_card_ids[12]
    single_card <- Price.GPU[Price.GPU$ProdId == card_id,]

  #get all prices of a single currency
    all_curr_ids = Factors.Crypto$Id
    curr_id = all_curr_ids[6]
    single_curr <- Price.Crypto[Price.Crypto$CodeId == curr_id,]

  #left-join the card with a single cryptocurrency
    corr_df = merge(single_card, single_curr, by = "TimeId")
      #what if I trim the data down to when crypto really skyrockets?
    corr_df = corr_df[corr_df$TimeId>20170500,]
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

  
  #fixed model
  m1.gls <- gls(Price_USD~Open, data = corr_df, method = "REML",na.action=na.omit)
  #alternative models
  m1.lme4 <- lmer(Price_USD~Open + (1|RegionId), data = corr_df, REML = TRUE)
  m1a.lme4 <- lmer(Price_USD~Open+RegionId + (1|RegionId), data = corr_df, REML = TRUE)
  m2.lme4<- lmer(Price_USD~Open+RegionId*Merchant + (1|RegionId), data = corr_df, REML = TRUE)
  m2a.lme4<- lmer(Price_USD~Open+RegionId+Merchant + (1|RegionId), data = corr_df, REML = TRUE)
  
  #AIC comparison of FM and alt
  AIC(m1.gls,m1.lme4,m1a.lme4,m2.lme4,m2a.lme4)
  
  #Narrow down plausible models
  m1.lme4_ml <- lmer(Price_USD~Open + (1|RegionId), data = corr_df, REML = FALSE)
  m1a.lme4_ml <- lmer(Price_USD~Open+RegionId + (1|RegionId), data = corr_df, REML = FALSE)
  m2.lme4_ml<- lmer(Price_USD~Open+RegionId+Merchant + (1|RegionId), data = corr_df, REML = FALSE)
  
  # anova(m1.lme4_ml)
  # anova(m1a.lme4_ml)
  # anova(m2.lme4_ml)
  # anova(m1.lme4_ml,m1a.lme4_ml,m2.lme4_ml)
  #use m1, with random intercept on region
  m1.lme4 <- lmer(Price_USD~Open + (1|RegionId), data = corr_df, REML = TRUE)
  summary(m1.lme4)
    
  #various data tests
  resid_panel(m1.lme4, plots = "all")
  ggqqplot(resid(m1.lme4))
  #residuals are pretty substantially  non-normal. Use a transformation
  #try logorithmic on dependent variable (cryptocurrency price)
  #try a square-root on dependent variable (cryptocurrency price)
  m1.lme4_log <- lmer(Price_USD~log(Open) + (1|RegionId), data = corr_df, REML = TRUE)
  m1.lme4_sqrt <- lmer(Price_USD~sqrt(Open) + (1|RegionId), data = corr_df, REML = TRUE)
  summary(m1.lme4_log)
  summary(m1.lme4_sqrt)
  
  resid_panel(m1.lme4_log, plots = "all")
  resid_panel(m1.lme4_sqrt, plots = "all")
  #still not fixed. Work to create/use a transformation to correct for data non-normality
  
  
  

  #Plotting the chosen fit
  m1.plot <- ggemmeans(m1.lme4, c("Open"), type = "fe")
  ggplot(corr_df, aes(x = Open)) +
    geom_point(aes(y = Price_USD, color = RegionId)) +
    geom_line(data = m1.plot, aes(x = x, y = predicted)) +
    geom_ribbon(data = m1.plot, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2)



  
  
  
  