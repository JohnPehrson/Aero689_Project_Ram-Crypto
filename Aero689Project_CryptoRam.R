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



      #get all cards of the same type
        all_card_ids = Factors.GPU$Id
        card_id = all_card_ids[25] 
        single_card_df <- Price.GPU[Price.GPU$ProdId == card_id,]
        card_info <- Factors.GPU[ Factors.GPU$Id==card_id,]
        card_plot_title = paste(card_info$Processor,card_info$Processor_Manufacturer,'by'
                                ,card_info$GPU_Manufacturer, sep = " ", collapse = NULL)
        
        corr_df = single_card_df
        # for (i in 1:nrow(1:Factors.Crypto)) {
          #get all prices of a single currency
            all_curr_ids = Factors.Crypto$Id
            curr_id = all_curr_ids[1]
            single_curr_df <- Price.Crypto[Price.Crypto$CodeId == curr_id,]
            curr_info <- Factors.Crypto[ Factors.Crypto$Id==curr_id,]
            
            curr_info$Currency_Name
            

          #left-join the card with a single cryptocurrency
            corr_df = merge(corr_df, single_curr_df, by = "TimeId")
        # }
        
          
          all_curr_ids = Factors.Crypto$Id
          curr_id = all_curr_ids[5]
          single_curr_df <- Price.Crypto[Price.Crypto$CodeId == curr_id,]
          curr_info <- Factors.Crypto[ Factors.Crypto$Id==curr_id,]
          curr_plot_title = paste(curr_info$Currency_Name,'Cryptocurrency', sep = " ", collapse = NULL)
          
          corr_df2 = merge(corr_df, single_curr_df, by = "TimeId")
          
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
        str(corr_df)
        corr_df <- corr_df[,-c(7,8,9)]  #get rid of columns with extra cryptocurrency stuff
        
        
        #scatterplot each through time
        ggplot(corr_df,                    # Change colors of lines by region
               aes(x = TimeId,
                   y = Price_USD,
                   col = RegionId)) +
          xlab('Time')+
          ylab('Price of Sold Graphics Card [USD]')+
          ggtitle(card_plot_title)+
          theme(plot.title = element_text(hjust = 0.5))+
          geom_point()
      
        ggplot(corr_df,                    # Change colors of lines by region
               aes(x = TimeId,
                   y = Open)) +
          xlab('Time')+
          ylab('Price of Cryptocurrency')+
          ggtitle(curr_plot_title)+
          theme(plot.title = element_text(hjust = 0.5))+
          geom_point()
      
      #scatterplot vs each other
        compare_title = paste(curr_plot_title,'vs',card_plot_title, sep = " ", collapse = NULL)
        
      ggplot(corr_df,                    # Change colors of lines by region
             aes(x = Open,
                 y = Price_USD,
                 col = RegionId)) +
        xlab('Price of Cryptocurrency')+
        ylab('Price of Sold Graphics Card [USD]')+
        ggtitle(compare_title)+
        theme(plot.title = element_text(hjust = 0.5))+
        geom_point()
    
      #scatterplot vs each other
      ggplot(corr_df,                    # Change colors of lines by merchant
             aes(x = Open,
                 y = Price_USD,
                 col = Merchant)) +
        xlab('Price of Cryptocurrency')+
        ylab('Price of Sold Graphics Card [USD]')+
        ggtitle(compare_title)+
        theme(plot.title = element_text(hjust = 0.5))+
        geom_point()
    
    ##   
      ggplot(corr_df, aes(x = Open, y = Price_USD)) +
        geom_point() +
        stat_smooth(method = "lm") +
        xlab('Price of Cryptocurrency')+
        ylab('Price of Sold Graphics Card [USD]')+
        ggtitle('General Mixed Model on GPU Seller')+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_wrap(~ Merchant, ncol = 5)
      
      #fixed model
      m1.gls <- gls(Price_USD~Open, data = corr_df, method = "REML",na.action=na.omit)
      
      #alternative models
      m1_all.lme4<- lmer(Price_USD~Open*RegionId*Merchant + (1|RegionId), data = corr_df, REML = TRUE)
      m1_nocovar.lme4<- lmer(Price_USD~Open+RegionId+Merchant + (1|RegionId), data = corr_df, REML = TRUE)
      m1.d.lme4 <- lmer(Price_USD~Open+RegionId + (1|RegionId), data = corr_df, REML = TRUE)
      m1.c.lme4 <- lmer(Price_USD~Open+Merchant + (1|Merchant), data = corr_df, REML = TRUE)
      m1.b.lme4 <- lmer(Price_USD~Open + (1|RegionId), data = corr_df, REML = TRUE)
      m1.a.lme4 <- lmer(Price_USD~Open + (1|Merchant), data = corr_df, REML = TRUE)
    
      #AIC comparison of FM and alt
      AIC(m1.gls,m1_all.lme4,m1_nocovar.lme4,m1.d.lme4,m1.c.lme4,m1.b.lme4,m1.a.lme4)
      
      
      #Narrow down plausible models
      m1.a.lme4_ml <- lmer(Price_USD~Open+ (1|Merchant), data = corr_df, REML = FALSE)
      m1.b.lme4_ml <- lmer(Price_USD~Open + (1|RegionId), data = corr_df, REML = FALSE)
      m1.c.lme4_ml <- lmer(Price_USD~Open+Merchant +(1|Merchant), data = corr_df, REML = FALSE)
      
      anova(m1.a.lme4_ml,m1.b.lme4_ml,m1.c.lme4_ml)
      anova(m1.a.lme4_ml)
      anova(m1.b.lme4_ml)
      anova(m1.c.lme4_ml)
      
      
      #use m1.a.lme4, which is gpu price vs crypto price with a random factor of merchant.
      # I chose this due to simplicity of model and reliability of fitting. 
      #My goal is primarily applicability of the model to a variety of cryptocurrencies 
        #and gpu cards, so model simplicity is important
      
    
      #use m1, with random intercept on region
      m1.lme4 <- lmer(Price_USD~Open + (1|Merchant), data = corr_df, REML = TRUE)
      summary(m1.lme4)
      resid_panel(m1.lme4, plots = "all")
      ggqqplot(resid(m1.lme4))
      
      #test for normality
      shapiro.test(resid(m1.lme4))
      p_norm = shapiro.test(resid(m1.lme4))$p.value
      
            #didn't work
      #       
      # ## Box Cox ##
      #       #testing times and dates
      #       corr_df_numdate = corr_df
      #       corr_df_numdate$TimeId = as.numeric(corr_df_numdate$TimeId)
      #       #redo all factors as numbers
      #       corr_df_numdate$Merchant = as.numeric(corr_df_numdate$Merchant)
      #       str(corr_df_numdate)
      #       
      #       m2 <-lm(Price_USD ~ Open, data = corr_df_numdate)
      # 
      #       boxcox.list <- boxcox(m2, optimize = TRUE, plotit = TRUE)
      #       lambda = boxcox.list$lambda
      #       plot(boxcox.list, plot.type = "Q-Q Plots", same.window = TRUE)
      #       
      #       m3 <- lm(((Price_USD^lambda-1)/lambda) ~ Open, data = corr_df_numdate)
      #       ggqqplot(resid(m3))
      #       
      #       m3.lme4 <- lmer(((Price_USD^lambda-1)/lambda) ~ Open + (1|Merchant), data = corr_df_numdate, REML = TRUE)
      #       ggqqplot(resid(m3.lme4))
    
            
    ## Ignoring non-normality and fitting anyways because I have no real alternative
            #type of fit
            m.lme4 <- lmer(Price_USD~Open + (1|Merchant), data = corr_df, REML = TRUE)
            m.lm <- gls(Price_USD~Open, data = corr_df, method = "REML",na.action=na.omit)
    
            
            
            #get the regression coefficient
            reg_coeff <- summary(m.lme4)$coefficients[2,1]
            p_val <- summary(m.lme4)$coefficients[2,5]
            
            iter_df[nrow(iter_df) + 1,] = c(nrow(iter_df) + 1,i,card_plot_title,j,
                                            curr_plot_title,reg_coeff,p_val)
            #c('Iteration', 'CardId', 'Card_Name', 'CryptoID', 'Crypto_Name',
            #'Regression_Coeff','P_value')
            
            # #plotting fit
            # corr_df$lmer_fit <- predict(m.lme4)   #Add model fits to dataframe
            # corr_df$lm_fit <- predict(m.lm)   #Add model fits to dataframe
            # 
           # ggplot(corr_df,aes(x = Open,y = Price_USD, col=Merchant)) +
           #    geom_line(aes(y=lmer_fit), size=1) +
           #    geom_point(alpha = 0.5) +
           #    xlab('Price of Cryptocurrency')+
           #    ylab('Price of Sold Graphics Card [USD]')+
           #    ggtitle(compare_title)+
           #    theme(plot.title = element_text(hjust = 0.5))+
           #    theme_bw()
           # 
           # ggplot(corr_df,aes(x = Open,y = Price_USD)) +
           #   geom_point() +
           #   stat_smooth(method = "lm",formula = 'y ~ x') +
           #   xlab('Price of Cryptocurrency')+
           #   ylab('Price of Sold Graphics Card [USD]')+
           #   ggtitle(compare_title)+
           #   theme(plot.title = element_text(hjust = 0.5))+
           #   theme_bw()
            


        
        
        
        