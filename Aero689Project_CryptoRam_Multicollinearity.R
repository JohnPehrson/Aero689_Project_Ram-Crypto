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
library(remotes)
# install_version("boxcoxmix","0.28")
library(boxcoxmix)
library(scales)
library(MuMIn)
require(ggiraph)
require(ggiraphExtra)
require(plyr)

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
        card_id = all_card_ids[52]  #35
        single_card_df <- Price.GPU[Price.GPU$ProdId == card_id,]
        card_info <- Factors.GPU[ Factors.GPU$Id==card_id,]
        card_plot_title = paste(card_info$Processor,card_info$Processor_Manufacturer,'by'
                                ,card_info$GPU_Manufacturer, sep = " ", collapse = NULL)
        card_plot_price = paste(card_info$Processor," Price [USD]", sep = " ", collapse = NULL)
        
        corr_df = single_card_df
        for (i in 1:nrow(Factors.Crypto)) {
          #get all prices of a single currency
            all_curr_ids = Factors.Crypto$Id
            curr_id = all_curr_ids[i]
            single_curr_df <- Price.Crypto[Price.Crypto$CodeId == curr_id,]
            curr_info <- Factors.Crypto[ Factors.Crypto$Id==curr_id,]
            single_curr_use = single_curr_df[,2:3]
            names(single_curr_use)[names(single_curr_use) == 'Open'] <- curr_info$Currency_Name

          #left-join the card with a single cryptocurrency
            corr_df = merge(corr_df, single_curr_use, by = "TimeId")
        }
        corr_df = corr_df[corr_df$TimeId>20170000,] #only get relevent data
        
        
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


    #scatterplot each through time
    ggplot(corr_df,                    # Change colors of lines by region
           aes(x = TimeId,
               y = Price_USD,
               col = Merchant)) +
      xlab('Time [Month,Year]')+
      ylab(card_plot_price)+
      ggtitle(card_plot_title)+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_point()+
      scale_x_date(limits = as.Date(c("2017-03-01", "2018-05-01")))+
      # theme(legend.position = "none")+
            labs(title = "")+
      theme(axis.text=element_text(size=15),
            axis.title=element_text(size=15,face="bold"))

    curr_plot_title = 'Etherum Cryptocurrency'
    ggplot(corr_df,                    # Change colors of lines by region
           aes(x = TimeId,
               y = Ethereum)) +
      xlab('Time [Month,Year]')+
      ylab(card_plot_price)+
      ggtitle(curr_plot_title)+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_point()+
      geom_line()+
      scale_x_date(limits = as.Date(c("2017-03-01", "2018-05-01")))

  #scatterplot vs each other
    compare_title = paste(curr_plot_title,'vs',card_plot_title, sep = " ", collapse = NULL)

  ggplot(corr_df,                    # Change colors of lines by region
         aes(x = Ethereum,
             y = Price_USD,
             col = RegionId)) +
    xlab('Price of Ethereum')+
    ylab(card_plot_price)+
    ggtitle(compare_title)+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_point()

  #scatterplot vs each other
  ggplot(corr_df,                    # Change colors of lines by merchant
         aes(x = Ethereum,
             y = Price_USD,
             col = Merchant)) +
    xlab('Price of Ethereum [USD]')+
    ylab(card_plot_price)+
    # ggtitle(compare_title)+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_point()+
    theme(legend.position = "none")
  

##
  corr_df_plot = corr_df
  corr_df_plot$Merchant =  as.numeric(corr_df_plot$Merchant)
  
  ggplot(corr_df_plot, aes(x = Ethereum, y = Price_USD)) +
    geom_point() +
    stat_smooth(method = "lm") +
    xlab('Price of Ethereum [USD]')+
    ylab(card_plot_price)+
    # ggtitle('General Mixed Model on GPU Seller')+
    theme(plot.title = element_text(hjust = 0.5))+
    facet_wrap(~ Merchant, ncol = 5)
  
      cryptocurrencies_df <- corr_df[, c("Bitcoin", "DASH", "Dogecoin","Ethereum", "Litecoin", "Monero")]
      # #Visualize Multicollinearity in cryptocurrencies
      # pairs.panels(cryptocurrencies_df, method = "pearson",
      #              density = TRUE,
      #              ellipses = TRUE)

      #fixed model comparison to pick which cryptocurrencies to use
      full.model <- lm(Price_USD ~Bitcoin + Ethereum + 
                         Litecoin + DASH + Monero + Dogecoin, data = corr_df)
      empty.model <- lm(Price_USD ~ 1, data = corr_df)
      
      step.model <- stepAIC(empty.model, scope = list(upper=full.model,lower=empty.model),
                            direction = "both", trace = TRUE)
      summary(step.model)  
      
      m1.lm <- lm(Price_USD~Ethereum, data = corr_df)
      m2.lm <- lm(Price_USD~Ethereum+Bitcoin, data = corr_df)
      m3.lm <- lm(Price_USD~Bitcoin+Dogecoin+Ethereum, data = corr_df)
      m4.lm <- lm(Price_USD~Bitcoin+DASH+Dogecoin+Ethereum+Litecoin+Monero, data = corr_df)
      anova(m1.lm,m2.lm,m3.lm,m4.lm)  
      summary(m1.lm)
      summary(m2.lm)
      summary(m3.lm)
      #just using Ethereum is viable for fitting, reduces multicollinearity
      
      #Scaling dollars into a more useable metric:
      corr_df$Price_USD = (corr_df$Price_USD)/1000
      corr_df$Ethereum = (corr_df$Ethereum)/1000
      
      #Picking a model
      m1_all.lme4    <- lmer(Price_USD~Ethereum*RegionId*Merchant + (1|RegionId), data = corr_df, REML = TRUE)
      m1_nocovar.lme4<- lmer(Price_USD~Ethereum+RegionId+Merchant + (1|RegionId), data = corr_df, REML = TRUE)
      m1.d.lme4      <- lmer(Price_USD~Ethereum+RegionId          + (1|RegionId), data = corr_df, REML = TRUE)
      m1.c.lme4      <- lmer(Price_USD~Ethereum+Merchant          + (1|Merchant), data = corr_df, REML = TRUE)
      m1.b.lme4      <- lmer(Price_USD~Ethereum                   + (1|RegionId), data = corr_df, REML = TRUE)
      m1.a.lme4      <- lmer(Price_USD~Ethereum                   + (1|Merchant), data = corr_df, REML = TRUE)

      #AIC comparison of FM and alt
      # AIC(m1.lm,m1_all.lme4,m1_nocovar.lme4,m1.d.lme4,m1.c.lme4,m1.b.lme4,m1.a.lme4)
      
      #Narrow down plausible models
      m1_all.lme4_ml <-  lmer(Price_USD~Ethereum*RegionId*Merchant + (1|RegionId), data = corr_df, REML = FALSE)
      m1.a.lme4_ml <- lmer(Price_USD~Ethereum +                     (1          |Merchant), data = corr_df, REML = FALSE)
      m1.b.lme4_ml <- lmer(Price_USD~Ethereum + Merchant:Ethereum + (1          |Merchant), data = corr_df, REML = FALSE)
      m1.c.lme4_ml <- lmer(Price_USD~Ethereum +                   + (1+Ethereum |Merchant), data = corr_df, REML = FALSE)
      
      # anova(m1.a.lme4_ml,m1.b.lme4_ml,m1.c.lme4_ml)
      # anova(m1_all.lme4_ml)
      anova(m1.c.lme4_ml)
      
      
      #use m1.c.lme4, which is:
      #DV: gpu price
      #IVs: Crypto price (Ethereum), Merchant
      #Random IV: Merchant
      
      # I chose this due to having the simplest model to give the AIC closest to zero.
      # Visual plotting shows some slope-dependence on merchant, so I included it.
      
      corr_df_log = corr_df
      corr_df_log$Price_USD = log(corr_df_log$Price_USD)
      # corr_df_log$Ethereum = log(corr_df_log$Ethereum)
      
      
      m1.lme4 <- lmer(Price_USD~Ethereum +(1+Ethereum|Merchant), data = corr_df_log,
                                      REML = TRUE, control=lmerControl(optCtrl=list(maxfun=100000) ))
      summary(m1.lme4)
      resid_panel(m1.lme4, plots = "all")
      sum_lme4 = summary(m1.lme4)
      coeff_gpu_ethereum = exp(sum_lme4$coefficients[2,1])
       m1.sim <- simulateResiduals(m1.lme4)
      plot(m1.sim)
      
      res <- resid(m1.lme4)      
      ggqqplot(res,xlab = "Ideal Distribution of Residuals",
               ylab = "Distribution of Residuals")
      
      
      
      #test for normality
      shapiro.test(resid(m1.lme4))
      p_norm = shapiro.test(resid(m1.lme4))$p.value

      # ## Linear Box Cox ##
      #     #testing times and dates
      #     corr_df_numdate = corr_df
      #     corr_df_numdate$TimeId = as.numeric(corr_df_numdate$TimeId)
      #     #redo all factors as numbers
      #     corr_df_numdate$Merchant = as.numeric(corr_df_numdate$Merchant)
      #     str(corr_df_numdate)
      # 
      #     m2 <-lm(Price_USD ~ Ethereum, data = corr_df_numdate)
      # 
      #     boxcox.list <- boxcox(m2, optimize = TRUE, plotit = TRUE)
      #     lambda = boxcox.list$lambda
      #     plot(boxcox.list, plot.type = "Q-Q Plots", same.window = TRUE)
      # 
      #     m3 <- lm(((Price_USD^lambda-1)/lambda) ~ Ethereum, data = corr_df_numdate)
      #     ggqqplot(resid(m3))
      #     #does not work if I try and extend it to a mixed model

#       ## Iterative box cox
#             corr_df_mod = corr_df
#             corr_df_mod$Price_USD = log10(corr_df_mod$Price_USD)
# 
#             m3.lme4 <- lmer(Price_USD ~ Ethereum +(1|Merchant), data = corr_df_mod, REML = TRUE)
#             ggqqplot(resid(m3.lme4))
#             
#             
#             unique_merchs = unique(corr_df_mod$Merchant)
#             use_merchs = length(unique_merchs)
#             i = 4
#             # for (i in 1:use_merchs) {
#               
#             corr_df_mod_single = corr_df_mod[corr_df_mod$Merchant== unique_merchs[i],]
#             m1 <-lm(Price_USD ~ Ethereum, data = corr_df_mod_single)
#             ggqqplot(resid(m1))
#             p_norm = shapiro.test(resid(m1))$p.value
#             p_norm
#             # }
#             
#             
#             # m4.lme4 <- glmer(Price_USD~Ethereum+(1|Merchant),
#             #               family=Gamma(link="log"), data=corr_df)
#             # ggqqplot(resid(m4.lme4))
#             
#               
    ## Ignoring non-normality and fitting anyways because I have no real alternative
            #get the regression coefficient
            reg_coeff <- exp(sum_lme4$coefficients[2,1])
            p_val <- summary(m1.lme4)$coefficients[2,5]
            r2_fixed <- r.squaredGLMM(m1.lme4)[1]
            r2_fixedandrandom <- r.squaredGLMM(m1.lme4)[2]
            
            #plotting fit
            corr_df_log$lmer_fit <- predict(m1.lme4)   #Add model fits to dataframe

           ggplot(corr_df_log,aes(x = Ethereum,y = Price_USD, col=Merchant)) +
              geom_line(aes(y=lmer_fit), size=1) +
              geom_point(alpha = 1,size = 0.1, stroke = 0, shape = 16) +
              xlab('Price of Ethereum in Thousands of USD')+
              ylab('Log(GPU Price/$1000)')+
              # ggtitle(compare_title)+
              theme(plot.title = element_text(hjust = 0.5))+
              theme_bw()

           
         #Predicting fit
           merch_val = as.factor('NewMerch')
           eth_val = corr_df_log$Ethereum
           df_1_pred = expand.grid(eth_val,merch_val)
           names(df_1_pred)[names(df_1_pred) == 'Var1'] <- 'Ethereum'
           names(df_1_pred)[names(df_1_pred) == 'Var2'] <- 'Merchant'
           
           response_predict <- predict(m1.lme4, newdata = df_1_pred, type = "response",allow.new.levels = TRUE)
           response_confidence <- predict(m1.lme4, newdata = df_1_pred,  interval = "confidence",allow.new.levels = TRUE)
           corr_df_log$Response <-response_predict   #Add model fits to dataframe
           corr_df_log$Confidence <-response_confidence   #Add model fits to dataframe
         
           #ci.style = c("ribbon", "errorbar", "dash", "dot"),
           pred <- plot(ggpredict(m1.lme4,"Ethereum",add.data = TRUE),line.size = 2)
           
           pred+geom_point(data = corr_df_log,aes(x = Ethereum,y = Price_USD))+
             xlab('Price of Ethereum in Thousands of USD')+
             ylab('Log(GPU Price/$1000)')+ 
             labs(title = "")+
             theme(axis.text=element_text(size=15),
                                    axis.title=element_text(size=15,face="bold"))
             
             
           # ggplot(corr_df_log,aes(x = Ethereum,y = Price_USD)) +
           # geom_point() +
           # xlab('Price of Ethereum in Thousands of USD')+
           # ylab('Log(GPU Price/$1000)')+
           # # ggtitle(compare_title)+
           # theme(plot.title = element_text(hjust = 0.5))+
           # theme_bw()



        
        
        
        