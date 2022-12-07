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
library(MuMIn)
library(writexl)


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
  iter_df <- data.frame(matrix(ncol = 11, nrow = 0))

#provide column names
colnames(iter_df) <- c('CardId', 'Card_Name','Manufacturer', 'Normality_P_value',
                  'Regression_Coeff','P_value','NumCards','Memory_Capacity','Memory_Type',
                  'R2_Fixed_Effects','R2_Fixed_and_Random_Effects')

# i = 9

for (i in 1:gpu_count) {
      #get all cards of the same type
        all_card_ids = Factors.GPU$Id
        card_id = all_card_ids[i] 
        single_card_df <- Price.GPU[Price.GPU$ProdId == card_id,]
        card_info <- Factors.GPU[ Factors.GPU$Id==card_id,]
        card_name = paste(card_info$Processor,card_info$Processor_Manufacturer, sep = " ", collapse = NULL)
        memory_cap = Factors.GPU$Memory_Capacity[i]
        mem_type = Factors.GPU$Memory_Type[i]
  
        corr_df = single_card_df
        for (j in 1:nrow(Factors.Crypto)) {
          #get all prices of a single currency
          all_curr_ids = Factors.Crypto$Id
          curr_id = all_curr_ids[j]
          single_curr_df <- Price.Crypto[Price.Crypto$CodeId == curr_id,]
          curr_info <- Factors.Crypto[ Factors.Crypto$Id==curr_id,]
          single_curr_use = single_curr_df[,2:3]
          names(single_curr_use)[names(single_curr_use) == 'Open'] <- curr_info$Currency_Name
          
          #left-join the card with a single cryptocurrency
          corr_df = merge(corr_df, single_curr_use, by = "TimeId")
        }
        corr_df = corr_df[corr_df$TimeId>20170500,] #only get relevent data
    
        #make factors
        names(Factors.Merchant)[1] <- 'MerchantId'
        corr_df = merge(corr_df, Factors.Merchant, by = "MerchantId")
        corr_df$RegionId <- factor(corr_df$RegionId,
                                   levels = c(1, 3, 4, 9, 10, 11),
                                   labels = c("AUD", "CAD", "EUR","NZD","GBP","USD"))
        corr_df <- corr_df[,2:ncol(corr_df)]
        corr_df$Merchant <- as.factor(corr_df$Merchant)   # Convert character column to factor
        corr_df = toDateTime(corr_df)
        
        
        #Scaling dollars into a more useable metric:
        corr_df$Price_USD = (corr_df$Price_USD)/1000
        corr_df$Ethereum = (corr_df$Ethereum)/1000
        corr_df_log = corr_df
        corr_df_log$Price_USD = log(corr_df_log$Price_USD)

      #Model
        num_unique_merchants = length(unique(corr_df_log$Merchant))
      if (num_unique_merchants>1) {

        m1.lme4 <- lmer(Price_USD~Ethereum +(1+Ethereum|Merchant), data = corr_df_log,
                        REML = TRUE, control=lmerControl(optCtrl=list(maxfun=100000) ))        

          #get the regression coefficient
          sum_lme4 = summary(m1.lme4)
          reg_coeff <- exp(sum_lme4$coefficients[2,1])-1
          p_val     <- summary(m1.lme4)$coefficients[2,5]
          r2_fixed  <- r.squaredGLMM(m1.lme4)[1]
          r2_fixedandrandom <- r.squaredGLMM(m1.lme4)[2]
        
      } else {
        #no random effects
        m1.lme4 <- lm(Price_USD~Ethereum, data = corr_df_log)
          #get the regression coefficient
          sum_lme4 = summary(m1.lme4)
          reg_coeff <- exp(sum_lme4$coefficients[2,1])-1
          p_val <- summary(m1.lme4)$coefficients[2,4]
          r2_fixed = sum_lme4$adj.r.squared 
          r2_fixedandrandom = sum_lme4$adj.r.squared 
      } 
      p_norm = shapiro.test(resid(m1.lme4))$p.value

            #get the regression coefficient
            n_cards = nrow(corr_df_log)
            manu_name = card_info$GPU_Manufacturer
            
            iter_df[nrow(iter_df) + 1,] = c(i,card_name,manu_name,p_norm,reg_coeff,p_val,n_cards,
                                            memory_cap,mem_type,r2_fixed,r2_fixedandrandom)

}

#total unique number of cards
length(unique(iter_df$Card_Name))

#filtering out all cards with a non-significant p value for the regression coefficient 
filt_p_vals =  as.numeric(iter_df$P_value)<0.025
iter_df_filt1 = iter_df[filt_p_vals,]
iter_df_filt1$Regression_Coeff <- as.numeric(iter_df_filt1$Regression_Coeff)
iter_df_filt1$R2_Fixed_Effects <- as.numeric(iter_df_filt1$R2_Fixed_Effects)
iter_df_filt1$R2_Fixed_and_Random_Effects <- as.numeric(iter_df_filt1$R2_Fixed_and_Random_Effects)
iter_df_filt1$P_value                     <- as.numeric(iter_df_filt1$P_value                    )
iter_df_filt1$Normality_P_value           <- as.numeric(iter_df_filt1$Normality_P_value          )


#total unique number of cards
length(unique(iter_df_filt1$Card_Name))

filt_rows = (strtoi(iter_df_filt1$NumCards, base=0L)>600)|grepl("980", iter_df_filt1$Card_Name, fixed = TRUE)|grepl("1080", iter_df_filt1$Card_Name, fixed = TRUE)
iter_df_filt = iter_df_filt1[filt_rows,]

#total unique number of cards
length(unique(iter_df_filt$Card_Name))



ggplot(iter_df_filt, aes(x = Card_Name, y = Regression_Coeff)) +
  geom_boxplot()

level_order <- c('HBM2', 'GDDR5','GDDR5X') 
ggplot(iter_df_filt, aes(x = Memory_Type, y = Regression_Coeff)) +
  geom_boxplot()

level_order <- c('4', '6', '8','11','16','32') 
ggplot(iter_df_filt, aes(x = factor(Memory_Capacity, level = level_order), y = Regression_Coeff)) +
  geom_boxplot()

        
sp<-ggplot(iter_df_filt, aes(x=Card_Name, y=Regression_Coeff))+ 
  geom_boxplot()+
  theme(text = element_text(size=15),
        axis.text.x = element_text(face="bold", color="#000000", 
                                   size=12, angle=25),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=12, angle=0))+
  ylab("Correlation Coefficient") + xlab("GPU Card")+
  geom_hline(yintercept=0, linetype='dotted', col = 'black')
  
sp

sp1<-ggplot(iter_df_filt, aes(x=Card_Name, y=R2_Fixed_and_Random_Effects))+
  geom_point(size = 2)+
      theme(text = element_text(size=15),
        axis.text.x = element_text(face="bold", color="#000000", 
                                   size=12, angle=25),
        axis.text.y = element_text(face="bold", color="#000000", 
                                   size=12, angle=0))+
  ylab(parse(text='R^2')) + xlab("GPU Card")+
  theme(legend.position = "none")+
  geom_hline(yintercept=0, linetype='dotted', col = 'black')

sp1

#Table making
report_table_df = iter_df_filt1[,c(2,3,5,6,7,8,10,11)]
report_table_df$P_value =   rep('<0.005',length(report_table_df$P_value))
report_table_df$Regression_Coeff = round(report_table_df$Regression_Coeff, digits = 3)
report_table_df$R2_Fixed_Effects = round(report_table_df$R2_Fixed_Effects, digits = 3)
report_table_df$R2_Fixed_and_Random_Effects = round(report_table_df$R2_Fixed_and_Random_Effects, digits = 3)

library("writexl")
save_filepath = "GPU_regression.xlsx"
write_xlsx(report_table_df,save_filepath)
