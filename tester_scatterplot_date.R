rm(list = ls())   #clear current memory
graphics.off()    #close all graphics windows
cat("\014")       #clear the console

library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)

code_1000 <-
  as.data.frame(cbind(
    c("3", "3", "7", "7", "7", "7", "2", "2", "4", "4"),
    c("344", "344", "73", "73", "71", "72", "21", "27", "42", "43"),
    c("9-02-2017", "10-01-2016","9-02-2014", "25-03-2015", "9-02-2017",
      "10-06-2017", "8-04-2017", "25-08-2016", "07-08-2017", "15-09-2016"
    )
  ))
names(code_1000) <- c("number", "code", "date")

code_1000$date <- lubridate::dmy(as.character(code_1000$date))

qplot(
  data = code_1000,
  x = date,
  y = code,
  geom = c("point"),
  na.rm = TRUE,
  xlab = "Emission date",
  ylab = "Code",
  size = 1,
  col = 2
) + theme_bw() + theme(legend.position = "none") + scale_x_date(
  date_breaks = "1 year",
  date_minor_breaks = "1 month",
  labels = date_format("%m-%Y")
)