rm(list = ls())   #clear current memory
graphics.off()    #close all graphics windows
cat("\014")       #clear the console

library(MASS)
library(faraway)

gala_df <- gala
gala_model = lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
boxcox(gala_model, lambda = seq(-0.25, 0.75, by = 0.05), plotit = TRUE)

gala_model_cox = lm((((Species ^ 0.3) - 1) / 0.3) ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)

plot(fitted(gala_model_cox), resid(gala_model_cox), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)



