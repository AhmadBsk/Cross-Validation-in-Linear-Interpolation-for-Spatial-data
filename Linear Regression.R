# Multiple Linear Regression

library(readxl)
library(sp)
library(geoR)
library(ggplot2)
library(scatterplot3d)
library(raster)

setwd("C:/Users/ACER/Desktop/cupper data/1401-06-31/SamiroumGeochemistry-6352")
df <- read_xlsx("6352.xlsx",sheet = "Sheet1")
# db <- data.frame(x=df$X_UTM,y=df$Y_UTM,cu=df$Cu)
# d <- data.frame(x=(df$X_UTM-min(df$X_UTM)),y=(df$Y_UTM-min(df$Y_UTM)),cu=df$Cu)
d <- data.frame(x=round(df$X_UTM),y=round(df$Y_UTM),cu=df$Cu)

grid_x <- seq(min(d$x),max(d$x),l = 100) ## make vector
grid_y <- seq(min(d$y),max(d$y),l = 100) ## make vector
pred_grid <- expand.grid(x = grid_x, y = grid_y)

## Multiple Linear Regression
fit_1 <- lm(cu ~ x+y, d)
summary(fit_1)
par(mfrow = c(2, 2))
plot(fit_1)
pred_grid$Volume1 <-predict(fit_1, new = pred_grid)

pred <- data.frame(x=pred_grid$x,y=pred_grid$y,z=pred_grid$Volume1)
coordinates(pred) <- ~x+y
gridded(pred) <- TRUE
plot(pred) 

## Interaction Terms
fit_2 <- lm(cu ~ x*y, d)
summary(fit_2)
par(mfrow = c(2, 2))
plot(fit_2)
pred_grid$Volume2 <-predict(fit_2, new = pred_grid)

pred_2 <- data.frame(x=pred_grid$x,y=pred_grid$y,z=pred_grid$Volume2)
coordinates(pred_2) <- ~x+y
gridded(pred_2) <- TRUE
plot(pred_2) 
# setwd("C:/Users/ACER/Desktop/cupper data/1401-06-31/SamiroumGeochemistry-6352/linear prediction")
# png(paste(pred, '.png', sep = ''), width = 600, height = 600, units = "px")
# dev.off()
