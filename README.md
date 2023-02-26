# Cross-Validation-in-Linear-Interpolation-for-Spatial-data
Model-Diagnostics Cross Validation in Interpolation and Geostatistics  for Spatial data

## Data
Copper analysis for sediment samples (Contains: sample-number, x(UTM), y(UTM), Lat, Long, cupper assay)

### Exploratory Data Analysis 
![image](https://user-images.githubusercontent.com/123794462/221370579-7ed11b0a-b361-438a-9569-a3e1316012a4.png)


## 1)Cross Validation in Linear Interpolation (Non-geostatistical Interpolation) 

### Load library
```{R}
library(sp)
library(readxl)
library(geoR)
library(ggplot2)
library(raster)
library(mlr)
```
### Set Working Directory and Get Data
```{R}
setwd("C:/Users/ACER/Desktop/cupper data/1401-06-31/SamiroumGeochemistry-6352")
df <- read_xlsx("6352.xlsx",sheet = "Sheet1")
data <- data.frame(x=df$X_UTM ,y=df$Y_UTM ,cu=df$Cu)
```
### Grid data
```{R}
grid_x <- seq(min(data$x),max(data$x), by=1000) ## make vector
grid_y <- seq(min(data$y),max(data$y), by=10) ## make vector
pred_grid <- expand.grid(x = grid_x, y = grid_y)
```
### Linear Regression
```{R}
fit_1 <- lm(cu ~ x+y, data)
summary(fit_1)
par(mfrow = c(2, 2))
plot(fit_1)
pred_grid$Volume1 <-predict(fit_1, new = pred_grid)

pred <- data.frame(x=pred_grid$x,y=pred_grid$y,z=pred_grid$Volume1)
coordinates(pred) <- ~x+y
gridded(pred) <- TRUE
plot(pred) 
```

![image](https://user-images.githubusercontent.com/123794462/221396500-07f2d5fb-c0da-4407-8861-98142014434d.png)


```{R}
## Interaction Terms
fit_2 <- lm(cu ~ x*y, data)
summary(fit_2)
par(mfrow = c(2, 2))
plot(fit_2)
pred_grid$Volume2 <-predict(fit_2, new = pred_grid)
pred_2 <- data.frame(x=pred_grid$x,y=pred_grid$y,z=pred_grid$Volume2)
coordinates(pred_2) <- ~x+y
gridded(pred_2) <- TRUE
plot(pred_2) 
```
![image](https://user-images.githubusercontent.com/123794462/221396526-b4798370-0c03-4240-a610-445d83ccea6e.png)

### k-fold cross validation Option and splitter
```{R}
k = 5
data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k
prediction <- testsetCopy <- data.frame()
```

###  Generates (train, test) splits and prediction with linear model
```{R}
for(i in 1:k){
  trainset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  # linear model
  model <- lm(cu ~ x+y, data = trainset)
  temp <- as.data.frame(predict(model, testset))
  prediction <- rbind(prediction, temp)
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,3]))
}
```
### Combine predictions and observations and made result with Difference(residual)
```{R}
result <- cbind(prediction, testsetCopy[, 1])
names(result) <- c("Predicted", "Actual")
result$Difference <- abs(result$Actual - result$Predicted)
summary(result$Difference)
```
### Calculate the error rate
```{R}
# Sum squared error (SSE)
SSE = sum(result$Difference^2) #44948.32
# Mean absolute error (MAE)
MAE = mean(result$Difference) #5.485954
# Mean squared error (MSE)
MSE = mean(result$Difference^2) #53.765943
# Root mean square error (RMSE)
RMSE = sqrt(mean(result$Difference^2)) #7.3325264
```
