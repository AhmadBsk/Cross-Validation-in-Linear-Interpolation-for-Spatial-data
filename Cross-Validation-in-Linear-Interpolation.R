library(sp)
library(readxl)
library(ggplot2)
library(mlr)

setwd("C:/Users/ACER/Desktop/cupper data/1401-06-31/SamiroumGeochemistry-6352")
df <- read_xlsx("6352.xlsx",sheet = "Sheet1")
# db <- data.frame(x=df$X_UTM,y=df$Y_UTM,cu=df$Cu)
data <- data.frame(x=df$X_UTM ,y=df$Y_UTM ,cu=df$Cu)

k = 5
data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k

prediction <- testsetCopy <- data.frame()

grid_x <- seq(min(data$x),max(data$x), by=1000) ## make vector
grid_y <- seq(min(data$y),max(data$y), by=10) ## make vector
pred_grid <- expand.grid(x = grid_x, y = grid_y)


for(i in 1:k){
  
  trainset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  # linear model
  model <- lm(cu ~ x+y, data = trainset)
  temp <- as.data.frame(predict(model, testset))
  
  prediction <- rbind(prediction, temp)
  
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,3]))
  
}

result <- cbind(prediction, testsetCopy[, 1])
names(result) <- c("Predicted", "Actual")
result$Difference <- abs(result$Actual - result$Predicted)

summary(result$Difference)

# Sum squared error (SSE)
SSE = sum(result$Difference^2) #44948.32

# Mean absolute error (MAE)
MAE = mean(result$Difference) #5.485954

# Mean squared error (MSE)
MSE = mean(result$Difference^2) #53.765943

# Root mean square error (RMSE)
RMSE = sqrt(mean(result$Difference^2)) #7.3325264

