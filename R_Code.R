#Load necessary libraries
library(randomForest)      #Random forest 
library(e1071)             #Support Vector Machine
library(corrplot)          #Correlation Plot
library(ggplot2)

#Load the train and test data
train_data <- read.csv(file = "C:\\Users\\vishn\\OneDrive - University of Bradford\\Documents\\Sem 2\\Artificial Intelligence and Data Science\\Assignment\\Data\\RegressionData-20230426T195340Z-001\\RegressionData\\msc_training_dataset.csv")
test_data <- read.csv(file = "C:\\Users\\vishn\\OneDrive - University of Bradford\\Documents\\Sem 2\\Artificial Intelligence and Data Science\\Assignment\\Data\\RegressionData-20230426T195340Z-001\\RegressionData\\msc_testing_dataset.csv")

#Descriptive Statistics
head(train_data)
str(train_data)
summary(train_data)

#Histogram of the target "Price"
hist(train_data$price, 
     col = "#69b3a2", 
     border = "white", 
     main = "Histogram of Price", 
     xlab = "Price", 
     ylab = "Frequency")

#Calculate and plot the correlation between the features
cor_matrix <- cor(train_data)          #Calculate correlation between features
corrplot(cor_matrix,
         col = COL2("BrBG"),
         method = "color", 
         tl.col = "black", 
         addCoef.col = "black", 
         number.cex = 0.7)             #Plot the correlation heatmap


#Create Random Forest and Support Vector Machine Regression models
rf_model <- randomForest(price ~., data = train_data)     #Random Forest
rf_pred <- predict(rf_model, newdata = test_data)         #Make predictions on the test data

svm_model <- svm(price ~., data = train_data)             #Support Vector Machine
svm_pred <- predict(svm_model, newdata = test_data)       #Make predictions on the test data


#Plot the feature importance
imp <- importance(rf_model)                                            #Calculate feature importance
imp_df <- data.frame(feature = rownames(imp), importance = imp[ ,1])   #Convert into dataframe
ggplot(imp_df, 
       aes(x = reorder(feature, importance), y = importance)) + 
       geom_bar(stat = "identity", fill = "#69b3a2") + 
       coord_flip() + 
       labs(x = "", y = "Importance")                                  #Plot feature importance


#Evaluate the model using RMSE and R-Squared
rf_rmse <- sqrt(mean((rf_pred - test_data$price)^2))      #RMSE for the random forest model
rf_rsquared <- cor(rf_pred, test_data$price)^2            #R-Squared value for random forest

svm_rmse <- sqrt(mean((svm_pred - test_data$price)^2))    #RMSE for SVM model
svm_rsquared <- cor(svm_pred, test_data$price)^2          #R-Squared value for SVM

summary(svm_model)

#Evaluate the predicted house prices
Predicted_prices <- as.data.frame(svm_pred)               #Convert into dataframe
head(Predicted_prices)
head(test_data$price)

#Plot predicted vs actual house price
ggplot(test_data, aes(x=price, y=svm_pred)) +
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) +
  xlab("Actual House Price") + 
  ylab("Predicted House Price") +
  ggtitle("Actual vs Predicted House Prices")


#Create a data frame with predicted prices and actual prices
predicted_df <- data.frame(predicted_price = Predicted_prices)   

results <- cbind(predicted_df, actual_price = test_data$price)

# change column names
colnames(results) <- c("Predicted Price", "Actual Price")
head(results)
