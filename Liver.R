library(readxl)
indian_liver_patient <- read_excel("C:/Users/Dhanush/OneDrive/Desktop/ML for Business/HW-2/indian_liver_patient.xlsx")
View(indian_liver_patient)
##dummy variable
indian_liver_patient$Gender <- ifelse(indian_liver_patient$Gender == "Male", 1, 0)

# View the first few rows of the dataset to confirm changes
head(indian_liver_patient)
str(indian_liver_patient)
#==========================================================================#



continuous_vars <- indian_liver_patient[c("Total_Bilirubin", "Direct_Bilirubin", "Alkaline_Phosphotase", "Alamine_Aminotransferase", "Aspartate_Aminotransferase", "Total_Protiens", "Albumin", "Albumin_and_Globulin_Ratio", "Disease")]
library(ggplot2)
library(GGally)
install.packages("GGally")
# Use ggpairs to create the scatterplot matrix
ggpairs(continuous_vars, mapping = ggplot2::aes(color = as.factor(Disease)))

###################Logit############################################
logitmodel <- glm(Disease ~ ., data = indian_liver_patient, family = "binomial")
summary(logitmodel)
###########################NN###########################
library(neuralnet)
set.seed(7)
Liver_index <- sample(nrow(indian_liver_patient), 0.7 * nrow(indian_liver_patient),replace = FALSE)
Liver_train <- indian_liver_patient [Liver_index, ]
Liver_test <- indian_liver_patient [-Liver_index, ]
sum(is.na(Liver_train))
Liver_train <- na.omit(Liver_train)
Livernet <- neuralnet(Disease ~ ., Liver_train, hidden=1, lifesign="minimal", 
                     linear.output=FALSE, threshold=0.01)

plot(Livernet)
temp_test <- subset(Liver_test, select = c("Total_Bilirubin", "Direct_Bilirubin", "Alkaline_Phosphotase", "Alamine_Aminotransferase", "Aspartate_Aminotransferase", "Total_Protiens", "Albumin", "Albumin_and_Globulin_Ratio","Age", "Gender"))
head(temp_test)

Livernet.results <- compute(Livernet, temp_test)
results <- data.frame(actual=Liver_test$Disease, prediction=Livernet.results$net.result)
results[1:10,]
results$prediction <- round(results$prediction)
results [1:10,]
actual <- as.factor(Liver_test$Disease)
neural_result <- as.factor(round(results$prediction))
#build confusion matrix
predicttable = table(neural_result, actual)
predicttable
sum(diag(predicttable))/sum(predicttable)
#############################################NNN-2##########################
library(neuralnet)
set.seed(7)
Liver_index <- sample(nrow(indian_liver_patient), 0.7 * nrow(indian_liver_patient),replace = FALSE)
Liver_train <- indian_liver_patient [Liver_index, ]
Liver_test <- indian_liver_patient [-Liver_index, ]
sum(is.na(Liver_train))
Liver_train <- na.omit(Liver_train)
Livernet <- neuralnet(Disease ~ ., Liver_train, hidden=2, lifesign="minimal", 
                      linear.output=FALSE, threshold=0.01)

plot(Livernet)
temp_test <- subset(Liver_test, select = c("Total_Bilirubin", "Direct_Bilirubin", "Alkaline_Phosphotase", "Alamine_Aminotransferase", "Aspartate_Aminotransferase", "Total_Protiens", "Albumin", "Albumin_and_Globulin_Ratio","Age", "Gender"))
head(temp_test)

Livernet.results <- compute(Livernet, temp_test)
results <- data.frame(actual=Liver_test$Disease, prediction=Livernet.results$net.result)
results[1:10,]
results$prediction <- round(results$prediction)
results [1:10,]
actual <- as.factor(Liver_test$Disease)
neural_result <- as.factor(round(results$prediction))
#build confusion matrix
predicttable = table(neural_result, actual)
predicttable
sum(diag(predicttable))/sum(predicttable)
###############################################NNN-3#################################
library(neuralnet)
set.seed(7)
Liver_index <- sample(nrow(indian_liver_patient), 0.7 * nrow(indian_liver_patient),replace = FALSE)
Liver_train <- indian_liver_patient [Liver_index, ]
Liver_test <- indian_liver_patient [-Liver_index, ]
sum(is.na(Liver_train))
Liver_train <- na.omit(Liver_train)
Livernet <- neuralnet(Disease ~ ., Liver_train, hidden=3, lifesign="minimal", 
                      linear.output=FALSE, threshold=0.01)

plot(Livernet)
temp_test <- subset(Liver_test, select = c("Total_Bilirubin", "Direct_Bilirubin", "Alkaline_Phosphotase", "Alamine_Aminotransferase", "Aspartate_Aminotransferase", "Total_Protiens", "Albumin", "Albumin_and_Globulin_Ratio","Age", "Gender"))
head(temp_test)

Livernet.results <- compute(Livernet, temp_test)
results <- data.frame(actual=Liver_test$Disease, prediction=Livernet.results$net.result)
results[1:10,]
results$prediction <- round(results$prediction)
results [1:10,]
actual <- as.factor(Liver_test$Disease)
neural_result <- as.factor(round(results$prediction))
#build confusion matrix
predicttable = table(neural_result, actual)
predicttable
sum(diag(predicttable))/sum(predicttable)
#################################################Random Forest########################################################
# Install and load the randomForest package
#install.packages("randomForest")
library(randomForest)

# Set seed for reproducibility
set.seed(7)

# Create the Random Forest model
# Use the 'Disease' column as the dependent variable
# Ensure there are no missing values in the dataset before running the model
Liver_train <- na.omit(Liver_train)

rf_model <- randomForest(Disease ~ ., data = Liver_train, ntree = 500, mtry = 3, importance = TRUE)

# View model summary
print(rf_model)

# Predict on the test data
rf_pred <- predict(rf_model, Liver_test)

# Convert predictions to binary (if necessary)
rf_pred <- round(rf_pred)

# Create confusion matrix
rf_confusion <- table(Predicted = rf_pred, Actual = Liver_test$Disease)
print(rf_confusion)

# Calculate accuracy
rf_accuracy <- sum(diag(rf_confusion)) / sum(rf_confusion)
print(paste("Random Forest Model Accuracy:", round(rf_accuracy * 100, 2), "%"))

# Variable importance plot
importance(rf_model)
varImpPlot(rf_model)
##########################################################################################