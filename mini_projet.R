data <- read.csv("Cardiotocographic.csv")


head(data)

# data to frame and also change Nsp to numeric varbale 

#mydata$NSP <- as.factor(data$NSP)
#mydata$NSP <- as.numeric(data$NSP)


#
library(nnet)
# Since we are going to use Academic as the reference group, we need relevel the group.
#data$NSP <- relevel(as.factor(data$NSP ))
#data$NSP <- as.factor(data$NSP)
#levels(data$NSP)
data <- as.data.frame(data)
data <- na.omit(data)
sum(duplicated(data)) 
data <- unique(data)
# normalsie data 
data[, 1:(ncol(data)-1)] <- scale(data[, 1:(ncol(data)-1)])

install.packages("dplyr")



set.seed(123)
# Split the data set into a training set (80%) and a testing set (20%)
train_index <- sample(nrow(data), 0.8*nrow(data))
train <- data[train_index,]
test <- data[-train_index,]
head(train)
train <- as.data.frame(train)
test <- as.data.frame(test)

require(nnet)
# Fit a multinomial logistic regression model to the training set
model <- nnet::multinom(NSP ~ . , data=train)
summary(model)

# Check the Z-score for the model (wald Z)
z <- summary(model)$coefficients/summary(model)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


# test anova and wald test 
#install.packages("car")
#require(car)
# Perform the ANOVA analysis
#car::Anova(model, type = 3, test.statistic = "Wald", singular.ok = TRUE)"



#summary_model <- summary(model)
# Print the Wald test results for each predictor
#print(summary_model$coefficients[, c("Estimate", "Std. Error", "z value", "Pr(>|z|)")])

# Perform a Wald test for a group of predictors
#wald_test <- coef(summary_model) %*% solve(vcov(model)) %*% t(coef(summary_model))

# Print the Wald test results for the group of predictors
#print(wald_test)

exp(coef(model))

# Make predictions on the testing set
predicted_classes <- predict(model, newdata=test)

#Confusion Matrix & Misclassification Error - Training Data
p <- predict(model,test)
head(p)
head(test$NSP)

tab <- table(p,test$NSP)
tab
acc=sum(diag(tab))/sum(tab)
acc
1-acc


# Create a confusion matrix
confusion_matrix <- table(test$NSP, predicted_classes)
err_rate <-(confusion_matrix[2,1]+confusion_matrix[1,2])/sum(confusion_matrix)
cat("Confusion Matrix:\n")
print(confusion_matrix)
cat("error rate:\n")
print(err_rate)

# Calculate precision, recall, and F1-score for each class
precision <- diag(confusion_matrix)/colSums(confusion_matrix)
recall <- diag(confusion_matrix)/rowSums(confusion_matrix)
f1_score <- 2*precision*recall/(precision + recall)

# Calculate overall accuracy
overall_accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)

# Print the results

cat("\n")
cat(sprintf("Precision: %s\n", paste(round(precision, 3), collapse = ", ")))
cat(sprintf("Recall: %s\n", paste(round(recall, 3), collapse = ", ")))
cat(sprintf("F1-Score: %s\n", paste(round(f1_score, 3), collapse = ", ")))
cat(sprintf("Overall Accuracy: %s\n", round(overall_accuracy, 3)))

print(model)

# Use the backward elimination method to select the most relevant variables
model_backward <- step(model, direction = "backward") 
model_backward


exp(coef(model_backward))

# Make predictions on the testing set
predicted_classes <- predict(model_backward, newdata=test)

#Confusion Matrix & Misclassification Error - Training Data
p <- predict(model_backward,test)
head(p)
head(test$NSP)

tab <- table(p,test$NSP)
tab
acc=sum(diag(tab))/sum(tab)
acc
1-acc

#install.packages("lmtest")
"library(lmtest)
lrtest(model) 
lrtest(model_backward) "
# test anova and wald test 
# install.packages("car")
#library(car)
# Perform the ANOVA analysis
#Anova(model_backward, type = 3, test.statistic = "Wald", singular.ok = TRUE)
#install.packages("lmtest")
#library(lmtest)
#lrtest(model) 
#lrtest(model_backward) 