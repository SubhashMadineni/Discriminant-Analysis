#########################################################
## data Mining I HW4
## Subhashchandra Babu Madineni   UBIT = 50373860
## Created on 20 th october
## Edited: 
#########################################################
rm(list = ls())

#setwd('C:/University At Buffalo Fall 2020 Classes/EAS 506-CDA 541Stastical Data Mining/R for STA/homework_4')
library(MMST)
library(MASS)
library(klaR)   #install.packages("MMST")
library(mclust)   #install.packages("mclust")
library(rda)
#########################################################
# Loading th dataset
#########################################################
load("C:/University At Buffalo Fall 2020 Classes/EAS 506-CDA 541Stastical Data Mining/R for STA/homework_4/Diabetes.RData")


#########################################################
# Making pairwise scatter plots
#########################################################

clp = clPairs(Diabetes[,-6],cl=Diabetes$group, main = "Pairwise Scatter plots")
clPairsLegend(0.05, 0.99, class = clp$class,
              col = clp$col,pch = clp$pch)

#########################################################
#  Create a test and training dataset
########################################################
dataset = Diabetes
set.seed(45)
train = sample(1:nrow(Diabetes[,-6]), nrow(Diabetes[,-6])*.70)
training_set = Diabetes[train, ]
test_set= Diabetes[-train, ]
dim(training_set)
dim(test_set)


# Translate the "response" to 0-1 coding, so that 
# We can eventually calculate the missclassification rate.
y_true_train <- as.numeric(training_set$group)-1 #diabetes =0, normal = 1
y_true_test <- as.numeric(test_set$group)-1 #diabetes =0, normal = 1



#########################################################
#  Applying LDA
########################################################

lda.fit <- lda(group~., data = training_set)

lda.pred.train <- predict(lda.fit, newdata = training_set)
y_hat_train <- as.numeric(lda.pred.train$class)-1
lda.pred.test <- predict(lda.fit, newdata = test_set)
y_hat_test <- as.numeric(lda.pred.test$class)-1

# Compute the error
lda_train_error <- sum(abs(y_true_train - y_hat_train))/length(y_true_train) # 0.099
lda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)  # 0.136
lda_train_error
lda_test_error






####################################
#   Quadratic Discriminant Analysis
####################################
qda.fit <- qda(training_set$group ~., data = training_set)
qda.pred.train = predict(qda.fit, newdata = training_set)
y_hat_train <- as.numeric(qda.pred.train$class)-1
qda.pred.test = predict(qda.fit, newdata = test_set)
y_hat_test <- as.numeric(qda.pred.test$class) - 1

# Compute the error
qda_train_error <- sum(abs(y_true_train - y_hat_train))/length(y_true_train) #0.04950495
qda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)     #0.06818182
qda_train_error
qda_test_error

####################################
#   Regularized Discriminant Analysis
####################################
rda_fit <- rda(training_set$group ~., data = training_set, regularization = c(gamma = 0, lambda = .5))
rda_fit$error.rate  #0.0693
my_pred_rda <- predict(rda_fit, test_set)
rda_yhat_test <- as.numeric(my_pred_rda$class)-1
rda_te <- sum(abs(y_true_test - rda_yhat_test))/length(y_true_test)   #0.1363636





####################################
# C predicting the results
#####################################

dat=data.frame(list(relwt = 1.86,
                    glufast = 184,
                    glutest = 68,
                    instest = 122,
                    sspg = 554))

lda.pred.test2 <- predict(lda.fit, newdata = dat)        #NORMAL

qda.pred.test2 <- predict(qda.fit, newdata = dat)       #over_diabetic





