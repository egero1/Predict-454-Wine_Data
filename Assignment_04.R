# http://archive.ics.uci.edu/ml/datasets/Wine

library(lattice)
library(caret)
library(corrplot)
library(rpart)
library(rpart.plot)
library(pROC)

# helper function to print decision boundaries for variables
# adapted for this assignment
# original author: http://michael.hahsler.net/SMU/EMIS7332/R/viz_classifier.html
decisionplot <- function(model, data, class = NULL, predict_type = "class",
         resolution = 100, showgrid = TRUE, ...) {
        
        if(!is.null(class)) cl <- data[,class] else cl <- 1
        data <- data[,1:2]
        k <- length(unique(cl))
        
        plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
        legend("top", legend = unique(cl), horiz = TRUE, 
               col = as.integer(unique(cl))+1L, pch = as.integer(unique(cl))+1L)
        
        # make grid
        r <- sapply(data, range, na.rm = TRUE)
        xs <- seq(r[1,1], r[2,1], length.out = resolution)
        ys <- seq(r[1,2], r[2,2], length.out = resolution)
        g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
        colnames(g) <- colnames(r)
        g <- as.data.frame(g)
        
        ### guess how to get class labels from predict
        ### (unfortunately not very consistent between models)
        p <- predict(model, g, type = predict_type)
        if(is.list(p)) p <- p$class
        p <- as.factor(p)
        
        if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
        
        z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
        contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
                lwd = 1, levels = (1:(k-1))+.5)
        
        invisible(z)
}

# helper function to print multiple plots on a lattice panel
print_plot_panel <- function(plots) {
        print(plots[[2]], split = c(1, 1, 3, 2), more = TRUE)
        print(plots[[3]], split = c(2, 1, 3, 2), more = TRUE)
        print(plots[[4]], split = c(3, 1, 3, 2), more = TRUE)
        
        print(plots[[5]], split = c(1, 2, 3, 2), more = TRUE)
        print(plots[[6]], split = c(2, 2, 3, 2), more = TRUE)
        print(plots[[7]], split = c(3, 2, 3, 2), more = FALSE)
        
        print(plots[[8]], split = c(1, 1, 3, 2), more = TRUE)
        print(plots[[9]], split = c(2, 1, 3, 2), more = TRUE)
        print(plots[[10]], split = c(3, 1, 3, 2), more = TRUE)
        
        print(plots[[11]], split = c(1, 2, 3, 2), more = TRUE)
        print(plots[[12]], split = c(2, 2, 3, 2), more = TRUE)
        print(plots[[13]], split = c(3, 2, 3, 2), more = FALSE)
        
        print(plots[[14]], split = c(1, 1, 3, 2), more = FALSE)
}

print_plot_panel_6 <- function(plots) {
        print(plots[[2]], split = c(1, 1, 3, 2), more = TRUE)
        print(plots[[3]], split = c(2, 1, 3, 2), more = TRUE)
        print(plots[[4]], split = c(3, 1, 3, 2), more = TRUE)
        
        print(plots[[5]], split = c(1, 2, 3, 2), more = TRUE)
        print(plots[[6]], split = c(2, 2, 3, 2), more = TRUE)
        print(plots[[7]], split = c(3, 2, 3, 2), more = FALSE)
}

setwd("C:/Users/200019450/Desktop/Assignment 4")

# Data import and preparation
setwd("~/Desktop/Predict 454/Assignment 4")
wine_data <- read.csv("/Users/ericgero/Desktop/Predict 454/Assignment 4/Data/wine.data", header = FALSE)
var_names <- c("Cultivar", "Alcohol", "Malic_acid","Ash", "Alcalinity_of_ash" 
               , "Magnesium", "Total_phenols", "Flavanoids", "Nonflavanoid_phenols"
               , "Proanthocyanins", "Color_intensity", "Hue", "OD280_OD315"
               , "Proline")

names(wine_data) <- var_names

dim(wine_data)

summary(wine_data)

# set Class as factor 
wine_data$Cultivar <- as.factor(wine_data$Cultivar)

# Set factor variable levels
levels(wine_data$Cultivar) = c("One", "Two", "Three")

var_cnt <- dim(wine_data)[2]

# get class for each variable in the data frame
sapply(wine_data, class)

# review box plot to look for outliers
box_ash <- bwplot(wine_data$Ash, ylab="Ash", xlab="Class") 
box_aoa <- bwplot(wine_data$Alcalinity_of_ash, ylab="Alcalinity of Ash", xlab="Class") 

print(box_ash, split = c(1, 1, 1, 2), more = TRUE)
print(box_aoa, split = c(1, 2, 1, 2), more = FALSE)

# histograms - 6 variables
myplots <- list()
for (var in 2:7) {
        myplots[[var]] <- histogram(wine_data[,var], xlab=paste(var_names[var]))
}

print_plot_panel_6(myplots)

# density plots - 6 variables
myplots <- list()
for (var in 2:7) {
        myplots[[var]] <- densityplot(wine_data[,var], ylab=paste(var_names[var]), xlab = "", plot.points = FALSE)
}

print_plot_panel_6(myplots)

# EDA
# box plots by class
myplots <- list()
for (var in 2:7) {
        myplots[[var]] <- bwplot(wine_data[,var] ~ wine_data$Class, ylab=paste(var_names[var]), xlab = "Class")
}

print_plot_panel_6(myplots)

# histograms by class
myplots <- list()
for (var in 2:7) {
        myplots[[var]] <- histogram(~wine_data[,var] | wine_data$Class, ylab=paste(var_names[var]), xlab = "Class", layout = c(1,3))
}

print_plot_panel_6(myplots)

# density plots by class
myplots <- list()
for (var in 2:7) {
        myplots[[var]] <- densityplot(~wine_data[,var], groups = wine_data$Class, ylab=paste(var_names[var]), xlab = "", plot.points = FALSE)
}

print_plot_panel_6(myplots)

# correlation
for (level in unique(wine_data$Class)) {
        corrplot(cor(wine_data[wine_data$Class == level, 2:13]),
                 tl.col = "black", tl.cex = 0.5, title = paste("Class = ", level), 
                 tl.srt = 45, mar=c(0,0,1,0), diag = FALSE, type = "upper")
}

# Decision tree - create a simple tree to see what we can learn
tree <- rpart(Class ~ ., data = wine_data)
rpart.plot(tree, extra = 101, type = 2)

## Model Build
# Model 1 - Random Forest
# Set run parameters
fitControl = trainControl(method="repeatedcv", number=10, repeats=3, classProbs = TRUE)

# Fit the model
set.seed(123)
wine_rf <- train(Cultivar ~ .,
                 data = wine_data,
                 method = "rf",
                 importance = TRUE,
                 trControl = fitControl)

# Summary
wine_rf$finalModel

# Plot important variables identified by the random forest model
plot(varImp(wine_rf), main = "Variable Importance - Random Forest", layout = c(1,3))

# Training set predictions for roc
wine_rf_pred_probs <- predict(wine_rf, wine_data, type = "prob")

## Create ROC curve
wine_rf_roc <- roc(response = wine_data$Cultivar, predictor = wine_rf_pred_probs[,2], levels = c("One", "Two"))

## Plot ROC curve
plot(wine_rf_roc, col = "blue", main = paste0("ROC Curve for Random Forest - 1: AUC ", round(wine_rf_roc$auc[1], 4)))
# not falid for or useful for two reasonse... one this is multiclass.. 2 with a sensitivy of 1, all plots are the same and no information can be gained
# Create the confusion matrix
wine_rf_pred <- predict(wine_rf, wine_data)
confusionMatrix(data = wine_rf_pred, wine_data$Cultivar, positive = "One")

# Decision boundaries - important variables: Cultivar One
vars <- wine_data[,c("Proline", "Alcohol", "Cultivar")]
fitControl = trainControl(classProbs = TRUE)

set.seed(123)
impVars <- train(Cultivar ~ .,
                 data = vars,
                 method = "rf",
                 importance = TRUE,
                 trControl = fitControl)

decisionplot(model = impVars, data = vars, class = "Cultivar", 
             main = "Random Forest: Important Variables - Cultivar 1", predict_type = "raw")

# Decision boundaries - important variables: Cultivar Two
vars <- wine_data[,c("Alcohol", "Color_intensity", "Cultivar")]
fitControl = trainControl(classProbs = TRUE)

set.seed(123)
impVars <- train(Cultivar ~ .,
                  data = vars,
                  method = "rf",
                  importance = TRUE,
                  trControl = fitControl)

decisionplot(model = impVars, data = vars, class = "Cultivar", 
             main = "Random Forest: Important Variables - Cultivar 2", predict_type = "raw")

# Decision boundaries - important variables: Cultivar Three
vars <- wine_data[,c("Flavanoids", "Color_intensity", "Cultivar")]
fitControl = trainControl(classProbs = TRUE)

set.seed(123)
impVars <- train(Cultivar ~ .,
                 data = vars,
                 method = "rf",
                 importance = TRUE,
                 trControl = fitControl)

decisionplot(model = impVars, data = vars, class = "Cultivar", 
             main = "Random Forest: Important Variables - Cultivar 3", predict_type = "raw")

# Decision boundaries - important variables: bad pair
vars <- wine_data[,c("Nonflavanoid_phenols", "Ash", "Cultivar")]
fitControl = trainControl(classProbs = TRUE)

set.seed(123)
impVars <- train(Cultivar ~ .,
                 data = vars,
                 method = "rf",
                 importance = TRUE,
                 trControl = fitControl)

decisionplot(model = impVars, data = vars, class = "Cultivar", 
             main = "Random Forest: Important Variables - Bad Pair", predict_type = "raw")

# (2) a Support Vector Machine
# Set run parameters
fitControl = trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          classProbs = TRUE)

# Fit the model
# Model 2
set.seed(123)
wine_svm <- train(Cultivar ~ .,
                  data = wine_data,
                  method = "svmRadialWeights",
                  importance = TRUE,
                  trControl = fitControl)

# Summary
wine_svm$finalModel

# Plot important variables identified by the random forest model
plot(varImp(wine_svm), main = "Variable Importance - Support Vector Machine", layout = c(1,3))
v<- varImp(wine_svm)
# Training set predictions for roc
wine_svm_pred_probs <- predict(wine_rf, wine_data, type = "prob")

## Create ROC curve
wine_svm_roc <- roc(response = wine_data$Cultivar, predictor = wine_svm_pred_probs[,2], levels = c("One", "Two"))

## Plot ROC curve
plot(wine_svm_roc, col = "blue", main = paste0("ROC Curve for Random Forest - 1: AUC ", round(wine_svm_roc$auc[1], 4)))
# not falid for or useful for two reasonse... one this is multiclass.. 2 with a sensitivy of 1, all plots are the same and no information can be gained

# Create the confusion matrix
wine_svm_pred <- predict(wine_svm, wine_data)
confusionMatrix(data = wine_svm_pred, wine_data$Cultivar, positive = "One")

# Decision boundaries - important variables: Cultivar One and Three
vars <- wine_data[,c("OD280_OD315", "Flavanoids", "Cultivar")]
fitControl = trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          classProbs = TRUE)

set.seed(123)
impVars <- train(Cultivar ~ .,
                 data = vars,
                 method = "svmRadialWeights",
                 importance = TRUE,
                 trControl = fitControl)

decisionplot(model = impVars, data = vars, class = "Cultivar", 
             main = "SVM: Important Variables - Cultivar 1 & 3", predict_type = "raw")

confusionMatrix(predict(impVars, vars), vars$Cultivar)

# Decision boundaries - important variables: Cultivar Two
vars <- wine_data[,c("Hue", "Flavanoids", "Cultivar")]
fitControl = trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          classProbs = TRUE)

set.seed(123)
impVars <- train(Cultivar ~ .,
                 data = vars,
                 method = "svmRadialWeights",
                 importance = TRUE,
                 trControl = fitControl)

decisionplot(model = impVars, data = vars, class = "Cultivar", 
             main = "SVM: Important Variables - Cultivar 2", predict_type = "raw")

confusionMatrix(predict(impVars, vars), vars$Cultivar)

# (3) a neural network model
# Set run parameters
fitControl = trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          classProbs = TRUE)

# Fit the model
# Model 3
set.seed(123)
wine_nnet <- train(Cultivar ~ .,
                  data = wine_data,
                  method = "nnet",
                  trace = FALSE,
                  importance = TRUE,
                  trControl = fitControl)

# Summary
wine_nnet$finalModel

# Plot important variables identified by the random forest model
varimp = varImp(wine_nnet)
varimp$importance = as.data.frame(varimp$importance)[, -1]
plot(varimp, main = "Variable Importance - Neural Net", layout = c(1,3))

# Training set predictions for roc
wine_nnet_pred_probs <- predict(wine_rf, wine_data, type = "prob")

## Create ROC curve
wine_nnet_roc <- roc(response = wine_data$Cultivar, predictor = wine_nnet_pred_probs[,2], levels = c("One", "Two"))
#wine_rf_roc <- multiclass.roc(response = wine_data$Cultivar, predictor = wine_rf_pred_probs, levels = c("One", "Two", "Three"))

## Plot ROC curve
plot(wine_nnet_roc, col = "blue", main = paste0("ROC Curve for Random Forest - 1: AUC ", round(wine_nnet_roc$auc[1], 4)))
# not falid for or useful for two reasonse... one this is multiclass.. 2 with a sensitivy of 1, all plots are the same and no information can be gained

# Create the confusion matrix
wine_nnet_pred <- predict(wine_nnet, wine_data)
confusionMatrix(data = wine_nnet_pred, wine_data$Cultivar, positive = "One")

# Decision boundaries - important variables: Cultivars
vars <- wine_data[,c("Alcohol", "Flavanoids", "Cultivar")]
fitControl = trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          classProbs = TRUE)

set.seed(123)
impVars <- train(Cultivar ~ .,
                 data = vars,
                 method = "nnet",
                 importance = TRUE,
                 trace = FALSE,
                 trControl = fitControl)

decisionplot(model = impVars, data = vars, class = "Cultivar", 
             main = "NNET: Important Variables", predict_type = "raw")

confusionMatrix(predict(impVars, vars), vars$Cultivar)

## Compare models
# collect resamples
results <- resamples(list(RF=wine_rf, SVM=wine_svm, NNET=wine_nnet))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results, main = "Model Comparison")

