require(tree); (rpart); (randomForest)
#'@author HxLi
#'@title LOO analysis for siderophore machine learning 
#'@param a tsv file with 30 siderophore and 30 decoy
#'@return a table with prediction of siderophores from LOO analysis

LOO_analysis <- function(directory){
  train.sider <- get.train.sider(directory)
  test.sider <- get.test.sider(directory)
  decoy <- get.decoy(directory)
  LOO_tree <- tree_model(sider_train, sider_test)
  write.csv(LOO_tree, file = "~/git/R-script-for-learning/LOO_tree.csv")
  LOO_rpart <- rpart_model(sider_train, sider_test)
  write.csv(LOO_raprt, file = "~/git/R-script-for-learning/LOO_rpart.csv")
  LOO_rf <- rf_model(sider_train, sider_test)
  write.csv(LOO_rf, file = "~/git/R-script-for-learning/LOO_rf.csv")
}

tree_model <- function(sider_train, sider_test){
  fit_tree <- making_tree_model(sider_train)
  LOO_tree <- testing_tree_model(fit_tree, sider_test)
}

rpart_model <- function(sider_train, sider_test){
  fit_rpart <- making_rpart_model(sider_train)
  LOO_rpart <- testing_rpart_model(fit_rpart, sider_test)
}

rf_model <- function(sider_train, sider_test){
  fit_rf <- making_rf_model(sider_train)
  LOO_rf <- testing_rf_model(fit_rf, sider_test)
}

#making the model based on the training set and testing the model on the testing cluster
making_tree_model <- function(sider_train){
fit_tree <- tree(factor(siderophore) ~ . , data = sider_train) ## fit classification tree using the training data ##
plot(fit_tree) ## plot the tree ##
text(fit_tree) ## attach labels ##
#summary(fit_tree) ## summary statistics of the fit ##
}

testing_tree_model <- function(fit_tree, sider_test) {
class_tree <- predict(fit_tree, newdata = sider_test, type = "class") ## predicted class for the test data ##
pred_tree <- cbind(row.names(sider_test), class_tree)
colnames(pred_tree) <- c("clusterName", "siderophore")
#write.csv(pred_tree, file = "")## saving your predictions ##
return(pred_tree)
}

making_rpart_model <- function(sider_train){
fit_rpart <- rpart(factor(siderophore) ~ . , data = sider_train) ## fit a recursive partitioning tree using the training data ##
par(xpd = TRUE)
plot(fit_rpart) ## plot the tree ##
text(fit_rpart, use.n = T) ## attach labels ##
labels(fit_rpart, use.n = T) ## see the labels ##
}

testing_rpart_model <- function(fit_rpart, sider_test) {
class_rpart <- predict(fit_rpart, newdata = sider_test, type = "class") ## predicted class for the test data ##
pred_rpart <- cbind(row.names(sider_test), class_rpart)
colnames(pred_rpart) <- c("clusterName", "siderophore")
return(pred_rpart)
}

making_rf_model <- function(sider_train){
set.seed(1)
fit_rf <- randomForest(factor(siderophore) ~ . , data = sider_train) ## fit random forest using training data ##
varImpPlot(fit_rf) ## check important variables ##
fit_rf$importance ## check the numbers for variable importance ##
plot(fit_rf) ## plot of fit, start of tuning your model ##
}

testing_rf_model <- function(fit_rf, sider_test) {
class_rf <- predict(fit_rf, newdata = sider_test, type = "class") ## predicted class for the test data using random forest ##
pred_rf <- cbind(row.names(sider_test), class_rf)
colnames(pred_rf) <- c("clusterName", "siderophore")
return(pred_rf)
}

#prob_rf <- predict(fit_rf, newdata = sider_test, type = "prob") ## predicted class probability for the test data using random forest ##
#pred_rf <- cbind(row.names(sider_test), prob_rf)
#colnames(pred_rf) <- c("clusterName", "non-siderophore", "siderophore")
#pred_rf
#write.csv(pred_rf, file = "") ## save your predicted probabilities ##

#making a subtable for the training set and another subtable (1 row) for the testing cluster
get.sider <- function(directory){
  LOO_sider <- read.table(file = directory)
  LOO_sider <- as.matrix(LOO_sider)[,-1]
}

get.decoy <- function(directory){
  decoy <- read.table(file = "~/git/R-scripts-for-learning/cluster_breakdowns_decoy.tsv")
  decoy <- as.matrix(decoy)[,-1]
  return(decoy)
}

get.row.from.sider <- function(LOO_sider){
  list.of.rows <- split(LOO_sider, row(LOO_sider))
  return(list.of.rows)
}

get.test.sider <- function(list.of.rows)
  for(i in 1: length(list.of.rows)){
  test.sider <- matrix(list.of.rows[[i]], nrow = 1, byrow = FALSE)
  colnames(test.sider) <- colnames(LOO_sider)
  return(test.sider)
  }

get.train.sider <- function(test.sider, list.of.rows){
  train.sider <- setdiff(list.of.rows, test.sider)
  train.sider <- lapply(train.sider,  matrix(unlist(train.sider), ncol = 28, byrow = TRUE))
  colnames(train.sider) <- colnames(LOO_sider)
  train.sider <- rbind(train.sider, decoy)
  return(train.sider)
}