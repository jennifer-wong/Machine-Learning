setwd("C:/Users/jenni/Desktop/Final Project")

unzip("digits.zip")

# Problem 1

# This function loads a digits file and converts the columns to appropriate 
# datatypes. The argument digits_file is the name of the digits file the
# user wants to load.
read_digits = function(digits_file) {
  data = read.table(digits_file)
  data$V1 = as.factor(data$V1)
  
  return(data)
}


# Problem 2

# This function displays one observation from the data set as a grayscale 
# image. The argument dataset is the data set the user wants to select an
# observation from. The argument observation is the observation number of
# a digit.
view_digit = function(dataset, observation) {
  pixels = dataset[observation, -1]
  pixels = matrix(unlist(t(pixels)), byrow=TRUE, nrow=16, ncol=16)
  pixels = apply(pixels, 2, rev)
  image(t(pixels), col = grey(seq(0, 1, length = 256)), xaxt='n', yaxt='n')
}


# Problem 3

# get number of observations for each digit
table(train$V1)
barplot(table(train$V1), ylim=c(0, 1200), xlab = "digit", 
        ylab = "# of observations", 
        main = "Number of Observations for Each Digit")

# find sum of variance of pixels for each digit
for(digit in levels(train$V1)) {
  data = train[train$V1==digit, -1]
  var = apply(data, 2, var)
  
  if(digit == "0") {
    variance = sum(var)
  } else {
    variance = cbind(variance, sum(var))
  }
}
colnames(variance) = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
barplot(variance, ylim=c(0, 120), xlab = "digit", 
        ylab = "sum of variance", 
        main = "Sum of Variance of Pixel Values for Each Digit")

# display what each digit looks like on average
for(digit in levels(train$V1)) {
  data = train[train$V1==digit, -1]
  average = cbind(-2, t(colMeans(data)))
  view_digit(average, 1)
}

# for each digit, get average of each pixel
for(digit in levels(train$V1)) {
  data = train[train$V1==digit, -1]
  
  if(digit == "0") {
    average = t(colMeans(data))
  } else {
    average = rbind(average, t(colMeans(data)))
  }
}

# find variance of each pixel to find most and least useful pixels
variance = apply(average, 2, var)
variance = sort(variance, decreasing = TRUE)
head(variance)  # most useful pixels
tail(variance)  # least useful pixels


#Problem 4

# This function uses k-nearest neighbors to predict the label for a point or 
# a collection of points. The argument k is the number of nearest neighbors. test is
# a dataframe that contains pixel values of a digit. train is the training dataset.
# metric is the distance metric to use when calculating distance.
predict_knn = function(k, test, train, metric) {
  distances = as.matrix(dist(rbind(test, train[, -1]), method=metric, upper=TRUE, 
                             diag=TRUE))
  distances = distances[(nrow(test) + 1):nrow(distances), ]
  
  predictions = get_predictions(k, nrow(test), train, distances)
  
  return(predictions)
}

# This function gets the number of counts for each class in neighbors. k is the number
# of nearest neighbors. The argument neighbors contains the points of all the neighbors.
get_counts = function(k, neighbors) {
  counts = numeric(10)
  
  for(i in 1:k) {
    counts[as.numeric(neighbors[i, 1])] = counts[as.numeric(neighbors[i, 1])] + 1
  }
  
  return(counts)
}

# This function gets the k-nearest neighbors. k is the number of nearest neighbors.
# train is the training dataset.
get_neighbors = function(k, train) {
  train = train[order(train$distance), ]
  
  neighbors = train[1:k, ]
  
  return(neighbors)
}

# This function gets the predictions for k-nearest neighbors. k is the number of nearest
# neighbors. num_test is the number of testing points. train is the training dataset. 
# distances is the matrix that contains the distances between every point.
get_predictions = function(k, num_test, train, distances) {
  predictions = numeric(num_test)
  
  for(i in 1:num_test) {
    train$distance = distances[, i]
    neighbors = get_neighbors(k, train)
    counts = get_counts(k, neighbors)
    predictions[i] = which.max(counts) - 1
  }
  
  return(predictions)
}


# Problem 5

# This function uses 10-fold cross-validation to estimate the error rate for k-nearest 
# neighbors. k is the number of nearest neighbors. data is the dataset to use cross-
# validation for. metric is the distance metric to use when calculating distance.
cv_error_knn = function(k, data, metric) {
  set.seed(23)
  data = data[sample(nrow(data), replace=FALSE), ]
  folds = as.data.frame(cbind(as.numeric(rownames(data)), rep_len(1:10, nrow(data))))
  distances = as.matrix(dist(data[, -1], method=metric, upper=TRUE, diag=TRUE))
  
  return(do_cv(k, data, folds, distances))
}

# This function does 10-fold cross-validation. k is the number of nearest neighbors. 
# data is the dataset to use cross-validation for. folds is the dataframe that contains
# the assignment of indices to folds. distances is the matrix that holds the distances
# between every point.
do_cv = function(k, data, folds, distances) {
  error_rates = numeric(10)
  
  for(i in 1:10) {
    test = data[as.numeric(rownames(data)) %in% folds[folds$V2==i, 1], ]
    train = data[!(as.numeric(rownames(data)) %in% folds[folds$V2==i, 1]), ]
    
    dist = distances[, as.numeric(rownames(data)) %in% folds[folds$V2==i, 1]]
    dist = dist[!(as.numeric(rownames(data)) %in% folds[folds$V2==i, 1]), ]
    
    predictions = get_predictions(k, nrow(test), train, dist)
    
    error_rates[i] = get_error_rate(test, predictions)
  }
  
  return(mean(error_rates))
}

# This function gets the error rate. test is the testing dataset. predictions contains
# the predictions of the testing dataset.
get_error_rate = function(test, predictions) {
  wrong_counter = 0
  
  for(i in 1:length(predictions)) {
    if(predictions[i] != (as.numeric(test[i, 1]) - 1)) {
      wrong_counter = wrong_counter + 1
    }
  }
  
  error_rate = wrong_counter / nrow(test)
  
  return(error_rate)
}


# Problem 6

# make dataframe to store CV error rates
k_metric = as.data.frame(matrix(nrow = 45, ncol = 3))
colnames(k_metric) = (c("Metric", "k", "error_rate"))
k_metric$Metric = rep(c("euclidean", "manhattan", "maximum"), each=15)
k_metric$k = rep(1:15)

# get CV error rates for different distance metrics and k
row_num = 1
set.seed(23)
data = train[sample(nrow(train), replace=FALSE), ]
folds = as.data.frame(cbind(as.numeric(rownames(data)), rep_len(1:10, nrow(data))))
for(metric in c("euclidean", "manhattan", "maximum")) {
  distances = as.matrix(dist(data[, -1], method=metric, upper=TRUE, diag=TRUE))
  
  for(k in rep(1:15)) {
    k_metric1[row_num, 3] = do_cv(k, data, folds, distances)
    row_num = row_num + 1
  }
}

# plot CV error rates
library(ggplot2)
library(viridis)
ggplot(k_metric1, aes(factor(k), error_rate, fill = Metric)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_color_viridis() + 
  labs(x="k", y="Error Rates") +
  ggtitle("10-fold CV Error Rates") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, .205)


# Problem 7

# get confusion matrices for each of the 3 best k and distance metric combinations
confusion_matrices = vector("list", 3)
set.seed(23)
data = train[sample(nrow(train), replace=FALSE), ]
folds = as.data.frame(cbind(as.numeric(rownames(data)), rep_len(1:10, nrow(data))))
distances = as.matrix(dist(data[, -1], method="euclidean", upper=TRUE, diag=TRUE))
counter = 1

for(k in c(1, 3, 4)) {
  error_rates = numeric(10)
  
  for(i in 1:10) {
    test = data[as.numeric(rownames(data)) %in% folds[folds$V2==i, 1], ]
    train = data[!(as.numeric(rownames(data)) %in% folds[folds$V2==i, 1]), ]
    
    dist = distances[, as.numeric(rownames(data)) %in% folds[folds$V2==i, 1]]
    dist = dist[!(as.numeric(rownames(data)) %in% folds[folds$V2==i, 1]), ]
    
    predictions = get_predictions(k, nrow(test), train, dist)
    
    if(i == 1) {
      confusion_matrix = table(as.factor(predictions), test[ , 1]) / nrow(test)
    } else {
      confusion_matrix = confusion_matrix + 
        (table(as.factor(predictions), test[ , 1]) / nrow(test))
    }
  }
    
  confusion_matrices[[counter]] = confusion_matrix / 10
  counter = counter + 1
}


#Problem 8

# get misclassified digits using 10-fold cross-validation
misclassified = as.data.frame(matrix(nrow = 500, ncol = 2))
colnames(misclassified) = (c("actual", "predicted"))
distances = as.matrix(dist(data[, -1], method="euclidean", upper=TRUE, diag=TRUE))
row_num = 1

for(i in 1:10) {
  test = data[as.numeric(rownames(data)) %in% folds[folds$V2==i, 1], ]
  train = data[!(as.numeric(rownames(data)) %in% folds[folds$V2==i, 1]), ]
  
  dist = distances[, as.numeric(rownames(data)) %in% folds[folds$V2==i, 1]]
  dist = dist[!(as.numeric(rownames(data)) %in% folds[folds$V2==i, 1]), ]
  
  predictions = get_predictions(k, nrow(test), train, dist)
  
  for(j in 1:length(predictions)) {
    if(predictions[j] != (as.numeric(test[j, 1]) - 1)) {
      misclassified[row_num, 1] = as.numeric(test[j, 1]) - 1
      misclassified[row_num, 2] = predictions[j]
      row_num = row_num + 1
    }
  }
}

# get confusion matrix for misclassified data
table(misclassified$predicted, misclassified$actual)


# Problem 9

# make dataframe to store CV error rates
test_errors = as.data.frame(matrix(nrow = 45, ncol = 3))
colnames(test_errors) = (c("Metric", "k", "error_rate"))
test_errors$Metric = rep(c("euclidean", "manhattan", "maximum"), each=15)
test_errors$k = rep(1:15)

# get test error rates for different k and distance metrics
row_num = 1
for(metric in c("euclidean", "manhattan", "maximum")) {
  distances = as.matrix(dist(rbind(test[, -1], train[, -1]), method=metric, upper=TRUE, 
                             diag=TRUE))
  distances = distances[(nrow(test) + 1):nrow(distances), ]#change to columns?
  
  for(k in rep(1:15)) {
    predictions = get_predictions(k, nrow(test), train, distances)
    test_errors[row_num, 3] = get_error_rate(test, predictions)
    row_num = row_num + 1
  }
}

# plot test error rates
ggplot(test_errors, aes(factor(k), error_rate, fill = Metric)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_color_viridis() + 
  labs(x="k", y="Error Rates") +
  ggtitle("Test Set Error Rates") +
  theme(plot.title = element_text(hjust = 0.5))

