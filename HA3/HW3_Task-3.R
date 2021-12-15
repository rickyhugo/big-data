## -----------------------------------------------------------------------------------------------
rm(list = ls())


## -----------------------------------------------------------------------------------------------
library(kernlab)
library(caret)


## -----------------------------------------------------------------------------------------------
colSds <- function(x, na.rm=TRUE) {
  if (na.rm) {
    n <- colSums(!is.na(x))
  } else {
    n <- nrow(x)
  }
  colVar <- colMeans(x*x, na.rm=na.rm) - (colMeans(x, na.rm=na.rm))^2
  return(sqrt(colVar * n / (n - 1)))
}


## -----------------------------------------------------------------------------------------------
get_gaussian_gram = function(x, sigma) {
  set.seed(2020)

  # define kernel
  kernel = rbfdot(sigma = sigma)
  
  # compute gram matrix
  K = kernelMatrix(kernel = kernel, x = x)
  
  n = dim(x)[1]
  d = K@.Data
  return(d)
}


## -----------------------------------------------------------------------------------------------
get_prediction_gram = function(train, val, sigma) {
  set.seed(2020)
  
  # define kernel
  kernel = rbfdot(sigma = sigma)
  
  # compute gram matrix for prediction
  n = dim(train)[1]
  d = dim(val)[1]
  res = matrix(0, n, d)
  for (i in 1:n) {
    for (j in 1:d) {
      res[i, j] = kernel(train[i, ], val[j, ])
    }
  }
  return(res)
}


## -----------------------------------------------------------------------------------------------
split_data = function(data, size=400) {
  set.seed(2020)
  
  # split data into test and training sets
  x = data.matrix(data)
  n = dim(x)[1]
  train_id = sample(1:n, size)
  
  n_cols = ncol(data)
  feature_col = 1:(n_cols - 1)
  response_col = n_cols
  
  features = x[ , feature_col] # predictors
  response = x[ , response_col] # response
  
  x_train = features[train_id, ]
  x_train_scaled = scale(x_train)
  y_train = response[train_id]
  
  x_test = features[-train_id, ]
  x_test_scaled = scale(
    x_test,
    center = colMeans(x_train),
    scale = colSds(x_train)
  )
  y_test = response[-train_id]
  
  return(
    list(
      trainX=x_train_scaled,
      trainY=matrix(y_train),
      X=x_test_scaled,
      Y=matrix(y_test)
    )
  )
}


## -----------------------------------------------------------------------------------------------
get_RMSE = function(prediction, label) {
  return(sqrt(mean((label - prediction)^2)))
}


## -----------------------------------------------------------------------------------------------
k_fold_cv = function(k, param_grid, x, y) {
  set.seed(2020)
  
  # split data into k even folds
  folds = createFolds(y, k = 10)
  
  k = length(folds) # no. folds for CV
  n = dim(param_grid)[1] # no. parameter settings
  fold_index = matrix(1:k)
  
  RMSE_mat = matrix(0, k, n)
  
  for (i in 1:k) {
    # extract data for current k-1 folds
    which_folds = fold_index[fold_index != i]
    train_idx = as.numeric(unlist(folds[which_folds]))
    train_data = x[train_idx, ]
    train_labels = y[train_idx, ]
    val_data = x[-train_idx, ]
    val_labels = y[-train_idx, ]
    for (p in 1:n) {
      # fit a model for each parameter setting
      sigma = param_grid$sigma[p]
      lambda = param_grid$lambda[p]
      
      K_train = get_gaussian_gram(train_data, sigma)
      K_val = get_prediction_gram(train_data, val_data, sigma)

      w = get_weights(K_train, train_labels, lambda)
      val_pred = t(w) %*% K_val
      RMSE_mat[i, p] = get_RMSE(val_pred, val_labels) 
    }
  }
  return(colMeans(RMSE_mat, na.rm = T)) # return average RMSE
}


## -----------------------------------------------------------------------------------------------
get_weights = function(K, y, lambda) {
  n = dim(K)[1]
  identity = diag(1, n, n)
  w = solve(K + lambda * identity) %*% y
  return(w)
}


## -----------------------------------------------------------------------------------------------
predictKRR = function(trainX, trainY, X, lambda, sigma) {
  # kernelize data
  K_train = get_gaussian_gram(trainX, sigma)
  K_test = get_prediction_gram(trainX, X, sigma)
  
  # compute weights
  w = get_weights(K_train, trainY, lambda)
  
  # get predictions
  pred = t(w) %*% K_test
  return(pred)
}


## -----------------------------------------------------------------------------------------------
# read data 
data = read.table("data/Boston.txt", header = T, sep = ",")


## -----------------------------------------------------------------------------------------------
# get scaled test and training data
d = split_data(data)

# train
trainX = d$trainX
trainY = d$trainY

# test
X = d$X
Y = d$Y


## -----------------------------------------------------------------------------------------------
set.seed(2020)

# ordinary linear regression
lr_train = as.data.frame(trainX)
lr_train$medv = trainY
lr = lm(medv ~ ., data=lr_train)


## -----------------------------------------------------------------------------------------------
# test lr model
lr_y_hat = predict(lr, as.data.frame(X))


## -----------------------------------------------------------------------------------------------
LR_RMSE = get_RMSE(as.numeric(lr_y_hat), Y)

png(
  filename = "plots/lr_pred.png",
  height = 400,
  width = 600
)
plot(
  lr_y_hat, col = 4, pch = 16,
  main = "LR Predictions",
  sub = paste("RMSE:", round(LR_RMSE, 2)),
  ylab = "medv",
  xlab = "Observation\n"
)
points(Y, col = 1, pch=17) # true values
grid()
legend(
  "topright", inset = .02,
  legend = c("Prediction", "True value"),
  col = c(4, 1), pch = c(16, 17)
)


## -----------------------------------------------------------------------------------------------
set.seed(2020)

# find optimal hyperparameters by 10-fold CV
param_grid = expand.grid(
  lambda = c(.1, .01, .001),
  sigma = c(.1, .01, .001)
)
cv_params = k_fold_cv(k = 10, param_grid, trainX, trainY)


## -----------------------------------------------------------------------------------------------
set.seed(2020)

# extract optimal hyperparameters
param_idx = match(min(cv_params), cv_params)
opti_params = param_grid[param_idx, ]

# fit optimal model and predict test set
y_hat = predictKRR(
  trainX, trainY, X, opti_params$lambda, opti_params$sigma
)


## -----------------------------------------------------------------------------------------------
KRR_RMSE = get_RMSE(t(y_hat), Y)

png(
  filename = "plots/krr_pred.png",
  height = 400,
  width = 600
)
plot(
  y_hat[1, ], col = 3, pch = 16,
  main = "KRR Predictions",
  sub = paste("RMSE:", round(KRR_RMSE, 2)),
  ylab = "medv",
  xlab = "Observation\n"
)
points(Y, col = 1, pch=17) # true values
grid()
legend(
  "topright", inset = .02,
  legend = c("Prediction", "True value"),
  col = c(3, 1), pch = c(16, 17)
)


## -----------------------------------------------------------------------------------------------
predictKRR_ = function(trainX,trainY,X,lambda,sigma){
  
  rbf <- rbfdot(sigma)
  K = kernelMatrix(rbf, trainX)
  K_test = kernelMatrix(rbf, trainX, X)
  I = diag(dim(trainY)[1])
  pred = t(trainY) %*% solve(K + lambda*I) %*% K_test

  return(pred)
}

## -----------------------------------------------------------------------------------------------
y_EBBA = predictKRR(
  trainX, trainY, X, opti_params$lambda, opti_params$sigma
)
get_RMSE(t(y_EBBA), Y)


## -----------------------------------------------------------------------------------------------
library(listdtr)
set.seed(2020)

# built-in KRR for comparison
test_model = krr(trainX, trainY)


## -----------------------------------------------------------------------------------------------
# predict test set and compute RMSE
comparison_y_hat = predict(test_model, X)
comparison_RMSE = get_RMSE(comparison_y_hat, Y)
paste("Built-in KRR RMSE:", round(comparison_RMSE, 2))

