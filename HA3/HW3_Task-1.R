## -----------------------------------------------------------------------------------------------
# generate data
set.seed(20211106)
N <- 20
x1 <- runif(N, 1, 2)
x2 <- runif(N, 1, 2)
X <- cbind(x1, x2)
y <- ifelse(x2 > -.6 * x1 + 2.35, -1, 1)

# visualize data
plot(X, col = y + 3, pch=15)


## -----------------------------------------------------------------------------------------------
perceptron = function(x, y) {
  converged = F
  n_epochs = 0
  
  iter = dim(x)[1]
  n = dim(x)[2]
  x = as.matrix(x)
  
  # initialize weights
  w = matrix(0, 3, 1)
  while (converged == F) {
    
    # iterate over data set
    for (i in 1:iter) {
      y_pred = as.numeric(sign(t(w) %*% x[i, ]))
      w = w + (y[i] - y_pred) * x[i, ]
    }
    
    # stop training if all observations are 
    # correctly classified
    if ( sum(y * (t(w) %*% t(x)) <= 0) == 0 ) {
      print('Converged sucessfully')
      converged = T
    } else {
      n_epochs = n_epochs + 1
    }
  }
  return(w)
}


## -----------------------------------------------------------------------------------------------
# add bias term to training data
x = cbind(x0 = rep(1, N), X)

# train model
w = perceptron(x, y)


## -----------------------------------------------------------------------------------------------
# compute intercept and slope for decision boundary
intercept = w[1] / -w[3]
slope = w[2] / -w[3]


## -----------------------------------------------------------------------------------------------
# visualize data and decision boundary
png(
  filename = "plots/perceptron.png",
  height = 400,
  width = 600
)
plot(X, col = y + 3, pch = 15)
grid()
abline(a=intercept, b=slope)

