set.seed(20211106)
N <- 20
x1 <- runif(N,1,2)
x2 <- runif(N,1,2)
X <- cbind(x1,x2)
y <- ifelse(x2>-.6*x1+2.35,-1,1)

plot(X, col = y+3)
