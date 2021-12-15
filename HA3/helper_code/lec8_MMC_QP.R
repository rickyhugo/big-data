# QP-SVM
rm(list = ls())
library(quadprog)

d = cbind(c(0, 2, 2, 3), c(0, 0, 2, 0), c(-1, 1, -1, 1))

Q = diag(c(.00001,1,1))
p = c(0,0,0)
A = t(d[,3]*cbind(rep(1,4),d[,-3]))
h = c(1,1,1,1)

solve.QP(Dmat = Q, dvec = p, Amat = A, bvec = h)
