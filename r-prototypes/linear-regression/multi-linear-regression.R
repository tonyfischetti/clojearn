#!/usr/bin/Rscript --vanilla

# for generalized matrix inversions
library(MASS)
options(scipen=10)


hypothesis.function <- function(param.vec, x.mat){
  return(x.mat %*% matrix(param.vec))
}


# uses (naive) gradient descent to choose best linear
# regression coefficients
minimize <- function(x.mat, y.vec, alpha, epsilon=0.00000000001){
  # add dummy feature (to pair with intercept parameter)
  x.mat <- cbind(1, x.mat)
  # initialize param.vec
  param.vec <- rep(0, ncol(x.mat))

  m <- nrow(x.mat)

  while(TRUE){

    hypothesis <- hypothesis.function(param.vec, x.mat)

    new.params <- param.vec - alpha * (t(x.mat) %*% (hypothesis - y.vec)) / m

    if(all(abs(new.params - param.vec) < epsilon))
      return(round(new.params, 3))
    param.vec <- new.params
    #print(param.vec)
    #wait <- readline("continue? ")
    #cat("\n")
  }
}


# uses normal equation to choose best linear
# regression coefficients
norm.minimize <- function(x.mat, y.vec){
  # add column for theta0 (intercept)
  x.mat <- cbind(1, x.mat)
  param.vec <- ginv(t(x.mat) %*% x.mat) %*% t(x.mat) %*% y.vec
  return(as.vector(round(param.vec, 3)))
}



###########
# TESTING #
###########

test.mat <- matrix(c(2,1,3,4,2,3,6,3,2), ncol=3)

system.time(print(minimize(test.mat, c(26, 13, 15), alpha=0.05)))
system.time(print(norm.minimize(test.mat, c(26, 13, 15))))
cat("\n")
wait <- readline()


test.mat<-matrix(c(5,2,4,6,1,3,5,2,7,1,1,2,6,3,7,4,2,4,
                   6,1,3,6,3,6,1,3,6,7,3,5,1,1,2,1,3,4), ncol=6)

# theta 1 5 2 4 1 6 2
y.vec <- c(73, 47, 105, 98, 42, 79)

system.time(print(minimize(test.mat, y.vec, alpha=.02, epsilon=0.00000001)))
system.time(print(norm.minimize(test.mat, y.vec)))
cat("\n")
wait <- readline()


system.time(print(minimize(matrix(c(1, 2, 3)), c(2, 4, 6), alpha=.1)))                 # 0     2
system.time(print(norm.minimize(matrix(c(1, 2, 3)), c(2, 4, 6))))
cat("\n")
wait <- readline()
system.time(print(minimize(matrix(c(1, 2, 3)), c(-3, -6, -9), alpha=.1)))              # 0     -3
system.time(print(norm.minimize(matrix(c(1, 2, 3)), c(-3, -6, -9))))
cat("\n")
wait <- readline()
system.time(print(minimize(matrix(c(1, 2, 3, 4)), c(2, 4, 5, 8), alpha=.1)))           # 0     1.9
system.time(print(norm.minimize(matrix(c(1, 2, 3, 4)), c(2, 4, 5, 8))))
cat("\n")
wait <- readline()
system.time(print(minimize(matrix(c(1, 2, 3, 4, 5)), c(5, 8, 6, 8, 9), alpha=.1)))     # 4.8   0.8
system.time(print(norm.minimize(matrix(c(1, 2, 3, 4, 5)), c(5, 8, 6, 8, 9))))


