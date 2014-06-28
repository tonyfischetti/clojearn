#!/usr/bin/Rscript --vanilla



hypothesis.function <- function(param.vec, x.mat){
  return(x.mat %*% matrix(param.vec))
}


minimize <- function(x.mat, y.vec, alpha, epsilon=0.00000000001){
  # initialixe param.vec
  param.vec <- rep(0, ncol(x.mat)+1)
  x.mat <- cbind(1, x.mat)
  while(TRUE){

    new.params <- param.vec - apply(x.mat, 2,
    function(x.vec){
      return(alpha * (mean((hypothesis.function(param.vec, x.mat) - y.vec) * x.vec)))
    })

    if(all(abs(new.params - param.vec) < epsilon))
      return(round(new.params, 3))
    param.vec <- new.params
    #print(param.vec)
    #readline("continue? ")
    #cat("\n")
  }
}

test.mat <- matrix(c(2,1,3,4,2,3,6,3,2), ncol=3)

minimize(test.mat, c(26, 13, 15), alpha=0.05)

test.mat<-matrix(c(5,2,4,6,1,3,5,2,7,1,1,2,6,3,7,4,2,4,
                   6,1,3,6,3,6,1,3,6,7,3,5,1,1,2,1,3,4), ncol=6)

# theta 1 5 2 4 1 6 2
y.vec <- c(73, 47, 105, 98, 42, 79)

minimize(test.mat, y.vec, alpha=.02, epsilon=0.00000001)


minimize(matrix(c(1, 2, 3)), c(2, 4, 6), alpha=.1)                 # 0     2
minimize(matrix(c(1, 2, 3)), c(-3, -6, -9), alpha=.1)              # 0     -3
minimize(matrix(c(1, 2, 3, 4)), c(2, 4, 5, 8), alpha=.1)           # 0     1.9
minimize(matrix(c(1, 2, 3, 4, 5)), c(5, 8, 6, 8, 9), alpha=.1)     # 4.8   0.8
