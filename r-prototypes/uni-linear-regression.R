#!/usr/bin/Rscript --vanilla


#  we don't use this, but we use its partial derivatives
# cost.function <- function(x.vec, y.vec){
#   return(0.5 * mean((x.vec - y.vec) ^ 2))
# }

hypothesis.function <- function(theta1, theta2, x.vec){
  return(theta1 + theta2 * x.vec)
}

minimize <- function(x.vec, y.vec, alpha=1, theta1=0, theta2=0, epsilon=0.000001){
  while(TRUE){
    new.theta1 <- theta1 - (alpha *
                            ((mean(hypothesis.function(theta1,
                                                       theta2,
                                                       x.vec) - y.vec))))
    new.theta2 <- theta2 - (alpha *
                            ((mean((hypothesis.function(theta1,
                                                       theta2,
                                                       x.vec) - y.vec) * x.vec))))
    if((abs(new.theta1-theta1) < epsilon) &&
       (abs(new.theta2-theta2) < epsilon))
      return(c(round(new.theta1,3), round(new.theta2, 3)))
    theta1 <- new.theta1
    theta2 <- new.theta2
    cat("Theta 1: "); cat(theta1); cat("\n");
    cat("Theta 2: "); cat(theta2); cat("\n");
    readline("continue? ")
    cat("\n")
  }
}


minimize(c(1, 2, 3), c(2, 4, 6), alpha=.2)
minimize(c(1, 2, 3), c(-3, -6, -9), alpha=.2)
minimize(c(1, 2, 3, 4), c(2, 4, 5, 8), alpha=.2)
minimize(c(1, 2, 3, 4, 5), c(5, 8, 6, 8, 9), alpha=.1)
