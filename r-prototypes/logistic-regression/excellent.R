#!/usr/bin/Rscript --vanilla


# all's I need to do is clean up

#
#
# for linear, everything works,
#   optim (gradient, cost) and plotting
#
# now doing to fit a polynomial
#  and plot a polynomial
#
#

rm(list=ls())
options(scipen=10)

library(dplyr)
library(ggplot2)


hypothesis.function <- function(param.vec, x.mat){
  zed <- x.mat %*% matrix(param.vec)
  return(1 / (1 + exp(-zed)))
}

get.gradient <- function(param.vec, x.mat, y.vec, lambda=0){
  m <- nrow(x.mat)
  modtheta <- param.vec
  modtheta[1] <- 0
  the.hyp <- hypothesis.function(param.vec, x.mat)
  gradient <- (t(x.mat) %*% (the.hyp - y.vec) + lambda*modtheta) / m
  return(gradient)
}

cost.function <- function(param.vec, x.mat, y.vec, lambda=0){
  m <- nrow(x.mat)
  the.hyp <- hypothesis.function(param.vec, x.mat)
  cost <- (((t(-y.vec) %*% log(the.hyp)) - (t(1-y.vec) %*% log(1-the.hyp))) / m) +
          ((lambda / (2*m)) * sum(param.vec[2:length(param.vec)] ^ 2))
 
  return(cost)
}


add.poly.features <- function(x.mat, degree=2){
  new.mat <- matrix(1, nrow=nrow(x.mat))
  for (i in 1:degree){
    for (j in 0:i){
      new.mat <- cbind(new.mat, (x.mat[,1]^(i-j) * (x.mat[,2]^j)))
    }
  }
  return(new.mat)
}


do.plot <- function(a.data.frame, chosen.params, degree=2, accuracy=0,
                    lambda=0){
  thex1 <- a.data.frame[,1]
  thex2 <- a.data.frame[,2]
  somex <- seq(min(thex1), max(thex1), by=.05)
  somex2 <- seq(min(thex2), max(thex2), length.out=length(somex))

  z <- matrix(0, nrow=length(somex), ncol=length(somex))

  for (i in 1:length(somex)){
    for (j in 1:length(somex)){
      keep <- add.poly.features(t(matrix(c(somex[i], somex2[j]))), degree)
      z[i, j] <- as.matrix(keep) %*% chosen.params
    }
  }

  plot(a.data.frame$X2 ~ a.data.frame$X1,  pch=20, 
       col=c("red","green3")[a.data.frame$Y+1],
       xlab="X1", ylab="X2")

  contour(somex, t(somex2), z, nlevels=1, add=TRUE, drawlabels=FALSE)

  title(main=paste0(degree,
                    "-degree logistic regression decision boundary"))

  if(accuracy!=0 && lambda!=0){
    mtext(paste0("Accuracy: ", accuracy, "   -   lambda: ", lambda), side=3)
  }
}





##############################################
##############################################



training <- read.csv("./circles.csv", stringsAsFactors=FALSE,
                     colClasses="numeric")

names(training) <- c("X1", "X2", "Y")


design.mat <- add.poly.features(training[,c(1,2)], degree=2)

result <- optim(par=rep(0, ncol(design.mat)),
                cost.function, 
                get.gradient,
                x.mat=design.mat,
                y.vec=as.matrix(training[,3]), lambda=0,
                method="BFGS")


preds <- hypothesis.function(result$par, design.mat)
accur <- sum(round(preds) == training$Y) / 3

do.plot(training, result$par, degree=2, accuracy=paste0(round(accur,2), "%"), lambda=0)


####################################################

##############################################
##############################################



training <- read.csv("./moons.csv", stringsAsFactors=FALSE,
                     colClasses="numeric")

names(training) <- c("X1", "X2", "Y")

design.mat <- add.poly.features(training[,c(1,2)], degree=4)

result <- optim(par=rep(0, ncol(design.mat)),
                cost.function, 
                get.gradient,
                x.mat=design.mat,
                y.vec=as.matrix(training[,3]), lambda=1,
                method="BFGS")

preds <- hypothesis.function(result$par, design.mat)
accur <- sum(round(preds) == training$Y) / 3

do.plot(training, result$par, degree=4, accuracy=paste0(round(accur,2), "%"), lambda=1)


####################################################


##############################################
##############################################



#training <- read.csv("./acceptance.csv", stringsAsFactors=FALSE,
training <- read.csv("./second.csv", stringsAsFactors=FALSE,
                     colClasses="numeric")


design.mat <- add.poly.features(training[,c(1,2)], degree=4)

result <- optim(par=rep(0, ncol(design.mat)),
                cost.function, 
                get.gradient,
                x.mat=design.mat,
                y.vec=as.matrix(training[,3]), lambda=1,
                method="BFGS")

do.plot(training, result$par, degree=4)


####################################################




##############################################
##############################################


training <- read.csv("./second.csv", stringsAsFactors=FALSE,
                     colClasses="numeric")


design.mat <- as.matrix(training[,c(1,2)])
# let's try to fit a polynomial
design.mat <- cbind(design.mat, design.mat[,1] ^ 2,
                    design.mat[,2] ^ 2,
                    (design.mat[,1] + design.mat[,2]))

result <- optim(par=rep(0, ncol(design.mat)+1),
                cost.function, 
                get.gradient,
                x.mat=design.mat,
                y.vec=as.matrix(training[,3]), lambda=0,
                method="BFGS")


ress <- hypothesis.function(result$par,
                    cbind(1, as.matrix(training[,c(1,2)])))
ress <- round(ress)
sum(ress == training$admitted)


do.plot(training, result$par)

