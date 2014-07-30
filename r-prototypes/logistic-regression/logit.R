#!/usr/bin/Rscript --vanilla

rm(list=ls())
options(scipen=10)

library(dplyr)
library(ggplot2)


# a ok!
hypothesis.function <- function(param.vec, x.mat){
  zed <- x.mat %*% matrix(param.vec)
  return(1 / (1 + exp(-zed)))
}

# probably ok
minimize <- function(x.mat, y.vec, alpha, epsilon=0.000000000001){
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
    print(param.vec)
    # wait <- readline("continue? ")
    # cat("\n")
  }
}


get.gradient <- function(param.vec, x.mat, y.vec, lambda=0){
  x.mat <- cbind(1, x.mat)
  m <- nrow(x.mat)
  modtheta <- param.vec
  modtheta[1] <- 0
  the.hyp <- hypothesis.function(param.vec, x.mat)
  gradient <- (t(x.mat) %*% (the.hyp - y.vec) + lambda*modtheta) / m
  return(gradient)
}


# a ok
cost.function <- function(param.vec, x.mat, y.vec, lambda=0){
  x.mat <- cbind(1, x.mat)
  m <- nrow(x.mat)
  the.hyp <- hypothesis.function(param.vec, x.mat)
  cost <- (((t(-y.vec) %*% log(the.hyp)) - (t(1-y.vec) %*% log(1-the.hyp))) / m) +
          ((lambda / (2*m)) * sum(param.vec[2:length(param.vec)] ^ 2))
 
  return(cost)
}


do.plot <- function(a.data.frame, chosen.params){
  a.data.frame <- a.data.frame %>% mutate(yesorno=admitted==1)
  mintercept <- chosen.params[1] / -chosen.params[3]
  mslope <- chosen.params[2] / -chosen.params[3]
  ggplot(a.data.frame, aes(x=exam1, y=exam2, shape=yesorno, color=yesorno)) +
    geom_point() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title=element_blank(),
          axis.ticks=element_blank()) +
    geom_abline(intercept = mintercept, slope = mslope)
}



###########
# TESTING #
###########

training <- read.csv("./atester.csv", stringsAsFactors=FALSE)

ress <- hypothesis.function(c(-529.59, 52.61, 52.50),
                    cbind(1, as.matrix(training[,c(1,2)])))

ress <- hypothesis.function(c(-1065.5, 105.6, 107.7),
                    cbind(1, as.matrix(training[,c(1,2)])))

ress <- hypothesis.function(c(-2371.638, 245.786, 261.464),
                    cbind(1, as.matrix(training[,c(1,2)])))

ress <- hypothesis.function(c(-40.591103, 4.268732, 4.340145),
                    cbind(1, as.matrix(training[,c(1,2)])))

ress <- round(ress)

sum(ress == training$admitted)



result <- optim(par=rep(0, ncol(training)),
                cost.function, x.mat=as.matrix(training[,c(1,2)]),
                y.vec=as.matrix(training[,3]), lambda=0)



print(minimize(as.matrix(training[,c(1,2)]),
               as.matrix(training[,c(3)]),
               alpha=200))

print(system.time(minimize(as.matrix(training[,c(1,2)]),
               as.matrix(training[,c(3)]),
               alpha=200)))

print(system.time(minimize.non.vec(as.matrix(training[,c(1,2)]),
               as.matrix(training[,c(3)]),
               alpha=200)))


##############################################
##############################################



# this takes a *really* long time to converge
training <- read.csv("./acceptance.csv", stringsAsFactors=FALSE,
                     colClasses="numeric")

mymodel <- glm(admitted ~ exam1 + exam2, data=training, family="binomial")


print(minimize(as.matrix(training[,c(1,2)]),
               as.matrix(training[,c(3)]),
               alpha=.004))

result <- optim(par=rep(0, ncol(training)),
                cost.function, 
                get.gradient,
                x.mat=as.matrix(training[,c(1,2)]),
                y.vec=as.matrix(training[,3]), lambda=0,
                method="BFGS")         # 89

ress <- hypothesis.function(result$par,
                    cbind(1, as.matrix(training[,c(1,2)])))
ress <- round(ress)
sum(ress == training$admitted)


do.plot(training, result$par)


print(system.time(minimize(as.matrix(training[,c(1,2)]),
               as.matrix(training[,c(3)]),
               alpha=.004)))

print(system.time(minimize.non.vec(as.matrix(training[,c(1,2)]),
               as.matrix(training[,c(3)]),
               alpha=.004)))

# 89 %
ress <- hypothesis.function(c(-19.4945780, 0.1644228, 0.1535174),
                    cbind(1, as.matrix(training[,c(1,2)])))
ress <- round(ress)
sum(ress == training$admitted)



# octave's solution
# and mine
# and R's
# 90 %     (smaller epsilon)
ress <- hypothesis.function(c(-25.161, 0.206, 0.201),
                    cbind(1, as.matrix(training[,c(1,2)])))
ress <- round(ress)
sum(ress == training$admitted)


####################################################


# this takes a *really* long time to converge
training <- read.csv("./second.csv", stringsAsFactors=FALSE,
                     colClasses="numeric")

mymodel <- glm(admitted ~ exam1 + exam2, data=training, family="binomial")


print(minimize(as.matrix(training[,c(1,2)]),
               as.matrix(training[,c(3)]),
               alpha=.004))



print(system.time(minimize(as.matrix(training[,c(1,2)]),
               as.matrix(training[,c(3)]),
               alpha=.004)))

print(system.time(minimize.non.vec(as.matrix(training[,c(1,2)]),
               as.matrix(training[,c(3)]),
               alpha=.004)))

# 89 %
ress <- hypothesis.function(c(-23.902, 0.196, 0.191),
                    cbind(1, as.matrix(training[,c(1,2)])))
ress <- round(ress)
sum(ress == training$admitted)

# 90 %     (smaller epsilon)
ress <- hypothesis.function(c(-25.161, 0.206, 0.201),
                    cbind(1, as.matrix(training[,c(1,2)])))
ress <- round(ress)
sum(ress == training$admitted)
