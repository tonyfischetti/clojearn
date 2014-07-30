#!/usr/bin/Rscript --vanilla


#
#
# optim (and gradient and cost) work
# but not regularized
#
#

rm(list=ls())
options(scipen=10)


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
  #gradient <- (t(x.mat) %*% (the.hyp - y.vec) + lambda*modtheta) / m
  gradient <- (t(x.mat) %*% (the.hyp - y.vec)) / m
  return(gradient)
}

# grad = (X'' * (hypothesis - y)) / m;


# a ok
cost.function <- function(param.vec, x.mat, y.vec, lambda=0){
  x.mat <- cbind(1, x.mat)
  m <- nrow(x.mat)
  the.hyp <- hypothesis.function(param.vec, x.mat)
#   cost <- ((t((-y.vec)) %*% log(the.hyp)) - (t(1-y.vec) %*% log(1-the.hyp)) / m +
#             ((lambda / (2 * m)) * sum(param.vec[2:length(param.vec)] ^ 2)))
  cost <- ( (t(-y.vec) %*% log(the.hyp)) - (t(1-y.vec) %*% log(1-the.hyp))) / m
 
  return(cost)
}

#J = (( (-y)' * log(hypothesis)) -((1-y)' * log(1-hypothesis))) / m;

get.gradient(c(0, 0, 0),
              as.matrix(training[,c(1,2)]),
              as.matrix(training[,3]))



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
